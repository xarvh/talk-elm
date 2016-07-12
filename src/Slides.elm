module Slides exposing
    ( Message (Next, Prev)
    , Model
    , program
    , app

    , Options
    , slidesDefaultOptions

    , md
    , mdFragments
    , html
    )



import AnimationFrame
import Array exposing (Array)
import Ease
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.App as App
import Keyboard
import Markdown
import Mouse
import Navigation
import SmoothAnimator
import String
import Task
import Time
import Window



slidesDefaultOptions : Options
slidesDefaultOptions =
    { slidePixelSize =
        { height = 700
        , width = 960
        }

    , easingFunction =
        Ease.inOutCubic

    , singleSlideAnimationDuration =
        500 * Time.millisecond

    , keyCodesToMessage =
        [   { message = First
            , keyCodes = [36] -- Home
            }
        ,   { message = Last
            , keyCodes = [35] -- End
            }
        ,   { message = Next
            , keyCodes = [13, 32, 39, 76, 68] -- Enter, Spacebar, Arrow Right, l, d
            }
        ,   { message = Prev
            , keyCodes = [37, 72, 65] -- Arrow Left, h, a
            }
        ,   { message = Pause
            , keyCodes = [80]
            }
        ]
    }



--
-- Helpers
--
unindent multilineString =
    let
        lines =
            String.lines multilineString

        countLeadingSpaces line =
            case String.uncons line of
                Nothing -> 0
                Just (char, xs) ->
                    case char of
                        ' ' -> 1 + countLeadingSpaces xs
                        _ -> 0

        minLead =
            lines
            |> List.filter (String.any ((/=) ' '))
            |> List.map countLeadingSpaces
            |> List.minimum
            |> Maybe.withDefault 0

    in
        lines
        |> List.map (String.dropLeft minLead)
        |> String.join "\n"



--
-- Model
--
type alias Options =
    { slidePixelSize : { height : Int, width : Int }
    , easingFunction :  Float -> Float
    , singleSlideAnimationDuration : Time.Time
    , keyCodesToMessage : List { message : Message, keyCodes : List Int }
    }


type alias Slide =
    { fragments : List (Html Message)
    }


type alias Model =
    { slides : Array Slide
    , options : Options

    , scale : Float

    , pause : Bool

    , slideAnimation : SmoothAnimator.Model
    , fragmentAnimation : SmoothAnimator.Model
    }



--
-- Markdown slide constructor
--
md : String -> Slide
md markdownContent =
    mdFragments [markdownContent]

mdFragments : List String -> Slide
mdFragments markdownFragments =
    let
        markdownDefaultOptions =
            Markdown.defaultOptions

        options =
            { markdownDefaultOptions
            | githubFlavored = Just { tables = True, breaks = False }
            , defaultHighlighting = Nothing
            , smartypants = True
            }

        fragments =
            List.map (Markdown.toHtmlWith options [] << unindent) markdownFragments
    in
        { fragments = fragments }






--
-- Html slide constructor
--
html : Html Message -> Slide
html htmlNodes =
    { fragments = [htmlNodes] }



--
-- Init, Update
--
windowResize : Model -> Window.Size -> Model
windowResize m size =
    let
        scale = min
            (toFloat size.width / toFloat m.options.slidePixelSize.width)
            (toFloat size.height / toFloat m.options.slidePixelSize.height)
    in
        { m | scale = scale }


locationToSlideIndex : Navigation.Location -> Maybe Int
locationToSlideIndex location =
    String.dropLeft 1 location.hash |> String.toInt |> Result.toMaybe


modelToHashUrl : Model -> String
modelToHashUrl model =
    "#" ++ toString model.slideAnimation.targetPosition



type Message
    = Noop

    | First
    | Last
    | Next
    | Prev

    | AnimationTick Time.Time
    | Pause

    | WindowResizes Window.Size


init : Options -> List Slide -> Navigation.Location -> (Model, Cmd Message)
init options slides location =
    let
        model0 =
            { slides = (Array.fromList slides)
            , options = options
            , scale = 1.0
            , pause = False
            , slideAnimation = SmoothAnimator.Model 0 0 0.0
            , fragmentAnimation = SmoothAnimator.Model 0 0 0.0
            }

        (model, urlCmd) =
            urlUpdate location model0

        slidePosition0 =
            model.slideAnimation.targetPosition

        slideAnimation =
            SmoothAnimator.Model slidePosition0 slidePosition0 (toFloat slidePosition0)

        cmdWindow =
            Task.perform (\_ -> Noop) WindowResizes Window.size
    in
        ({ model | slideAnimation = slideAnimation }, Cmd.batch [cmdWindow, urlCmd])



subUpdate : Model -> SmoothAnimator.Message -> (Model, Cmd Message)
subUpdate oldParentModel childMessage =
    let
        duration = oldParentModel.options.singleSlideAnimationDuration
        maximumPosition = Array.length oldParentModel.slides - 1
        newChildModel = SmoothAnimator.update duration maximumPosition childMessage oldParentModel.slideAnimation

        newParentModel = { oldParentModel | slideAnimation = newChildModel }

        currentIndexInUrl =
            case childMessage of

                -- user entered a new url manually, which may be out of bounds
                SmoothAnimator.SelectExact indexFromUrl -> indexFromUrl

                -- url reflects old index
                _ -> oldParentModel.slideAnimation.targetPosition

        cmd =
            if newChildModel.targetPosition == currentIndexInUrl
            then Cmd.none
            else Navigation.newUrl <| modelToHashUrl newParentModel
    in
        (newParentModel, cmd)



update : Message -> Model -> (Model, Cmd Message)
update message oldModel =
    let
        noCmd m = (m, Cmd.none)
        sub = subUpdate oldModel
    in
        case message of
            Noop -> noCmd oldModel

            First -> sub SmoothAnimator.SelectFirst
            Last -> sub SmoothAnimator.SelectLast
            Prev -> sub SmoothAnimator.SelectPrev
            Next -> sub SmoothAnimator.SelectNext

            WindowResizes size ->
                noCmd <| windowResize oldModel size

            AnimationTick deltaTime ->
                if oldModel.pause
                then noCmd oldModel
                else sub <| SmoothAnimator.AnimationTick deltaTime

            Pause ->
                noCmd { oldModel | pause = not oldModel.pause }


urlUpdate : Navigation.Location -> Model -> (Model, Cmd Message)
urlUpdate location model =
    case locationToSlideIndex location of
        -- User entered an url we can't parse as index
        Nothing ->
            (model, Navigation.modifyUrl <| modelToHashUrl model)

        Just index ->
            subUpdate model <| SmoothAnimator.SelectExact index



--
-- View
--
type SlideMotionDirection
    = Incoming
    | Outgoing

type SlideRelativeOrder
    = SmallerIndex
    | LargerIndex

type SlideAnimation
    = Still
    | Moving SlideMotionDirection SlideRelativeOrder Float


-- TODO: this should be customizable
slideStyle : SlideAnimation -> List (Html.Attribute Message)
slideStyle slideAnimation =
    let
        position =
            case slideAnimation of
                Still -> 0
                Moving direction order completion ->
                    let
                        offset = case order of
                            SmallerIndex -> 0
                            LargerIndex -> 100
                    in
                        offset - completion * 100
    in
        [ style
            [ ("position", "absolute")
            , ("width", "100%")
            , ("transform", "translate(" ++ toString position ++ "%)")
            ]
        ]


fragmentStyle : Float -> List (Html.Attribute Message)
fragmentStyle completion =
    [ style
        [ ("opacity", toString completion) ]
    ]


slideSection attributes fragments =
    section
        attributes
        [ div
            [ class "slide-content" ]
            fragments
        ]


fragmentsByPosition model index fragmentPosition =
    let
        emptySlide =
            md ""

        slide =
            Maybe.withDefault emptySlide <| Array.get index model.slides

        completionByIndex index =
            (clamp 0 1 <| 1 + fragmentPosition - toFloat index)

        styleFrag index frag =
            div
                [ class "fragment-content" ]
                [ div
                    (fragmentStyle <| completionByIndex index)
                    [ frag ]
                ]

        styledFragments =
            List.indexedMap styleFrag slide.fragments

    in
        styledFragments



slideViewMotion model =
    let
        distance =
            toFloat model.slideAnimation.targetPosition - model.slideAnimation.currentPosition

        easing =
            if abs distance > 1 then identity
            else if distance >= 0
                then model.options.easingFunction
                else Ease.flip model.options.easingFunction

        smallerIndex =
            floor model.slideAnimation.currentPosition

        largerIndex =
            smallerIndex + 1

        completion =
            easing <| model.slideAnimation.currentPosition - toFloat smallerIndex

        -- directions for the slide with the smaller index and the slide with the larger index
        (smallerDirection, largerDirection) =
            if distance > 0 then (Outgoing, Incoming) else (Incoming, Outgoing)

    in
        [ slideSection (slideStyle <| Moving smallerDirection SmallerIndex completion) (fragmentsByPosition model smallerIndex 9999)
        , slideSection (slideStyle <| Moving largerDirection LargerIndex completion) (fragmentsByPosition model largerIndex 0)
        ]



slideViewStill model =
    let
        frags =
            fragmentsByPosition model model.slideAnimation.targetPosition model.fragmentAnimation.currentPosition

    in
        [ slideSection (slideStyle Still) frags ]


slideView model =
    let
        isStill = toFloat model.slideAnimation.targetPosition == model.slideAnimation.currentPosition
    in
        (if isStill then slideViewStill else slideViewMotion) model





view : Model -> Html Message
view model =
    div
        [ class "slides"
        , style
            [ ("width", toString model.options.slidePixelSize.width ++ "px")
            , ("height", toString model.options.slidePixelSize.height ++ "px")
            , ("transform", "translate(-50%, -50%) scale(" ++ toString model.scale ++ ")")

            , ("left", "50%")
            , ("top", "50%")
            , ("bottom", "auto")
            , ("right", "auto")
            , ("position", "absolute")
            , ("overflow", "hidden")
            ]
        ]
        (slideView model)



--
-- Subscriptions
--
keyPressDispatcher keyCodeMap keyCode =
    case keyCodeMap of
        x :: xs -> if List.member keyCode x.keyCodes then x.message else keyPressDispatcher xs keyCode
        _ -> Noop --let x = Debug.log "keyCode" keyCode in Noop


mouseClickDispatcher position =
    Next


-- TODO: add touch nav
subscriptions model =
    Sub.batch
    -- TODO: switch to Keyboard.presses once https://github.com/elm-lang/keyboard/issues/3 is fixed
    [ Keyboard.ups (keyPressDispatcher model.options.keyCodesToMessage)
    , Mouse.clicks mouseClickDispatcher
    , Window.resizes WindowResizes
    , AnimationFrame.diffs AnimationTick
    ]



--
-- `main` helper
--
program options slides =
    { init = init options slides
    , update = update
    , urlUpdate = urlUpdate
    , view = view
    , subscriptions = subscriptions
    }

app : Options -> List Slide -> Program Never
app options slides =
    Navigation.program (Navigation.makeParser identity) (program options slides)

