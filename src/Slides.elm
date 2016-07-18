module Slides exposing
    ( Message (..)
    , Model
    , program
    , app

    , Options
    , slidesDefaultOptions

    , md
    , mdFragments
    , html
    , htmlFragments
    )



import AnimationFrame
import Array exposing (Array)
import Ease
import Html exposing (Html, div, section)
import Html.Attributes exposing (class, style)
import Html.App as App
import Keyboard
import Markdown
import Mouse
import Navigation
import SmoothAnimator
import String
import StringUnindent
import Task
import Time
import Window



--
-- Model
--
type alias Model =
    { slides : Array Slide
    , options : Options

    , windowSize : Window.Size

    , isPaused : Bool

    , slideAnimation : SmoothAnimator.Model
    , fragmentAnimation : SmoothAnimator.Model
    }


type alias Options =
    { slidePixelSize : { height : Int, width : Int }
    , easingFunction :  Float -> Float
    , slideAttributes : SlideAttributes
    , fragmentAttributes : FragmentAttributes
    , animationDuration : Time.Time
    , keyCodesToMessage : List { message : Message, keyCodes : List Int }
    }


type alias Slide =
    { fragments : List (Html Message)
    }



--
-- Messages
--
type Message
    = Noop

    | First
    | Last
    | Next
    | Prev

    | AnimationTick Time.Time
    | PauseAnimation

    | WindowResizes Window.Size



--
-- API types
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

type alias SlideAttributes =
    SlideAnimation -> List (Html.Attribute Message)

type alias FragmentAttributes =
    Float -> List (Html.Attribute Message)



--
-- Defaults
--
slideAttributesScroll : SlideAttributes
slideAttributesScroll slideAnimation =
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


fragmentAttributesOpacity : FragmentAttributes
fragmentAttributesOpacity completion =
    [ style
        [ ("opacity", toString completion) ]
    ]


slidesDefaultOptions : Options
slidesDefaultOptions =
    { slidePixelSize =
        { height = 700
        , width = 960
        }

    , easingFunction =
        Ease.inOutCubic

    , animationDuration =
        500 * Time.millisecond

    , slideAttributes =
        slideAttributesScroll

    , fragmentAttributes =
        fragmentAttributesOpacity

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
        ,   { message = PauseAnimation
            , keyCodes = [80]
            }
        ]
    }



--
-- API: Markdown slide constructor
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

        htmlNodes =
            List.map (Markdown.toHtmlWith options [] << StringUnindent.unindent) markdownFragments
    in
        htmlFragments htmlNodes



--
-- API: Html slide constructor
--
scale : Model -> Float
scale model =
    min
        (toFloat model.windowSize.width / toFloat model.options.slidePixelSize.width)
        (toFloat model.windowSize.height / toFloat model.options.slidePixelSize.height)


html : Html Message -> Slide
html htmlNode =
    htmlFragments [htmlNode]


htmlFragments : List (Html Message) -> Slide
htmlFragments htmlNodes =
    { fragments = htmlNodes }



--
-- Helpers
--
locationToSlideIndex : Navigation.Location -> Maybe Int
locationToSlideIndex location =
    String.dropLeft 1 location.hash |> String.toInt |> Result.toMaybe


modelToHashUrl : Model -> String
modelToHashUrl model =
    "#" ++ toString model.slideAnimation.targetPosition


slideByIndex : Model -> Int -> Slide
slideByIndex model index =
    Maybe.withDefault (md "") <| Array.get index model.slides


slideDistance : Model -> Float
slideDistance model =
    toFloat model.slideAnimation.targetPosition - model.slideAnimation.currentPosition



--
-- Update
--
slideAnimatorUpdate : Model -> SmoothAnimator.Message -> (Model, Cmd Message)
slideAnimatorUpdate oldParentModel childMessage =
    let
        duration =
            oldParentModel.options.animationDuration

        maximumPosition =
            Array.length oldParentModel.slides - 1

        newChildModel =
            SmoothAnimator.update duration maximumPosition childMessage oldParentModel.slideAnimation

        newParentModel =
            { oldParentModel | slideAnimation = newChildModel }

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



fragmentAnimatorUpdate : Int -> Model -> SmoothAnimator.Message -> (Model, Cmd Message)
fragmentAnimatorUpdate maximumPosition oldParentModel message =
    let
        duration = oldParentModel.options.animationDuration

        newChildModel = SmoothAnimator.update duration maximumPosition message oldParentModel.fragmentAnimation
        newParentModel = { oldParentModel | fragmentAnimation = newChildModel }
    in
        (newParentModel, Cmd.none)



resetFragments : Float -> Model -> Model
resetFragments distance oldModel =
    let
        newPosition =
            if distance > 0
            then 0
            else List.length (slideByIndex oldModel oldModel.slideAnimation.targetPosition).fragments - 1
    in
        { oldModel | fragmentAnimation = SmoothAnimator.init newPosition }



update : Message -> Model -> (Model, Cmd Message)
update message oldModel =
    let
        noCmd m =
            (m, Cmd.none)

        maximumPosition =
            List.length (slideByIndex oldModel oldModel.slideAnimation.targetPosition).fragments - 1

        isAlreadyChangingSlides =
            slideDistance oldModel /= 0

        mixedUpdater isAboutToChangeSlides message =
            if isAlreadyChangingSlides || isAboutToChangeSlides
            then slideAnimatorUpdate oldModel message
            else fragmentAnimatorUpdate maximumPosition oldModel message

    in
        case message of
            Noop ->
                noCmd oldModel

            First ->
                mixedUpdater True SmoothAnimator.SelectFirst

            Last ->
                mixedUpdater True SmoothAnimator.SelectLast

            Prev ->
                mixedUpdater (oldModel.fragmentAnimation.targetPosition - 1 < 0) SmoothAnimator.SelectPrev

            Next ->
                mixedUpdater (oldModel.fragmentAnimation.targetPosition + 1 > maximumPosition) SmoothAnimator.SelectNext

            WindowResizes size ->
                noCmd <| { oldModel | windowSize = size }

            PauseAnimation ->
                noCmd { oldModel | isPaused = not oldModel.isPaused }

            AnimationTick deltaTime ->
                if oldModel.isPaused
                then noCmd oldModel
                else
                    let
                        distance =
                            slideDistance oldModel

                        (m, cmd) =
                            mixedUpdater False (SmoothAnimator.AnimationTick deltaTime)

                        newModel =
                            (if distance /= 0 then resetFragments distance else identity) m
                    in
                        (newModel, cmd)



urlUpdate : Navigation.Location -> Model -> (Model, Cmd Message)
urlUpdate location model =
    case locationToSlideIndex location of
        -- User entered an url we can't parse as index
        Nothing ->
            (model, Navigation.modifyUrl <| modelToHashUrl model)

        Just index ->
            slideAnimatorUpdate model <| SmoothAnimator.SelectExact index



--
-- Init
--
init : Options -> List Slide -> Navigation.Location -> (Model, Cmd Message)
init options slides location =
    let
        model0 =
            { slides = Array.fromList slides
            , options = options
            , windowSize = options.slidePixelSize
            , isPaused = False
            , slideAnimation = SmoothAnimator.init 0
            , fragmentAnimation = SmoothAnimator.init 0
            }

        (model, urlCmd) =
            urlUpdate location model0

        slidePosition0 =
            model.slideAnimation.targetPosition

        slideAnimation =
            SmoothAnimator.init slidePosition0

        cmdWindow =
            Task.perform (\_ -> Noop) WindowResizes Window.size
    in
        ({ model | slideAnimation = slideAnimation }, Cmd.batch [cmdWindow, urlCmd])



--
-- View
--
slideSection attributes fragments =
    section
        attributes
        [ div
            [ class "slide-content" ]
            fragments
        ]


fragmentsByPosition model index fragmentPosition =
    let
        slide =
            slideByIndex model index

        completionByIndex index =
            (clamp 0 1 <| 1 + fragmentPosition - toFloat index)

        styleFrag index frag =
            div
                [ class "fragment-content" ]
                [ div
                    (model.options.fragmentAttributes <| completionByIndex index)
                    [ frag ]
                ]

        styledFragments =
            List.indexedMap styleFrag slide.fragments

    in
        styledFragments


slideViewMotion model =
    let
        distance =
            slideDistance model

        easing =
            if abs distance > 1 then identity
            else if distance >= 0
                then model.options.easingFunction
                else Ease.flip model.options.easingFunction

        smallerIndex =
            -- HACK: When Prev has *just* been called, currentPosition will be
            -- an integer and floor() will return it verbatim.
            floor <| model.slideAnimation.currentPosition - (if distance > 0 then 0 else 0.000001)

        largerIndex =
            smallerIndex + 1

        -- directions for the slide with the smaller index and the slide with the larger index
        (smallerDirection, largerDirection) =
            if distance > 0 then (Outgoing, Incoming) else (Incoming, Outgoing)

        completion =
            easing <| model.slideAnimation.currentPosition - toFloat smallerIndex

        slideAttributes =
            model.options.slideAttributes

    in
        [ slideSection (slideAttributes <| Moving smallerDirection SmallerIndex completion) (fragmentsByPosition model smallerIndex 9999)
        , slideSection (slideAttributes <| Moving largerDirection LargerIndex completion) (fragmentsByPosition model largerIndex 0)
        ]


slideViewStill model =
    [   slideSection (model.options.slideAttributes Still)
        <| fragmentsByPosition model model.slideAnimation.targetPosition model.fragmentAnimation.currentPosition
    ]



view : Model -> Html Message
view model =
    div
        [ class "slides"
        , style
            [ ("width", toString model.options.slidePixelSize.width ++ "px")
            , ("height", toString model.options.slidePixelSize.height ++ "px")
            , ("transform", "translate(-50%, -50%) scale(" ++ toString (scale model) ++ ")")

            , ("left", "50%")
            , ("top", "50%")
            , ("bottom", "auto")
            , ("right", "auto")
            , ("position", "absolute")
            , ("overflow", "hidden")
            ]
        ]
        <| (if slideDistance model == 0 then slideViewStill else slideViewMotion) model


--
-- Subscriptions
--
keyPressDispatcher keyCodeMap keyCode =
    case keyCodeMap of
        x :: xs -> if List.member keyCode x.keyCodes then x.message else keyPressDispatcher xs keyCode
        _ -> Noop --let x = Debug.log "keyCode" keyCode in Noop


mouseClickDispatcher : Model -> Mouse.Position -> Message
mouseClickDispatcher model position =
    let
        s = scale model

        slide component = s * toFloat (component model.options.slidePixelSize)

        window component = toFloat <| component model.windowSize

        h = slide .height
        w = slide .width

        hh = window .height
        ww = window .width

        {-
            Equation of the straigh line that passes through the slide's bottom left corner and top right corner.

            +------------>
            |       /
            |   +--/
            |   | /|
            |   |/ |
            |   /--+
            v  /
        -}
        y x = ( -x/w + (ww-w)/(2*w) + (hh+h)/(2*h) ) * h

        isAbove = toFloat position.y < y (toFloat position.x)
    in
        if isAbove
        then Prev
        else Next


-- TODO: Add support for touch/swipe
subscriptions model =
    Sub.batch
    -- TODO: switch to Keyboard.presses once https://github.com/elm-lang/keyboard/issues/3 is fixed
    [ Keyboard.ups (keyPressDispatcher model.options.keyCodesToMessage)
    , Mouse.clicks <| mouseClickDispatcher model
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

