module Slides exposing
    ( Message (Next, Prev)
    , Model
    , program
    , app

    , Options
    , defaultOptions

    , md
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
import String
import Task
import Time
import Window


markdownDefaultOptions = Markdown.defaultOptions


defaultOptions : Options
defaultOptions =
    { markdown =
        { markdownDefaultOptions
        | githubFlavored = Just { tables = True, breaks = False }
        , smartypants = True
        }

    , slidePixelSize =
        { height = 700
        , width = 960
        }

    , easingFunction =
        Ease.outCubic

    , singleSlideAnimationDuration =
        1000 * Time.millisecond

    , multipleSlidesAnimationDuration =
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
    { markdown : Markdown.Options
    , slidePixelSize : { height : Int, width : Int }
    , easingFunction :  Float -> Float
    , singleSlideAnimationDuration : Time.Time
    , multipleSlidesAnimationDuration : Time.Time
    , keyCodesToMessage : List { message : Message, keyCodes : List Int }
    }


type alias Slide =
    { content : Options -> Html Message
    }


type alias Model =
    { slides : Array Slide
    , options : Options

    , scale : Float

    , pause : Bool
    , targetPosition : Int
    , currentPosition : Float
    }



--
-- Markdown slide constructor
--
md : String -> Slide
md markdownContent =
    { content = \options ->
        Markdown.toHtmlWith options.markdown [] (unindent markdownContent)
    }



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
    "#" ++ toString model.targetPosition


newPosition m deltaTime =
    if m.pause
    then m.currentPosition
    else
        let
            distance = toFloat m.targetPosition - m.currentPosition
            absDistance = abs distance

            (direction, limitTo) =
                if distance > 0
                then (1, min)
                else (-1, max)

            -- velocity is a funciton of the absolute distance
            velocity d =
                if d > 1 then d / m.options.multipleSlidesAnimationDuration
                else 1 / m.options.singleSlideAnimationDuration

            deltaPosition = deltaTime * direction * velocity absDistance

            newUnclampedPosition = m.currentPosition + deltaPosition

        in
            -- either min or max, depending on the direction we're going
            newUnclampedPosition `limitTo` toFloat m.targetPosition


clampSlideIndex : Model -> Int -> Int
clampSlideIndex model unclampedIndex =
    clamp 0 (Array.length model.slides - 1) unclampedIndex


selectSlide : Model -> Int -> (Model, Cmd Message)
selectSlide oldModel unclampedTargetPosition =
    let
        newTargetIndex = clampSlideIndex oldModel unclampedTargetPosition
        newModel = { oldModel | targetPosition = newTargetIndex }

        cmd =
            if newModel.targetPosition == oldModel.targetPosition
            then Cmd.none
            else Navigation.newUrl <| modelToHashUrl newModel
    in
        (newModel, cmd)


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
        (model, urlCmd) = urlUpdate location
            { slides = (Array.fromList slides)
            , options = options
            , scale = 1.0
            , pause = False
            , targetPosition = 0
            , currentPosition = 0.0
            }

        cmdWindow = Task.perform (\_ -> Noop) WindowResizes Window.size
    in
        (model, Cmd.batch [cmdWindow, urlCmd])


update : Message -> Model -> (Model, Cmd Message)
update message oldModel =
    let
        noCmd m = (m, Cmd.none)
        select = selectSlide oldModel
    in
        case message of
            Noop -> noCmd oldModel

            First -> select 0
            Last -> select 99999
            Prev -> select <| oldModel.targetPosition - 1
            Next -> select <| oldModel.targetPosition + 1

            WindowResizes size -> noCmd <| windowResize oldModel size

            AnimationTick deltaTime -> noCmd { oldModel | currentPosition = newPosition oldModel deltaTime }

            Pause -> noCmd { oldModel | pause = not oldModel.pause }


urlUpdate : Navigation.Location -> Model -> (Model, Cmd Message)
urlUpdate location model =
    case locationToSlideIndex location of
        -- User entered an invalid has url
        Nothing ->
            (model, Navigation.modifyUrl <| modelToHashUrl model)

        Just index ->
            let
                newIndex =
                    clampSlideIndex model index
                cmd =
                    if newIndex == index
                    then Cmd.none
                    else Navigation.newUrl <| modelToHashUrl model
            in
                ({ model | targetPosition = newIndex}, cmd)



--
-- View
--
slideView model =
    let
        distance =
            toFloat model.targetPosition - model.currentPosition

        easing d =
            if abs distance > 1 then d
            else if distance >= 0
                then model.options.easingFunction d
                else 1 - model.options.easingFunction (1 - d)

        leftSlideIndex = floor model.currentPosition
        rightSlideIndex = leftSlideIndex + 1
        traslation = easing <| model.currentPosition - toFloat leftSlideIndex

        slideSection offset index =
            section
                [ style
                    [ ("position", "absolute")
                    , ("transform", "translate(" ++ toString (offset - traslation * 100) ++ "%)")
                    ]
                ]
                [ (Maybe.withDefault (md "") <| Array.get index model.slides).content model.options
                ]
    in
        [ slideSection 0 leftSlideIndex
        , slideSection 100 rightSlideIndex
        ]


view : Model -> Html Message
view model =
    div
        [ class "slide"
        , style
            [   ("position", "relative")
            ,   ("width", "100%")
            ,   ("height", "100%")
            ,   ("overflow", "hidden")
            ]
        ]
        [ div
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
                ]
            ]
            (slideView model)

--         , text <| toString model.currentPosition
--         , text <| toString (toFloat model.targetPosition - model.currentPosition)
        ]



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

