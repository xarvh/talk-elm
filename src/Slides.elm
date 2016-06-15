module Slides exposing
    ( Message (Next, Prev)
    , Model
    , program
    , app

    , md
    )


import AnimationFrame
import Array exposing (Array)
import Ease
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.App as App
import Keyboard
import Markdown exposing (defaultOptions)
import Mouse
import String
import Task
import Time
import Window



-- TODO: move these in model.config or something
markdownOptions =
    { defaultOptions
    | githubFlavored = Just { tables = True, breaks = False }
    , smartypants = True
    }

slidePixelSize =
    { height = 700
    , width = 960
    }


easingFunction =
    Ease.inOutBounce


animationDuration =
    2000 * Time.millisecond

keyCodesToMessage =
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
        , keyCodes = [8, 37, 72, 65] -- Backspace, Arrow Left, h, a
        }
    ,   { message = Pause
        , keyCodes = [80]
        }
    ]



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
type Message
    = Noop

    | First
    | Last
    | Next
    | Prev

    | AnimationTick Time.Time

    | WindowResizes Window.Size

    | Pause


type AnimationStatus
    = Idle
    | Transitioning Int Float


type alias Slide =
    { content : Html Message
    }


type alias Model =
    { slides : Array Slide
    , currentSlideIndex : Int
    , animationStatus : AnimationStatus
    , scale : Float
    , pause : Bool
    }



--
-- Markdown slide constructor
--
md : String -> Slide
md markdownContent =
    { content =
        Markdown.toHtmlWith markdownOptions [] (unindent markdownContent)
    }



--
-- Init
--
init : List Slide -> (Model, Cmd Message)
init slides =
    let
        model = Model (Array.fromList slides) 0 Idle 1.0 False
        cmd = Task.perform (\_ -> Noop) WindowResizes Window.size
    in
        (model, cmd)



--
-- Update
--
windowResize m size =
    let
        scale = min
            (toFloat size.width / slidePixelSize.width)
            (toFloat size.height / slidePixelSize.height)
    in
        { m | scale = scale }


animate m deltaTime =
    if m.pause then m else
    case m.animationStatus of
        Idle -> m
        Transitioning endSlideIndex completion ->
            let
                newCompletion = completion + deltaTime / animationDuration
            in
                if newCompletion >= 1
                then { m | currentSlideIndex = endSlideIndex, animationStatus = Idle }
                else { m | animationStatus = Transitioning endSlideIndex newCompletion }


update : Message -> Model -> (Model, Cmd Message)
update message oldModel =
    let
        noCmd m =
            (m, Cmd.none)


        selectSlide newIndex =
            let
                newIndex = clamp 0 (Array.length oldModel.slides - 1) newIndex
                newModel = { oldModel | animationStatus = Transitioning newIndex 0 }
            in
                noCmd <|
                    if newIndex == oldModel.currentSlideIndex || oldModel.animationStatus /= Idle
                    then oldModel
                    else newModel

    in
        case message of
            Noop -> noCmd oldModel

            First -> selectSlide 0
            Last -> selectSlide 99999
            Prev -> selectSlide <| oldModel.currentSlideIndex - 1
            Next -> selectSlide <| oldModel.currentSlideIndex + 1

            WindowResizes size -> noCmd <| windowResize oldModel size

            AnimationTick deltaTime -> noCmd <| animate oldModel deltaTime

            Pause -> noCmd { oldModel | pause = not oldModel.pause }


--
-- View
--
slideView model =
    let
        slideSection completion direction offset index =
            section
                [ style
                    [ ("position", "absolute")
                    , ("transform", "translate(" ++ toString (offset + completion * direction * 100) ++ "%)")
                    ]
                ]
                [ (Maybe.withDefault (md "") <| Array.get index model.slides).content
                ]

    in case model.animationStatus of
        Idle ->
            [ slideSection 0 0 0 model.currentSlideIndex ]

        Transitioning newIndex completion ->
            let
                -- moving forward, slides will translate leftwards
                -- moving backwards, slides will translate rightwards
                direction = if newIndex > model.currentSlideIndex then -1 else 1
                newSlideStartingOffset = -100 * direction

                easedCompletion = easingFunction completion
            in
                [ slideSection easedCompletion direction 0 model.currentSlideIndex
                , slideSection easedCompletion direction newSlideStartingOffset newIndex
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
                [ ("width", toString slidePixelSize.width ++ "px")
                , ("height", toString slidePixelSize.height ++ "px")
                , ("transform", "translate(-50%, -50%) scale(" ++ toString model.scale ++ ")")

                , ("left", "50%")
                , ("top", "50%")
                , ("bottom", "auto")
                , ("right", "auto")
                , ("position", "absolute")
                ]
            ]
            (slideView model)

        , text <| case model.animationStatus of
            Idle -> "idle"
            Transitioning direction completion ->
                toString direction ++ "  " ++ toString completion
        ]



--
-- Subscriptions
--
keyPressDispatcher keyCodeMap keyCode =
    case keyCodeMap of
        x :: xs -> if List.member keyCode x.keyCodes then x.message else keyPressDispatcher xs keyCode
        _ -> let x = Debug.log "aa" keyCode in Noop


mouseClickDispatcher position =
    Next


-- TODO: add touch nav
subscriptions model =
    Sub.batch
    -- TODO: switch to Keyboard.presses once https://github.com/elm-lang/keyboard/issues/3 is fixed
    [ Keyboard.ups (keyPressDispatcher keyCodesToMessage)
    , Mouse.clicks mouseClickDispatcher
    , Window.resizes WindowResizes
    , AnimationFrame.diffs AnimationTick
    ]


--
-- `main` helper
--
program slides =
    { init = init slides
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

app : List Slide -> Program Never
app slides =
    App.program <| program slides

