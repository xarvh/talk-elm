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
    1000 * Time.millisecond

-- This is used to multiply the velocity when the animation passes across more than one slide
bigLeapVelocityMultiplier =
    4

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


type alias Slide =
    { content : Html Message
    }


type alias Model =
    { slides : Array Slide
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
    { content =
        Markdown.toHtmlWith markdownOptions [] (unindent markdownContent)
    }



--
-- Init
--
init : List Slide -> (Model, Cmd Message)
init slides =
    let
        model =
            { slides = (Array.fromList slides)
            , scale = 1.0
            , pause = False
            , targetPosition = 0
            , currentPosition = 0.0
            }

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
                if d > 1 then bigLeapVelocityMultiplier * d
                else 1

            deltaPosition = deltaTime * direction * velocity absDistance / animationDuration

            newUnclampedPosition = m.currentPosition + deltaPosition

        in
            -- either min or max, depending on the direction we're going
            newUnclampedPosition `limitTo` toFloat m.targetPosition



selectSlide m unclampedTargetPosition =
    { m | targetPosition = clamp 0 (Array.length m.slides - 1) unclampedTargetPosition }



update : Message -> Model -> (Model, Cmd Message)
update message oldModel =
    let
        noCmd m = (m, Cmd.none)
        select = selectSlide oldModel
    in
        noCmd <| case message of
            Noop -> oldModel

            First -> select 0
            Last -> select 99999
            Prev -> select <| oldModel.targetPosition - 1
            Next -> select <| oldModel.targetPosition + 1

            WindowResizes size -> windowResize oldModel size

            AnimationTick deltaTime -> { oldModel | currentPosition = newPosition oldModel deltaTime }

            Pause -> { oldModel | pause = not oldModel.pause }


--
-- View
--
slideView model =
    let
        leftSlideIndex = floor model.currentPosition
        rightSlideIndex = leftSlideIndex + 1

        uneasedTranslation = model.currentPosition - toFloat leftSlideIndex
        easedTranslation =
            if (abs <| model.currentPosition - toFloat model.targetPosition) > 1
            then uneasedTranslation
            else easingFunction uneasedTranslation

        slideSection offset index =
            section
                [ style
                    [ ("position", "absolute")
                    , ("transform", "translate(" ++ toString (offset - easedTranslation * 100) ++ "%)")
                    ]
                ]
                [ (Maybe.withDefault (md "") <| Array.get index model.slides).content
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

--         , text <| toString model.currentPosition
        , text <| toString (toFloat model.targetPosition - model.currentPosition)
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

