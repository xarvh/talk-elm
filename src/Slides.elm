module Slides exposing
    ( Message (Next, Prev)
    , Model
    , program
    , app

    , md
    )


import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.App as App
import Keyboard
import Markdown exposing (defaultOptions)
import String
import Task
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

keyCodesToMessage =
    [   { message = First
        , keyCodes = [110] -- Home
        }
    ,   { message = Last
        , keyCodes = [115] -- End
        }
    ,   { message = Next
        , keyCodes = [13, 32, 39, 76, 68] -- Enter, Spacebar, Arrow Right, l, d
        }
    ,   { message = Prev
        , keyCodes = [8, 37, 72, 65] -- Backspace, Arrow Left, h, a
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
    | WindowResizes Window.Size


type alias Slide =
    { content : Html Message
    }

type alias Model =
    { slides : Array Slide
    , currentSlideIndex : Int
    , scale : Float
    }



--
-- Markdown slide constructor
--



md : String -> Slide
md markdownContent =
    { content =
        section
            []
            [ Markdown.toHtmlWith markdownOptions [] (unindent markdownContent) ]
    }



--
-- Init
--
init : List Slide -> (Model, Cmd Message)
init slides =
    let
        model = Model (Array.fromList slides) 0 1.0
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









update : Message -> Model -> (Model, Cmd Message)
update message oldModel =
    let
        noCmd m =
            (m, Cmd.none)

        selectSlide newIndex =
            noCmd { oldModel | currentSlideIndex = clamp 0 (Array.length oldModel.slides - 1) newIndex }
    in
        case message of
            Noop -> noCmd oldModel
            First -> selectSlide 0
            Last -> selectSlide 99999
            Prev -> selectSlide <| oldModel.currentSlideIndex - 1
            Next -> selectSlide <| oldModel.currentSlideIndex + 1
            WindowResizes size -> noCmd <| windowResize oldModel size



--
-- View
--
currentSlide model =
    Maybe.withDefault (md "") <| Array.get model.currentSlideIndex model.slides


view : Model -> Html Message
view model =
    div
        [ class "slide center" ]
        [ div
            [ class "slides"
            , style
                [ ("width", toString slidePixelSize.width ++ "px")
                , ("height", toString slidePixelSize.height ++ "px")
                , ("transform", "translate(-50%, -50%) scale(" ++ toString model.scale ++ ")")
                ]
            ]
            [ (currentSlide model).content ]
        ]



--
-- Subscriptions
--
keyPressDispatcher keyCodeMap keyCode =
    case keyCodeMap of
        x :: xs -> if List.member keyCode x.keyCodes then x.message else keyPressDispatcher xs keyCode
        _ -> Noop


-- TODO: add touch nav
subscriptions model =
    Sub.batch
    -- TODO: switch to Keyboard.presses once https://github.com/elm-lang/keyboard/issues/3 is fixed
    [ Keyboard.ups (keyPressDispatcher keyCodesToMessage)
    , Window.resizes WindowResizes
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

