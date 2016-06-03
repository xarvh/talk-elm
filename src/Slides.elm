module Slides exposing
    ( Message (Next, Prev)
    , Model
    , init
    , update
    , view
    , subscriptions

    , md

    , program
    )


import Array exposing (Array)
import Html exposing (..)
import Html.App as App
import Keyboard
import Markdown exposing (defaultOptions)
import String


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
    | Next
    | Prev


type alias Slide =
    { content : Html Message
    }

type alias Model =
    { slides : Array Slide
    , currentSlideIndex : Int
    }



--
-- Markdown slide constructor
--

-- TODO: move this in model.config.markdown
markdownOptions =
    { defaultOptions
    | githubFlavored = Just { tables = True, breaks = False }
    , smartypants = True
    }


md : String -> Slide
md markdownContent =
    { content = Markdown.toHtmlWith markdownOptions [] (unindent markdownContent) }



--
-- Init
--
init : List Slide -> (Model, Cmd Message)
init slides =
    let
        model = Model (Array.fromList slides) 0
        cmd = Cmd.none
    in
        (model, cmd)



--
-- Update
--
update : Message -> Model -> (Model, Cmd Message)
update message oldModel =
    let
        noCmd m =
            (m, Cmd.none)

        selectSlide deltaIndex =
            noCmd { oldModel | currentSlideIndex = clamp 0 (Array.length oldModel.slides - 1) (oldModel.currentSlideIndex + deltaIndex) }
    in
        case message of
            Noop -> noCmd oldModel
            Prev -> selectSlide -1
            Next -> selectSlide 1



--
-- View
--
currentSlide model =
    Maybe.withDefault (md "") <| Array.get model.currentSlideIndex model.slides


view : Model -> Html Message
view model =
    div []
        [ (currentSlide model).content ]



--
-- Subscriptions
--
keyboardPressDispatcher keyCode =
    if List.member keyCode [13, 32, 39] -- Enter, Spacebar, Arrow Right
    then Next
    else
        if List.member keyCode [8, 37] -- Backspace, Arrow Left
        then Prev
        else Noop


subscriptions model =
    Sub.batch
    -- TODO: switch to Keyboard.presses once https://github.com/elm-lang/keyboard/issues/3 is fixed
    [ Keyboard.ups keyboardPressDispatcher
    ]


--
-- `main` helper
--
program : List Slide -> Program Never
program slides =
    App.program
        { init = init slides
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

