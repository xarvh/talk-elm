module Slides exposing (..)


import Html exposing (..)
import Html.App as App
import Markdown


-- markdownSlide MD
--     |> layout twoCol
--     |> animate fadeIn
--     |> transition slideIn

-- list of properties, including `Content`?


type Message
    = Next
    | Prev





type alias Slide =
    { content : Html Message
    }





md : String -> Slide
md markdown =
    { content = Markdown.toHtml [] markdown
    }













--
-- Elm Architecture
--
type alias Model =
    { currentSlide : Int
    }



init : List Slide -> (Model, Cmd Message)
init slides =
    let
        model = { currentSlide = 0 }
        cmd = Cmd.none
    in
        (model, cmd)



update : Message -> Model -> (Model, Cmd Message)
update message model =
    (model, Cmd.none)


view : Model -> Html Message
view model =
    div [] []



subscriptions =
    \m -> Sub.none




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

