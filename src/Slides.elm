module Slides exposing (..)


import Array exposing (Array)
import Html exposing (..)
import Html.App as App
import Markdown exposing (defaultOptions)
-- import Regex
import String



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




-- TODO: move this in model.config.markdown
markdownOptions =
    { defaultOptions
    | githubFlavored = Just { tables = True, breaks = False }
    , smartypants = True
    }



{-
    1. replace /^\r[\r\s]*/, ''
    2. replace /[\r\s]*$/, ''
    3. get min leading spaces
    4. remove leading spaces



-}


-- removeRegex re string =
--     Regex.replace Regex.All (Regex.regex re) (\_ -> "") string




removeCommonLeadingSpaces multilineString =
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




md : String -> Slide
md markdownContent =
    { content = Markdown.toHtmlWith markdownOptions [] (removeCommonLeadingSpaces markdownContent) }







--
-- Elm Architecture
--
type alias Model =
    { slides : Array Slide
    , currentSlideIndex : Int
    }



currentSlide model =
    Maybe.withDefault (md "") <| Array.get model.currentSlideIndex model.slides



init : List Slide -> (Model, Cmd Message)
init slides =
    let
        model = Model (Array.fromList slides) 0
        cmd = Cmd.none
    in
        (model, cmd)



update : Message -> Model -> (Model, Cmd Message)
update message model =
    (model, Cmd.none)


view : Model -> Html Message
view model =
    div []
        [ (currentSlide model).content ]



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

