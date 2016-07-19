import Slides exposing (md, mdFragments, slidesDefaultOptions)


main = Slides.app

    slidesDefaultOptions

    [ md
        """
        # Elm

        _Or: making functional programming accessible_

        by Francesco Orsenigo @xarvh

        """


    , mdFragments
        [   """
            I love the concepts behind functional programming.
            """

        ,   """
            So I tried to learn Haskell and Clojure.
            """

        ,   """
            They are great languages, but I struggled every time
            I wanted to go past a tutorial.
            """
        ]


    , md
        """
        I tried Elm and I was happily reimplementing my own projects
        in Elm shortly afterwards.
        """


    , md
        """
        How can we improve other FP languages?

        What is Elm doing right?
        """


    , md
        """

        # Easy app setup

        ```elm
            $ elm-package install elm-lang/html
        ```

        And we're ready to go!

        """


    , md """

        # Single-step build

        ```elm
        $ elm-make HelloWorld.elm
        ```

        <span class='arrow'>➡</span>

        ```elm
        index.html
        ```
        """


    , md """
        # Easy custom entry point

        ```elm
        $ elm-make HelloWorld.elm --output HelloWorld.js
        ```

        ```xml
        <-- index.html -->
        <head>
            <script type="text/javascript" src="HelloWorld.js"></script>
        </head>
        <body>
            <script type="text/javascript">Elm.Main.fullscreen()</script>
        </body>
        ```
        """


    , md
        """
        # Friendly, descriptive error messages

        ```
        -- ALIAS PROBLEM ------------------------------------------------------ Tree.elm

        This type alias is recursive, forming an infinite type!

        2|>type alias Branch =
        3|>    { x : Int
        4|>    , y : Int
        5|>    , name : String
        6|>    , children : List Branch
        7|>    }

        When I expand a recursive type alias, it just keeps getting bigger and bigger.
        So dealiasing results in an infinitely large type! Try this instead:

            type Branch
                = Branch { x : Int, y : Int, name : String, children : List Branch }

        This is kind of a subtle distinction. I suggested the naive fix, but you can
        often do something a bit nicer. So I would recommend reading more at:
        <https://github.com/elm-lang/elm-compiler/blob/0.17.1/hints/recursive-alias.md>

        Detected errors in 1 module.
        ```
        """


    , md
        """
        # Descriptive names

            -- TODO --

        """


    , md
        """
        # Accessible docs

        Inline docs are displayed on http://package.elm-lang.org/

        elm-package will refuse to publish anything without docs!
        """


    , md
        """
        # Official libraries for most common tasks

            * Virtual DOM
            * SVG rendering
            * Markdown
            * Http
            * Geolocation
            * Websockets
        """


    , md
        """
        # Enforced semver

        -- TODO --

        """

--         ```elm
--         $ elm-package diff evancz/elm-html 1.0.0 2.0.0
--         Comparing evancz/elm-html 1.0.0 to 2.0.0...
--         This is a MAJOR change.
-- 
--         ------ Changes to module Html.Attributes - MAJOR ------
-- 
--             Added:
--                 attribute : String -> String -> Attribute
--                 classList : List (String, Bool) -> Attribute
--                 minlength : Int -> Attribute
-- 
--             Changed:
--               - colspan : String -> Attribute
--               + colspan : Int -> Attribute
-- 
--               - rowspan : String -> Attribute
--               + rowspan : Int -> Attribute
--         ```
--         """


    , md
        """

        @xarvh

        """
    ]
