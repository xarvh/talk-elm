import Slides exposing (md, slidesDefaultOptions)


main = Slides.app

    slidesDefaultOptions

    [ md
        """
        # Elm

        _Or: making functional programming accessible_

        by Francesco Orsenigo @xarvh

        """


    , md """
        Elm is doing a few things right
        """


    , md """

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

        [➡]()

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


    , md """
        # Friendly, descriptive error messages

        TODO: example

        """


    , md """
        # Descriptive names

        """


    , md """
        # Accessible docs

        elm-package will refuse to publish enything without docs!
        """


    , md """
        # (Some) batteries included

            * Graphics
            * Http
            * Websockets

        """


    , md """
        # Enforced semver

        ```elm
        $ elm-package diff evancz/elm-html 1.0.0 2.0.0
        Comparing evancz/elm-html 1.0.0 to 2.0.0...
        This is a MAJOR change.

        ------ Changes to module Html.Attributes - MAJOR ------

            Added:
                attribute : String -> String -> Attribute
                classList : List (String, Bool) -> Attribute
                minlength : Int -> Attribute

            Changed:
              - colspan : String -> Attribute
              + colspan : Int -> Attribute

              - rowspan : String -> Attribute
              + rowspan : Int -> Attribute
        ```
        """


    , md """

        @xarvh

        """
    ]
