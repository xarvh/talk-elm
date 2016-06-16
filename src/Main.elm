import Slides exposing (md)



main = Slides.app

    Slides.defaultOptions

    [ md
        """
        ### 1 aaaaaaaaa
        loool lol

        ### Elm: making functional programming accessible ###

        lorem ipsum meeehh!

        """


    , md """

        # 2 Easy app setup

        ```bash
           $ elm-package install --yes elm-lang/html
        ```
        And you are ready to go

        """


    , md """

        # 3 Stuff

        stuffity stuff!

        """
    , md """

        # 4 Stuff

        stuffity stuff!

        """
    , md """

        # 5 Stuff

        stuffity stuff!

        """
    , md """

        # 6 Stuff

        stuffity stuff!

        """
    , md """

        # 7 Stuff

        stuffity stuff!

        """
    ]
