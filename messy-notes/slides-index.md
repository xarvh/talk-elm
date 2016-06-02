
* Easy setup
    - `elm-package install --yes elm-lang/html`
    - gives us `elm-package.json` and `elm-stuff/`

* Single-step build
    - Very simple "Hello World", no index.html needed
    - `elm-make --yes HelloWorld.elm`
    - produces directly an `index.html`

* From here, using custom .html is trivial
    - `elm-make --yes HelloWorld.elm --output HelloWorld.js`
    - (provide example of custom index.html using `Elm.main.fullscreen()`)

? One language for functionality AND content


* Friendly error messages

* Descriptive names
    elm-make, elm-package...

* Accessible docs (enforced documentation)

* Enforced semver
```
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


* Twitter: @xarvh




* A lot of functionality covered out of the box (websockets, graphics, http)
