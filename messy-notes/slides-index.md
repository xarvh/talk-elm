
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

* Accessible docs (enforced documentation)

* Enforced semver

* A lot of functionality covered out of the box (websockets, graphics, http)
