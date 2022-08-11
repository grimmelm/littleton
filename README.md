## Setup Instructions

Littleton is built mainly in OCaml. First, install dependencies using the
following commands:

```
opam switch 4.11.0
eval $(opam env)
opam install alcotest async cohttp cohttp-async core menhir mparser mparser-re \
             ocamlgraph re toml yojson \
             ppx_deriving_yojson ppx_deriving ppx_jane ppx_sexp_conv \
             js_of_ocaml js_of_ocaml-ppx
```

## Building the Interpreter and Interface

The `Makefile` includes instructions to build two versions of the Littleton
interpreter. First, a native command-line executable (created as `littleton`)
can be built using just `make`.

Secondly, the `Makefile` can generate a JavaScript version bundled with a
web-based interface to the interpreter. This version can be built with `make
website` (which also builds the native version). The website contents will be
placed in `_build/website`, which can be served using your server software of
choice. The website takes examples from the `examples/` directory. To add a new
set of examples, add a new `.json` file following the format of the ones
present, and add the filename to `examples/index.json`.

The native executable also includes a simple web server so that you don't
require any other software for the web interface. Run `make website` and run the
server with `misc/run-website.sh`. The web interface will then be available at
`localhost:8000`.

The design of the interface is partially inspired by the [Rust playground](https://play.rust-lang.org "Rust playground").

## Writing Examples

As noted above the web interface pulls examples in a JSON format from the
`examples/` directory. As a convenience, you can write examples in an example
TOML format and use Littleton to convert them to json. `./littleton jsonify
path/to/file.toml` will create an equivalent `path/to/file.json` that you can
put in `examples/` and add to `index.json`.
