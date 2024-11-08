# Hypatia - A ML-family Language Compiled to Lua
Hello stranger,


How did you end up here? Let me introduce myself, I am Hypatia, youngest child of the ML family. As it is tradition, my name was given to honour a [deceased mathematician](https://en.wikipedia.org/wiki/Hypatia). Who I am? Well isn't that the question we all ask ourselves? I am still young, I have dreams, but I have not found my purpose yet. I am similar to my siblings, Purescript and Elm, but I compile to Lua. I am walking in the footsteps of my ancestors, but I want to be different: I want to make games. So let me tell you about me.

### My Appearance
```
module Example

import Native(Number, Array, (/))
import Native.Love(setColor, circle, rectangle)
import Common.Array(map)
import Geometry.Vector2(Vector2)

// This is an algebraic datatype.
// A Primitive is either a Rectangle or a Circle.
// Datatypes can have type variables.
// So this type could become a Primitive (Vector2 Number), Primitive (Vector3 Number) and more.
type Primitive point = Rectangle point point | Circle point Radius

// You can give types a new name with an alias
alias Radius = Number

alias Primitive2d = Primitive (Vector2 Number)


fill (Circle (Vector2 x y) r) = circle "fill" x y r
fill (Rectangle (Vector2 x y) (Vector2 w h)) = rectangle "fill" x y w h

primitives : Array Primitive2d
primitives = [
        Rectangle (Vector2 150 100) (Vector2 100 400),
        Rectangle (Vector2 350 100) (Vector2 100 400),
        Rectangle (Vector2 250 250) (Vector2 100 100),
        Rectangle (Vector2 550 300) (Vector2 100 200),
        // Trailing commas are allowed
        Circle (Vector2 550 200) 100,
    ]

draw _ =
    let
        _ = setColor (70 / 255) (40 / 255) (250 / 255)
    in map fill primitives
```

People say, I look like my father, Haskell, but somehow I am a little different:

* No guards. Just use if expressions.
* No operator sections. In Haskell `(-2)` could be a section or a prefix minus. Just use anonymous functions like `(\x -> x - 2)` or partial applications like `((-) 3)`.
* No do-expressions, list comprehensions and arithmetic sequences. Instead plain functions like `andThen`, `map2` or `range`.
* No records. My father's Achilles heel... I am dreaming of a unified solution for first class modules, records and ad-hoc polymorphism when I am older.
* Different keywords. `type` instead of `data`, in the spirit of Elm. `alias` can be used for pattern- and type aliases.
* You can write `fun` instead of `\` to be a little less cryptic.
* A module declaration has no `where` block, it is only a single line. One module, one file.

It was nice to talk with you stranger.


xoxo<br>
Hypatia

## How to get started
Hypatia is a prototype of a functional language of the ML-family that compiles to lua.
People are writing game engines in functional languages, however functional scripting languages are rarely seen.
Hypatia could fill this gap and it could reuse parts of the existing lua ecosystem.

Install the game engine [love](https://love2d.org) and [GHC](https://www.haskell.org/).
Copy the `library` folder to `~/.hypatia/library` or inside one of the example folders.
Then run
```
cabal run hypatia -- compile examples/Spheres/Spheres.hyp
love build/lua
```

Head over to the folder "examples" to see how applications look like.
