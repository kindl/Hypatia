# Hypatia - An ML-family Language Compiled to Lua
Hello stranger,


How did you end up here? Let me introduce myself, I am Hypatia, youngest child of the ML-family. As is tradition, my name honors a [departed mathematician](https://en.wikipedia.org/wiki/Hypatia). Who am I? Well, isn't that the question we all ask ourselves? I am still young, I have dreams, but I have not found my purpose yet. I am similar to my siblings, PureScript and Elm, but I compile to Lua. I follow in the footsteps of my ancestors, but I want to be different: I want to make games. So, let me tell you about myself.

### My Appearance
```
module Example

import Native(Number, Array, (/))
import Native.Love(setColor, circle, rectangle)
import Common.Array(map)
import Geometry.Vector2(Vector2)

/*
This is an algebraic data type.
A Primitive is either a Rectangle or a Circle.
Data types can have type variables.
So this type could become a Primitive (Vector2 Number), Primitive (Vector3 Number) and more.
*/
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

People say I look like my father, Haskell, but I am a little different:

* One colon for type signatures `i : Number`.
* Types are inferred. You generally write fewer type signatures in Hypatia compared to Haskell. The compiler is written in a similar style. (Side note of the author: This style is not what I would normally use for Haskell.)
* Array literals. Trailing commas are allowed `[a,b,]`. You cannot destructure arrays with `(head:tail)`, use functions from `Common.Array` instead.
* No guards. Use if expressions instead.
* No operator sections. In Haskell `(-2)` could be a section or a unary minus. Use anonymous functions like `(\x -> x - 2)` or partial applications like `((-) 3)`.
* Positive and negative literals. The minus in `f -2` is part of the literal and does not need parentheses. Negating a variable still needs them: `f (-a)`.
* No do-expressions, list comprehensions, or arithmetic sequences. Instead plain functions like `andThen`, `map2` or `range`.
* No tuple syntax. Use records with an Idris-like syntax to give fields a name and a type. Alternatively use `type Tuple2 a b = Tuple2 a b` instead.
* Different keywords. `type` instead of `data`, in the spirit of Elm. `alias` can be used for pattern aliases and type aliases.
* A module declaration has no `where` block, it is only a single line. A module starts with `module SomeModuleName`, the file name has to match the module name.
* Interpolated strings with `$"Hello {name}!"`.
* No character literals. Use string literals with a single character instead.

It was nice to talk with you, stranger.


xoxo<br>
Hypatia

## How to get started
Hypatia is a prototype of a functional language in the ML-family that compiles to Lua.
People are writing game engines in functional languages. However, functional scripting languages are rarely seen.
Hypatia could fill this gap and reuse parts of the existing Lua ecosystem.

Install the game engine [love](https://love2d.org) and [GHC](https://www.haskell.org/).
Then run
```
cabal run hypatia -- compile --library=library examples/Spheres/Spheres.hyp
love build/lua
```

Head over to the `examples` folder to see example applications.
