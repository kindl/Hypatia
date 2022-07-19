# Hypatia - A ML-family Language Compiled to Lua
Hello stranger,


How did you end up here? Let me introduce myself, I am Hypatia, youngest child of the ML family. As it is tradition, my name was given to honour a [deceased mathematician](https://en.wikipedia.org/wiki/Hypatia). Who I am? Well isn't that the question we all ask ourselves? I am still young, I have dreams, but I have not found my purpose yet. I am similar to my siblings, Purescript and Elm, but I compile to Lua. I am walking in the footsteps of my ancestors, but I want to be different: I want to make games. So let me tell you about me.

### My Appearance
```
module Example

import Native(Numeral, Array)
import Native.Love(setColor, circle, rectangle)
import Common.Array(map)

# This is an algebraic datatype. A primitive is either a Rectangle or a Circle
type Primitive = Rectangle Point Point | Circle Point Radius

# Datatypes can have type variables
type Vector2 a = Vec2 a a

# Typing Vector2 Numeral would be tedious. Let's give it a new name
alias Point = Vector2 Numeral

alias Radius = Numeral

fill (Circle (Vec2 x y) r) = circle "fill" x y r
fill (Rectangle (Vec2 x y) (Vec2 w h)) = rectangle "fill" x y w h

primitives : Array Primitive
primitives = [
    Rectangle (Vec2 150 100) (Vec2 100 400),
    Rectangle (Vec2 350 100) (Vec2 100 400),
    Rectangle (Vec2 250 250) (Vec2 100 100),
    Rectangle (Vec2 550 300) (Vec2 100 200),
    Circle (Vec2 550 200) 100]

draw _ = let { _ = setColor 70 40 250 } in map fill primitives
```

People say, I look like my father, Haskell, but somehow I am a little different:

* No guards. I want to be free! Just use if expressions.
* No operator sections. In Haskell `(-2)` could be a section or a prefix minus. Just use anonymous functions for partial application.
* No do-expressions, list comprehensions and arithmetic sequences. When I grow up I want to use plain functions like `liftM2` or `enumFrom`.
* No records. My father's Achilles heel... I am dreaming of a unified solution for first class modules, records and ad-hoc polymorphism when I am older.
* Different keywords. `type` instead of `data`, in the spirit of Elm. `alias` instead of `type`, because it can be used for pattern- and type aliases and `fun` instead of `\` to be a little less cryptic.
* A module declaration has no `where` block, it is only a single line. One module, one file.

It was nice to talk with you stranger.


xoxo  
Hypatia

## How to get started
Hypatia is a prototype of a functional language of the ML-family that compiles to lua.
People are writing game engines in functional languages, however functional scripting languages are rarely seen.
Hypatia could fill this gap and it could reuse parts of the existing lua ecosystem.

Install the game engine [love](https://love2d.org) and [GHC](https://www.haskell.org/) then run
```
cd path/of/hypatia
cabal install hypatia
cd examples
hypatia compile Spheres
love lua
```

Head over to the folder "examples" to see how applications look like.
