# Hypatia - A ML-family Language Compiled to Lua
Hello stranger,


How did you end up here? Let me introduce myself, I am Hypatia, youngest child of the ML family. As it is tradition, my name was given to honour a [dead mathematician](https://en.wikipedia.org/wiki/Hypatia). Who I am? Well isn't that the question we all ask ourselves? I am still young, I have dreams, but I have not found my purpose yet. I am similar to my siblings, Purescript and Elm, but I compile to Lua. I am walking in the footsteps of my ancestors, but I want to be different: I want to make games. So let me tell you about me.

### My Appearance
```
module Example

# Define a vector with two constructors
# A vector has either two or three elements
type Vector a = Vec2 a a | Vec3 a a a

# A type without a constructor
type Void

# A shorthand for the native number type
alias Numeral = Native.Numeral

# Functions without implementation are imported from Native.lua
circle : Vector Numeral -> Numeral -> Void
setColor : Vector Numeral -> Void

draw _ = let { _ = setColor (Vec3 40 30 240) } in circle (Vec2 400 300) 200
```

People say, I look like my father, Haskell, but somehow I am a little different:

* No guards. I want to be free! Just use if expressions.
* No operator sections. In Haskell `(-2)` could be a section or a prefix minus. Just use anonymous functions for partial application.
* No do-expressions, list comprehensions and arithmetic sequences. When I grow up I want to use plain functions like `liftM2` or `enumFrom`.
* No records. My father's Achilles heel... I am dreaming of a unified solution for first class modules, records and ad-hoc polymorphism when I am older.
* Different keywords. `type` instead of `data`, in the spirit of Elm. `alias` instead of `type`, because it can be used for pattern- and type aliases and `fun` instead of `\` to be a little less cryptic.
* A module declaration has no `where` block, it is only a single line. One module, one file.

### My Stages
Let me tell you about my daily routine:

* Lexer: While I am dreaming the Lexer takes all the strings I have read and turns them into tokens.
* Parser: Shortly before I wake up, token seeds grow into a syntax tree in the Parser.
* Operators: Yawn! Time to wake up and I already have my first chores to do. Sigh... I shake out my blanket, rename my operator symbols to plain names and change their associativity and precedence.
* Simplifier: I can't think before I had my coffee. A sip from the Simplifier and my world looks much clearer.
* Aliases: Time to brush my teeth. The Aliases brushes out all the bacteria and rename complicated patterns and types to simpler ones.
* Typechecker: Now I start with the actual work and I check if my types are correct.
* Sorting: In the evening, my creativity awakes. I sort my let-bindings and compile my syntax tree into an imperative dynamic language.
* Compiler: At night, I write the result down in lua, so that I don't forget anything.
* ModuleSystem: Is it bedtime already? Again? Tomorrow will be a good day, good that I have everything planned in my ModuleSystem.

It was nice to talk with you stranger.


xoxo  
Hypatia

## How to get started
Hypatia is a prototype of a functional language of the ML-family that compiles to lua.
People are writing game engines in functional languages, however functional scripting languages are rarely seen.
Hypatia could fill this gap and it could reuse parts of the existing lua ecosystem.

Install the game engine [love](https://love2d.org) then run
```
cabal run compile "examples/Spheres.hyp"
love lua
```

Head over to the folder "examples" to see how applications look like.
