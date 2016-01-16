# new-wave
This is programmed in Ocaml with the Core library. It can be compiled with

    $corebuild -lib str Main.native

When run you will be presented with a prompt. It can't do much right now, but
here's what you *can* do:

## Integer arithmetic

"+, -, *, /" all work as expected, currently on 32-bit signed integers.
"~" is used for unary negation.

## Define words

A word is defined by
entering an unbound identifier, followed by a definition, and a terminating ";"

```
> square dup * ;
> 3 square
9
```

## Shuffle the stack

```
dup     ( a -- a a )
drop    ( a -- )
swap    ( a b - b a )
rot     ( a b c - b c a )
```

## Dynamic Arrays

(These are a bit useless as you can't really save an array at the moment - only save a word that defines a fresh one ...)

```
dynarray-new  ( capacity -- da )
length        ( da -- len )
set           ( da index data -- )
ref           ( da index -- data )
push          ( da data -- )
pop           ( da -- data )
```
