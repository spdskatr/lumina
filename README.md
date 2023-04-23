# Lumina

## Description

Lumina is a functional programming language designed with each keyword having a single unicode character counterpart, so you can produce cool-looking minified code from it.

For example, the following code
```
(* Calculate Fibonacci numbers! *)
with fun fib (x: int) : int = with x case
    | 0 -> 0
    | 1 -> 1
    | _ -> fib (x - 1) + fib (x - 2)
end do fib 5 end
```
when minified, looks like
```
α λ f ( x : ◻ ) : ◻ = α x ↦ | 0 → 0 | 1 → 1 | _ → f ( x - 1 ) + f ( x - 2 ) ω β f 5 ω
```
the choices for character are quite arbitrary and I haven't fully decided on them yet.

## Running

This is a Haskell project. To compile and run, `stack run`.

When you run the project, you will be presented with a list of options. Type in the number of the option you want to choose and press return.

NOTE: The x86 Assembly conversion does not work on MkClosure instructions (Mona IR) yet. The demo above works though!

## Specification

Honestly I was going to write a spec here but my language is constantly changing as I figure out what kind of grammar I want and how its operational semantics are going to work. I've clustered most of the grammar stuff in `src/Lumina/Frontend/LuminaGrammar.hs`.