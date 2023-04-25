# Lumina

## Description

Lumina is a functional programming language written for an extremely cursed compiler assignment. It's similar to Standard ML, except the syntax is a bit different. For example, here's how you would calculate Fibonacci numbers:

```
(* Calculate Fibonacci numbers! *)
with fun fib (x: int) : int = with x case
    | 0 -> 0
    | 1 -> 1
    | _ -> fib (x - 1) + fib (x - 2)
end do fib 5 end
```

## Running

This is a Haskell project. To compile and run, `stack run`.

When you run the project, you will be presented with a list of options. Type in the number of the option you want to choose and press return.

## Specification

Honestly I was going to write a spec here but my language is constantly changing as I figure out what kind of grammar I want and how its operational semantics are going to work. I've clustered most of the grammar stuff in `src/Lumina/Frontend/LuminaGrammar.hs`.