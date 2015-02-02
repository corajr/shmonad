# Shmonad

Type-safe shell scripting!

Write your scripts in a Haskell EDSL, then compile them to regular
Bourne shell scripts that can run on a wide variety of machines.

## Motivation

Shell scripting can be rather fragile: everything is a string,
and the onus is on the scripter to use best practices.

Using a general purpose language like Python or Haskell is more robust,
but it requires that the target has a particular interpreter or the
right processor architecture for a compiled executable.

Shmonad provides an alternative to both: write your shell script using do
notation, then output those instructions as an `sh` script.

## Usage

Not nearly ready for use yet. Docs and a lot of features are still a big
TODO.

## Credit to...

A [Stack Overflow answer](http://stackoverflow.com/a/14084654/3827808) by
Gabriel Gonzalez introduced me to the method of creating such a DSL using the
free monad. While it turned out to be not what the asker wanted, it was a very
edifying reply!

NB: The [shell-monad](http://hackage.haskell.org/package/shell-monad) package
does much the same, but I was hoping for something more type-safe. Nevertheless
it's been helpful to see an alternative implementation of a similar tool.
