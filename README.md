# GoSea: Advanced IDE features for editing Go

**note: this software is not finished and is under active development**

Go-sea provides tree-sitter enhanced editing and refactoring
capabilities in Emacs, heavily utilizing tree-sitter.  The goal for
this project is to provide every feature a full-fledged IDE such as
GoLand would provide.  It is intended that you would use go-sea in
addition to a language server.  The language server would provide the
core functions of what would be expected from an IDE while go-sea
provides the missing pieces.  **If you have any ideas, definitely make
a feature requset.  I'd love to hear them**.

## Feature List

- Generate table test based on functions parameters and results.
- Move a function to another package, updating old references to use
  the updated package.
- Auto-implement interfaces for a type.
- Add a return parameter, adding the zero value for it to all returns
- Remove a return parameter, removing it from each return
- Apply De Morgans law to a boolean expression (ex. !A && B -> !(A || !B))
- Swap var and short var definitions
- Negate the if-else statement, swapping their bodies.
- Generate an else clause for the if statement
- Quickly jump to places

## Ideas for features

- Fold Go code and if result is one-line, show it after the fold (like
  GoLand folding)
- Generate types from JSON
- More small code refactorings
- Parse special document comments (like links)
- Introduce constant
- Advanced code searching

## Installation

This project is very much a work in progress and expect rough edges.  I
don't plan on uploading it to MELPA or ELPA until all these rough
edges are ironed out and I have tested it.  That being said, if you
want to try it out you can install it from source, adding the
go-sea.el file to you load path.  You need to have a version of Emacs
that supports tree-sitter (29) as well as `ag` installed.

I plan on providing much more detailed documentation with examples
before release.
