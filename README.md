# GoSea: Advanced IDE features for editing Go

**note: this software is not finished and is under active development**

Go-sea provides tree-sitter enhanced editing and refactoring
capabilities in Emacs, heavily utilizing tree-sitter.  The goal for
this project is to provide every feature a full-fledged IDE such as
GoLand would provide.  It is intended that you would use go-sea in
addition to a language server.  The language server would provide the
core functions of what would be expected from an IDE while go-sea
provides extra refactorings.  **If you have any ideas, definitely make
a feature request.  I'd love to hear them**.

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

## Demo

### go-sea-refactor-move

![Move function to another file](./docs/go-sea-refactor-move.gif)

Move a definition to another package, updating references.

### go-sea-insert-error

![Insert an error](./docs/go-sea-insert-error.gif)

Insert an "if error != nil, return the error" case, using the zero
value of the function.

### go-sea-toggle-var-declaration

![Toggle var declaration types](./docs/go-sea-toggle-var-declaration.gif)

Switch between using ":=" and "var" to define a variable.

### go-sea-add-return-type

![Add return type](./docs/go-sea-add-return-type.gif)

Add a return type for the current function, updating all nested return
statements.

### go-sea-remove-return-type

![Remove last return type](./docs/go-sea-remove-return-type.gif)

Remove the last return item, updating all return statements.

### go-sea-toggle-error-return

![Toggle function error return](./docs/go-sea-toggle-error-return.gif)

Toggle the returning of a functions error.

### go-sea-flip-if

![Flip if statement](./docs/go-sea-flip-if.gif)

Flip the if statement, inverting the condition and swapping blocks.

### go-sea-add-else

![Automatically add else statement](./docs/go-sea-add-else.gif)

Add else if clause.

### go-sea-implement-interface

![Implement interface](./docs/go-sea-implement-interface.gif)

Add implementation stubs for interface.

### go-sea-demorgans-law

![distribute not across and/or](./docs/go-sea-demorgans-law.gif)

Toggle between forms `!(A || B)` and `(!A && !B)` or `!(A && B)` and `(!A || !B)`.

### go-sea-generate-test

![Generate test based off of current test.](./docs/go-sea-generate-test.gif)

Generate a unit test based off of the current functions signature.

### go-sea-jump-to-result

![Jump to function result](./docs/go-sea-jump-to-result.gif)

Jump to functions return type.

### go-sea-jump-to-test

![Find and jump to a functions unit test](./docs/go-sea-jump-to-test.gif)

Find and jump to a functions unit test.

### go-sea-fold-at-line

![Fold code at line](./docs/go-sea-fold-at-line.gif)

Fold code at line. If `go-sea-fold-abbrev` is non-nil, the text on the
fold will show a symbol for each line of code: `?` if statement, `=`
assignment statement, `???` call expression, `???` for statement, `???`
return statement, `v` var declaration, and `???` switch statement.

### go-sea-fold-level

![Fold code to level](./docs/go-sea-fold-level.gif)

Fold entire buffer to specific level.

## Installation

This project is very much a work in progress and expect rough edges.
I don't plan on uploading it to MELPA or ELPA until all these rough
edges are ironed out and I have tested it personally thoroughly.  That
being said, if you want to try it out you can install it from source,
adding the this repository to you load path and running `(require 'go-sea)`.
You need to have a version of Emacs that supports tree-sitter (29) as well as
`ag` or `rg` installed.  If you are using `rg` you should set the value
of `go-sea-search-engine` to `'rg`.

Also there are no default keybinding so you will need to add these yourself.

I plan on providing much more detailed documentation with examples
before release.

## Ideas for features

- Generate types from JSON
- More small code refactorings
- Parse special document comments (like links)
- Introduce constant
- Advanced code searching
