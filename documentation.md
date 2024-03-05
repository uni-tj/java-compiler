# Documentation

What goes here:

- How to view each one's code seperately
- ideas of algorithms and what happens where
- how to run code/tests
- features
- per person ~1 page

## AST

The AST contains more information than the spec requires.
This is intentional to ensure its usability even if we decide to implement more features.

## Typecheck

Todos:

- check variable redeclarations (in nested blocks)
- position errors
- field override is subtype check
- test acceess modifier checks on override
- redo dead code checks to accomodate for if-else both returning
- resolveSuper function

Prechecked invariants that can later be relied on:

- valid inheritance graph
  - every extends exists
  - no inheritance cycles
- valid types
  - every user-written type is valid. If it refers to a class, the class exists.
  - Since all generated types are assumed to be correct, the type-related functions need not worry about class resolution failing and carrying positions around.

Features:

- dead code checks
- error messages in ExceptT monad
- constructor return value checks
- access control
  - global classes must not be protected or private
  - variable/method access
- overloading
  - no duplicate definitions
  - choose most specialized functions
  - no ambiguous references
- inheritance
  - overrides don't have stricter access
  - overrides have subtype as return type
  - @override labelled methods must be overrides
  - no inheritance cycle
  - inherited classes must exist
- inject return
  - only void methods
  - only if not definitely returning

Naming conventions:

- accessors are prefixed with first letter of constructor (`moverride` in `Method`)
- prefix t:
  - tagged (usually with position) (`tname` for a name with position. Types would be `(PositionTag i, Tag i Identifier) => i` or `WithPosition Identifier`)
  - type (`tthis`. Type would be `Type`)
- prefix m:
  - because of constructor
  - maybe (`mfound` for an value that may not be found. Type would be `Maybe ...`)

Invariants:

- AccCtx: name exists
