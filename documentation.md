# Documentation

## AST

The AST contains more information than the spec requires.
This is intentional to ensure its usability even if we decide to implement more features.

## Typecheck

Todos:

- check variable redeclarations (in nested blocks)
- add missing constructor
- simplify lookupMethod call to only require class name
- position errors?

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
