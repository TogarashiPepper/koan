# The Koan Language
This is a simple programming language I'll be working on. The goal is to have fun language features that aren't very common (e.g. apl-esque single character function names)

## Syntax
### Operator precedence
| Operator(s)                | L and R binding power     |
| -------------------------- | ------------------------- |
| `+`, `-`                   | L: `3`, R: `4`            |
| `*`, `/`                   | L: `5`, R: `6`            |
| `==`, `>`, `>=`, `<`, `<=` | L: `1`, R: `2`            |

All of the above operators are binary, but there are also two unary operators. There's unary minus `-` and pitimes `○`, all unary operators have the same precedence and always have higher precedence than binary operators.

### Comparision
All of the comparision operators return `1` for `true` and `0` for `false`. This facilitates easier branchless programming and simplifies the language (if only marginally).

### Blocks
Blocks are delineated by `{` and `}`. They consist of a series of statements and may optionally end with an expression. If a block ends with an expression that value will be used as the return value for the block.

```js
{
  let x = 41;
  x + 1
}
```

Here the block evaluates to the value `42`. An empty block will evaluate to `nothing`. Entering a block also enters a new scope

### Let
Variable definitions take the form `let name = value;` and are local to the current scope. Variable definitions may also shadow definitions from outer scopes.

### Function calls
Function calls take the form `name(param1, param2, param3)`. The stdlib provides a function named `print`.
