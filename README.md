# The Koan Language
This is a simple programming language I'll be working on. The goal is to have fun language features that aren't very common (e.g. apl-esque single character function names)

## Syntax
### Operator precedence
| Operator(s)                          | L and R binding power     |
| ------------------------------------ | ------------------------- |
| `^`                                  | L: `11`, R: `12`          |
| `*`, `/`                             | L: `9`, R: `10`           |
| `+`, `-`                             | L: `7`, R: `8`            |
| `==` and `!=`                        | L: `5`, R: `6`            |
| `>`, `>=`, `<`, and `<=`             | L: `3`, R: `4`            |
| `\|\|` and `&&`                      | L: `1`, R: `2`            |

All of the above operators are binary, but there are also four unary operators. There's unary minus `-`, pitimes `○`, sqrt `√`, and logical not `!`, all unary operators have the same precedence and always have higher precedence than binary operators.

### Unicode
For the most part, every ascii operator has a unicode equivalent, `>=` may be writen as `≥`, `*` as `×` (note this is not ascii x), `<=` as `≤`, and `!=` as `≠`.

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

### Arrays
Arrays take the form `[p1, p2, p3]`, trailing commas are permitted. All of the arithmetic operators operate on both arrays and numbers. An example of this behavior is observed in this example:

```js
let x = [1, 2, 3, 4, 5];
print(x ^ 2); // Prints [1, 4, 9, 16, 25]
print(2 ^ x); // Prints [2, 4, 8, 16, 32]
```
