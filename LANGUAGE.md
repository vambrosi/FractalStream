# FractalStream scripting language

FractalStream accepts both *expressions* and *scripts* in different
parts of the program. *Expressions* represent values of various types,
while *scripts* represent code that FractalStream should execute
step-by-step. For example, `z² + C` is an expression, but

```
z ⭠ z² + C
```

is a very simple script that tells FractalStream to update the value of
`z` to `z² + C`.

## Expressions

### Arithmetic expressions

The usual arithmetic operators `+`, `-`, `*` (multiplication), `/`, and `^` (exponentiation) are available for real and complex types. With the exception of
`/`, these are also available for integer types. For integer division,
use `//` instead.

### Comparisons

Values can be compared for equality with `=` (alternate spelling: `==`),
and for inequality with `!=` (alternate spellings `=/=` or `≠`). Real
and integer values can be compared with `<`, `>`, `<=` (alternate spelling
`≤`), and `>=` (alternate spelling `≥`).

### Boolean operations

The boolean operators `and`, `or`, and `not` can be used, with
`or` taking precedence over `and`, and `and` taking precedence over
`not`. That is, `not p and q or r` would be parsed as `((not p) and q) or r`.
This can be confusing, so as a general recommendation you should not
mix `and`s and `or`s without parentheses.

### Constants

FractalStream knows about the real constants `pi` and `e` (also spelled
`π` and `𝑒`), the imaginary unit `i` (also spelled `𝑖`), and
the boolean constants `true` and `false`. Numeric constants are
assumed to be integers if they don't include a decimal point, or
real numbers otherwise.

### Parentheses

Any of `(`/`)`, `[`/`]`, and `{`/`}` can be used to parenthesize subexpressions,
as in `𝑒^{π 𝑖 t}`.

### Functions

FractalStream knows a number of different functions.

Functions that can be used on both real and complex values are
`exp`, `log`, `sqrt`, `cos`, `sin`, `tan`, `tanh`, `cosh`, `sinh`, and `tanh`.

Functions that can be used only on real values are
`arccos`, `arcsin`, `arctan`, `arccosh`, `arcsinh`, and `arctanh`. Each of
these can also be spelled in its abbreviated form (e.g. `arccos` -> `acos`)

Functions that can be used only on complex values are
`re`, `im`, `arg`, and `conj`. These can also be spelled in capitalized form
(e.g. `re` -> `Re`)

### Syntax quirks

FractalStream supports some syntactical shortcuts common in mathematics.
If a function is applied to a simple argument, the application can be
written without parentheses. So instead of writing `cos(z)`, you could write
`cos z` instead. But `cos x + y` would mean `(cos x) + y`, not `cos (x + y)`.

FractalStream also supports implicit multiplication when arguments are
concatenated, so `x y` is the same as `x * y` and `2 cos z` is the same as
`2 * cos(z)`. To avoid ambiguity, FractalStream will not allow you to
write expressions like `cos 2z` since it could be interpreted as
`(cos 2) * z` or as `cos (2 * z)`.

### Colors

Values can also describe colors, not just plain scalar values.
FractalStream knows about a number of predefined colors:
`red`, `green`, `blue`, `black`, `white`, `grey` (`gray`),
`orange`, `yellow`, `purple`, and `violet`. You can lighten or darken a
color with the `light` and `dark` operators, so `dark blue` is a kind of
navy blue, and `light light red` is a shade of pink.

Colors can be inverted with the `invert` operator, so `invert green`
is a shade of purple.

Custom colors can be made with the `rgb(r,g,b)` operation, where
`r`, `g`, and `b` are values representing real numbers from `0` to `1`.
Two different colors can be blended by `blend(t, c1, c2)` where `t`
is a real number from `0` to `1` representing the percentage to take
from `c1`, with `(1 - t)` taken from `c2`.

## Script commands

### Variable assignment

To assign a value to a pre-existing variable, use the `<-` operator.

```
z <- z^2 + C
```

You can also use the unicode versions `⭠` or `←` (or more likely, let
the code editor automatically substitute these when you type `<-`)

### Variable initialization

To create a new variable in your script, use the `<-` operator but
provide a type annotation on the left-hand side.

```
k : Z ⭠ 0
```

Common types you might want to use include `Z` for the integers, `R` for the
real numbers, `C` for the complex numbers, `Color` for a color, and
`Boolean` for a truth value. You can also use the unicode blackboard bold
variants `ℤ`, `ℝ`, and `ℂ`, or the full names `Integer`, `Real`, and `Complex`.

### Loops

There are four looping constructs: `while...`, `until...`, `repeat...while`, and
`repeat...until`. The body of the loop is given by an indented block of statements:

```
while |z| < escape_radius and k < max_iters
  z ⭠ z^2 + C
  k ⭠ k + 1
```


The `until` forms are just like the `while` forms but with
the condition logically negated. So

```
while n < 10
  n ⭠ n + k
```

is the same as

```
until n ≥ 10
  n ⭠ n + k
```

The `repeat...` forms of the loop always executes the loop body at least once. After running this code:

```
x ⭠ 0
repeat
  x ⭠ x + 1
while 1 = 2
```

the value of `x` will be `1`. But after running this code:

```
x ⭠ 0
while 1 = 2
  x ⭠ x + 1
```

the value of `x` will still be `0`.

### Conditionals

You can conditionally execute code with `if/then` and `if/then/else` statements.

After running this code:

```
x ⭠ 0
if 1 = 2 then
  x ⭠ 1
```

`x` will still be `0`. But after this code:


```
x ⭠ 0
if 1 = 2 then
  x ⭠ 1
else
  x ⭠ 2
```

`x` will be `2`. You can chain multiple conditions together with `else if`s,
like so:

```
x ⭠ 0
if 1 = 2 then
  x ⭠ 1
else if 3 = 4 then
  x ⭠ 2
else
  x ⭠ 3
```

### Output a value from a script

Some scripts will need to produce one or more values as a result. For
example, scripts that are used to draw pictures must output a value
called `color` to determine which color to draw. This is done using
an `output` command:

```
output black to color
```

Putting everything together so far, we can make a basic script for
drawing the Douady rabbit. This script is meant to be run in a context
where there is an input variable `z` of complex type, and an output
variable `color` of color type.

```
c : ℂ ⭠ -0.12256 + 0.74486 𝑖
k : ℤ ⭠ 0

while |z| < 10 and k < 100
  z ⭠ z^2 + c
  k ⭠ k + 1

if k = 100 then
  output black to color
else
  output white to color
```

### Drawing commands

In some contexts, a script might have the ability to draw annotations
on a viewer. For example, custom-defined tools can run scripts in
response to user input, and mark on the view in response.

All points that appear in drawing commands are in the coordinates of the
model; you don't have to worry about converting from model coordinates
to window coordinates.

Points can be specified either with ordered pairs of real numbers, or
with complex numbers which will be interpreted as ordered pairs.

#### Setting the pen color
Drawing commands work with a virtual pen that can have a stroke color, used
to draw lines and curves, and a fill color, used to fill regions. To set the
stroke color to `red`, use the command

```
use red for stroke
```

(Alternatively, you can write `use red for line`). To set the fill color to
`blue`,

```
use blue for fill
```

#### Marking points

To draw a point in the view at point `z`, use the command

```
draw point at z
```

Points are drawn using the stroke color. The point can either be given by
a complex number or by an ordered pair of real numbers.

#### Drawing lines

To draw a line in the view from point `z` to point `w`, use the command

```
draw line from z to w
```

Lines are drawn using the stroke color. The points can either be given by
complex numbers or by ordered pairs of real numbers.

#### Drawing rectangles

To draw an empty rectangle in the view with opposite corners `z` and `w`,
use the command

```
draw rectangle from z to w
```

Or to make the rectangle filled:

```
draw filled rectangle from z to w
```

The boundary of the rectangle will be drawn using the stroke color, and
the rectangle will be filled with the fill color. The points can either
be given by complex numbers or by ordered pairs of real numbers.

#### Drawing circles

To draw an empty circle in the view with center `z` and radius `r`,
use the command

```
draw circle at z with radius r
```

Or to make the circle filled:

```
draw filled circle at z with radius r
```

The boundary of the circle will be drawn using the stroke color, and
the circle will be filled with the fill color. The center can either
be given by a complex number or by an ordered pair of real numbers.
