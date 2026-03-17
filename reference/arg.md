# Deliver arguments of command

`arg()` is intended for user use, while `arg0()` is for developers and
does not perform argument validation.

## Usage

``` r
arg(tag, value, indicator = FALSE, lgl2int = FALSE, format = "%s", sep = " ")

arg0(
  tag,
  value,
  indicator = FALSE,
  lgl2int = FALSE,
  format = "%s",
  sep = " ",
  allow_null = FALSE,
  arg = caller_arg(value),
  call = caller_call()
)
```

## Arguments

- tag:

  A string specifying argument tag, like "-i", "-o".

- value:

  Value passed to the argument.

- indicator:

  A logical value specifying whether value should be an indicator of
  tag. If `TRUE`, logical value will explain the set or unset of tag.

- lgl2int:

  A logical value indicates whether transfrom value `TRUE` to `1` or
  `FALSE` to `0`. If `TRUE`, format will always be set to `"%d"`.

- format:

  The format of the value, details see
  [`sprintf`](https://rdrr.io/r/base/sprintf.html).

- sep:

  A character string used to separate `"tag"` and `"value"`, usually
  `" "` or `"="`.

- allow_null:

  A single logical value indicates whether `value` can be `NULL`.

- arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- call:

  The execution environment of a currently running function.

## Value

A string.
