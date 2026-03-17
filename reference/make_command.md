# Helper function to create new command.

`make_command` is a helper function used by developers to create
function for a new
[`Command`](https://wanglabcsu.github.io/blit/reference/Command.md)
object. It should not be used by end users.

## Usage

``` r
make_command(name, fun, envir = parent.frame())
```

## Arguments

- name:

  A string of the function name.

- fun:

  A function used to initialize the
  [`Command`](https://wanglabcsu.github.io/blit/reference/Command.md)
  object.

- envir:

  A environment used to bind the created function.

## Value

A function.

## See also

[`Command`](https://wanglabcsu.github.io/blit/reference/Command.md)
