# Schedule expressions to run

Schedule expressions to run

## Usage

``` r
cmd_on_start(command, ...)

cmd_on_exit(command, ...)

cmd_on_fail(command, ...)

cmd_on_succeed(command, ...)
```

## Arguments

- command:

  A `command` object.

- ...:

  The expressions input will be captured with
  [`enquos()`](https://rlang.r-lib.org/reference/enquo.html). If your
  expressions depend on global data, you may want to unquote objects
  with [`!!`](https://rlang.r-lib.org/reference/injection-operator.html)
  to prevent unintended changes due to delayed evaluation.

  - `cmd_on_start`: Expression to be evaluated when the command started.

  - `cmd_on_exit`: Expression to be evaluated when the command finished.

  - `cmd_on_fail`: Expression to be evaluated when the command failed.

  - `cmd_on_succeed`: Expression to be evaluated when the command
    succeeded.

## Value

- `cmd_on_start`: The `command` object itself, with the start code
  updated.

&nbsp;

- `cmd_on_exit`: The `command` object itself, with the exit code
  updated.

&nbsp;

- `cmd_on_fail`: The `command` object itself, with the failure code
  updated.

&nbsp;

- `cmd_on_succeed`: The `command` object itself, with the successful
  code updated.

## Functions

- `cmd_on_start()`: define the startup code of the command

- `cmd_on_exit()`: define the exit code of the command

- `cmd_on_fail()`: define the failure code of the command

- `cmd_on_succeed()`: define the successful code of the command
