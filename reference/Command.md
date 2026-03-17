# R6 Class to prepare command parameters.

`Command` is an R6 class used by developers to create new command. It
should not be used by end users.

## See also

make_command

## Methods

### Public methods

- [`Command$new()`](#method-Command-new)

- [`Command$build_command()`](#method-Command-build_command)

- [`Command$get_on_start()`](#method-Command-get_on_start)

- [`Command$get_on_exit()`](#method-Command-get_on_exit)

- [`Command$get_on_fail()`](#method-Command-get_on_fail)

- [`Command$get_on_succeed()`](#method-Command-get_on_succeed)

- [`Command$print()`](#method-Command-print)

- [`Command$clone()`](#method-Command-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `Command` object.

#### Usage

    Command$new(...)

#### Arguments

- `...`:

  Additional argument passed into command.

------------------------------------------------------------------------

### Method `build_command()`

Build the command line

#### Usage

    Command$build_command(help = FALSE, verbose = TRUE)

#### Arguments

- `help`:

  A boolean value indicating whether to build parameters for help
  document or not.

- `verbose`:

  A boolean value indicating whether the command execution should be
  verbose.

- `envir`:

  An environment used to Execute command.

#### Returns

An atomic character combine the command and parameters.

------------------------------------------------------------------------

### Method `get_on_start()`

Get the command startup code

#### Usage

    Command$get_on_start()

#### Returns

A list of
[`quosures`](https://rlang.r-lib.org/reference/defusing-advanced.html).

------------------------------------------------------------------------

### Method `get_on_exit()`

Get the command exit code

#### Usage

    Command$get_on_exit()

#### Returns

A list of
[`quosures`](https://rlang.r-lib.org/reference/defusing-advanced.html).

------------------------------------------------------------------------

### Method `get_on_fail()`

Get the command failure code

#### Usage

    Command$get_on_fail()

#### Returns

A list of
[`quosures`](https://rlang.r-lib.org/reference/defusing-advanced.html).

------------------------------------------------------------------------

### Method `get_on_succeed()`

Get the command succeessful code

#### Usage

    Command$get_on_succeed()

#### Returns

A list of
[`quosures`](https://rlang.r-lib.org/reference/defusing-advanced.html).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Build parameters to run command.

#### Usage

    Command$print(indent = NULL)

#### Arguments

- `indent`:

  A single integer number giving the space of indent.

#### Returns

The object itself.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Command$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
