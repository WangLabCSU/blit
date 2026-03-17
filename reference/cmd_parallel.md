# Execute a list of commands

Execute a list of commands

## Usage

``` r
cmd_parallel(
  ...,
  stdouts = FALSE,
  stderrs = FALSE,
  stdins = NULL,
  stdout_callbacks = NULL,
  stderr_callbacks = NULL,
  timeouts = NULL,
  threads = NULL,
  verbose = TRUE
)
```

## Arguments

- ...:

  A list of `command` object.

- stdouts, stderrs:

  Specifies how the output/error streams of the child process are
  handled. One of or a list of following values:

  - `FALSE`/`NULL`: Suppresses the output/error stream.

  - `TRUE`: Prints the child process output/error to the R console. If a
    standard output/error stream exists, `""` is used; otherwise, `"|"`
    is used.

  - **string**: An empty string `""` inherits the standard output/error
    stream from the main R process (Printing in the R console). If the
    main R process lacks a standard output/error stream, such as in
    `RGui` on Windows, an error is thrown. A string `"|"` prints to the
    standard output connection of R process (Using
    [`cat()`](https://rdrr.io/r/base/cat.html)). Alternative, a file
    name or path to redirect the output/error. If a relative path is
    specified, it remains relative to the current working directory,
    even if a different directory is set using
    [`cmd_wd()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md).

  - `connection`: A writable R
    [`connection`](https://rdrr.io/r/base/connections.html) object. If
    the connection is not
    [`open()`](https://rdrr.io/r/base/connections.html), it will be
    automatically opened.

  For `stderrs`, use string `"2>&1"` to redirect it to the same
  connection (i.e. pipe or file) as `stdout`.

  When a single file path is specified, the stdout/stderr of all
  commands will be merged into this single file.

- stdins:

  should the input be diverted? One of or a list of following values:

  - `FALSE`/`NULL`: no standard input.

  - `TRUE`: If a standard input stream exists, `""` is used; otherwise,
    `NULL` is used.

  - **string**: An empty string `""` inherits the standard input stream
    from the main R process. If the main R process lacks a standard
    input stream, such as in `RGui` on Windows, an error is thrown.
    Alternative, a file name or path to redirect the input. If a
    relative path is specified, it remains relative to the current
    working directory, even if a different directory is set using
    [`cmd_wd()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md).

- stdout_callbacks, stderr_callbacks:

  One of or a list of following values:

  - `NULL`: no callback function.

  - `function`: A function invoked for each line of standard output or
    error. Non-text (non-character) output will be ignored. The function
    should accept two arguments: one for the standard output or error
    and another for the running
    [`process`](http://processx.r-lib.org/reference/process.md) object.

- timeouts:

  Timeout in seconds. Can be a single value or a list, specifying the
  maximum elapsed time for running the command in the separate process.

- threads:

  Number of threads to use.

- verbose:

  A single boolean value indicating whether the command execution should
  be verbose.

## Value

A list of exit status invisiblely.

## See also

- [`cmd_wd()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_envvar()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_envpath()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_condaenv()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)

- [`cmd_on_start()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)/[`cmd_on_exit()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)

- [`cmd_on_succeed()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)/[`cmd_on_fail()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)

- `cmd_parallel()`
