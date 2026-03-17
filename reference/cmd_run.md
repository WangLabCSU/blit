# Execute command

- `cmd_run`: Run the command.

- `cmd_help`: Print the help document for this command.

- `cmd_background`: Run the command in the background. This function is
  provided for completeness. Instead of using this function, we
  recommend using
  [`cmd_parallel()`](https://wanglabcsu.github.io/blit/reference/cmd_parallel.md),
  which can run multiple commands in the background while ensuring that
  all processes are properly cleaned up when the process exits.

## Usage

``` r
cmd_run(
  command,
  stdout = TRUE,
  stderr = TRUE,
  stdin = TRUE,
  stdout_callback = NULL,
  stderr_callback = NULL,
  timeout = NULL,
  spinner = FALSE,
  verbose = TRUE
)

cmd_help(
  command,
  stdout = TRUE,
  stderr = TRUE,
  stdout_callback = NULL,
  stderr_callback = NULL,
  verbose = TRUE
)

cmd_background(
  command,
  stdout = FALSE,
  stderr = FALSE,
  stdin = NULL,
  verbose = TRUE
)
```

## Arguments

- command:

  A `command` object.

- stdout, stderr:

  Specifies how the output/error streams of the child process are
  handled. Possible values include:

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

  For `stderr`, use string `"2>&1"` to redirect it to the same
  connection (i.e. pipe or file) as `stdout`.

  For `cmd_help()`, use `FALSE`/`NULL` will do nothing, since it always
  want to display the help document.

  For `cmd_background()`, `connection` cannot be used, and `TRUE` and
  `"|"` will fallback to the empty string `""`.

  When using a `connection` (if not already open) or a `string`,
  wrapping it with [`I()`](https://rdrr.io/r/base/AsIs.html) prevents
  overwriting existing content.

- stdin:

  should the input be diverted? Possible values include:

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

- stdout_callback, stderr_callback:

  Possible values include:

  - `NULL`: no callback function.

  - `function`: A function invoked for each line of standard output or
    error. Non-text (non-character) output will be ignored. The function
    should accept two arguments: one for the standard output or error
    and another for the running
    [`process`](http://processx.r-lib.org/reference/process.md) object.

- timeout:

  Timeout in seconds. This is a limit for the elapsed time running
  command in the separate process.

- spinner:

  Whether to show a reassuring spinner while the process is running.

- verbose:

  A single boolean value indicating whether the command execution should
  be verbose.

## Value

- `cmd_run`: Exit status invisiblely.

&nbsp;

- `cmd_help`: The input `command` invisiblely.

&nbsp;

- `cmd_background`: A
  [`process`](http://processx.r-lib.org/reference/process.md) object.

## See also

- [`cmd_wd()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_envvar()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_envpath()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_condaenv()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)

- [`cmd_on_start()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)/[`cmd_on_exit()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)

- [`cmd_on_succeed()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)/[`cmd_on_fail()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)

- [`cmd_parallel()`](https://wanglabcsu.github.io/blit/reference/cmd_parallel.md)
