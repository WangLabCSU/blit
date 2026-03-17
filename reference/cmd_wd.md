# Setup the context for the command

Setup the context for the command

## Usage

``` r
cmd_wd(command, wd = NULL)

cmd_envvar(command, ..., action = "replace", sep = NULL)

cmd_envpath(command, ..., action = "prefix", name = "PATH")

cmd_condaenv(command, ..., root = NULL, action = "prefix")
```

## Arguments

- command:

  A `command` object.

- wd:

  A string or `NULL` define the working directory of the command.

- ...:

  \<[dynamic dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>:

  - `cmd_envvar`: Named character define the environment variables.

  - `cmd_envpath`: Unnamed character to define the `PATH`-like
    environment variables `name`.

  - `cmd_condaenv`: Unnamed character to specify the name of conda
    environment.

- action:

  Should the new values `"replace"`, `"prefix"` or `"suffix"` existing
  environment variables?

- sep:

  A string to separate new and old value when `action` is `"prefix"` or
  `"suffix"`. By default, `" "` will be used.

- name:

  A string define the PATH environment variable name. You can use this
  to define other `PATH`-like environment variable such as `PYTHONPATH`.

- root:

  A string specifying the path to the conda root prefix. If not
  provided, the function searches for the root in the following order:

  1.  the [option](https://rdrr.io/r/base/options.html)
      `blit.conda.root`.

  2.  the [environment variable](https://rdrr.io/r/base/Sys.getenv.html)
      `BLIT_CONDA_ROOT`.

  3.  the root prefix of
      [`appmamba()`](https://wanglabcsu.github.io/blit/reference/appmamba.md).

## Value

- `cmd_wd`: The `command` object itself, with working directory updated.

&nbsp;

- `cmd_envvar`: The `command` object itself, with running environment
  variable updated.

&nbsp;

- `cmd_envpath`: The `command` object itself, with running environment
  variable specified in `name` updated.

&nbsp;

- `cmd_condaenv`: The `command` object itself, with running environment
  variable `PATH` updated.

## Functions

- `cmd_wd()`: define the working directory.

- `cmd_envvar()`: define the environment variables.

- `cmd_envpath()`: define the `PATH`-like environment variables.

- `cmd_condaenv()`: Set `conda-like` environment prefix to the `PATH`
  environment variables.

## See also

- [`cmd_run()`](https://wanglabcsu.github.io/blit/reference/cmd_run.md)/[`cmd_help()`](https://wanglabcsu.github.io/blit/reference/cmd_run.md)/[`cmd_background()`](https://wanglabcsu.github.io/blit/reference/cmd_run.md)

- [`cmd_on_start()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)/[`cmd_on_exit()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)

- [`cmd_on_succeed()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)/[`cmd_on_fail()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)

- [`cmd_parallel()`](https://wanglabcsu.github.io/blit/reference/cmd_parallel.md)
