
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blit: Bioinformatics Library for Integrated Tools <img src="man/figures/logo.png" alt="logo" align="right" height="140" width="120"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/WangLabCSU/blit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WangLabCSU/blit/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/blit)](https://CRAN.R-project.org/package=blit)
[![](https://cranlogs.r-pkg.org/badges/blit)](https://cran.r-project.org/package=blit)
<!-- badges: end -->

The goal of `blit` is to make it easy to execute command line tool from
R.

## Installation

You can install `blit` from `CRAN` using:

``` r
install.packages("blit")
```

Alternatively, install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("WangLabCSU/blit")
```

## Example

``` r
library(blit)
```

### Execute command

To build a `command`, simply use `exec`. The first argument is the
command name, and you can also provide the full path. After that, pass
the command parameters. This will create a `command` object:

``` r
exec("echo", "$PATH")
#> <Execute: echo>
```

To run the command, just pass the `command` object to the `cmd_run()`
(Note: `stdout = "|"` is always used in the vignette to ensure that the
standard output can be captured by knitr.)

``` r
Sys.setenv(TEST = "blit is awesome")
exec("echo", "$TEST") |> cmd_run(stdout = "|")
#> Running command (2025-04-04 02:19:34): echo $TEST
#> 
#> blit is awesome
#> Command process finished
#> System command succeed
```

Alternatively, you can run it in the background. In this case, a
[`process`](https://processx.r-lib.org/index.html) object will be
returned. For more information, refer to the official site:

``` r
proc <- exec("echo", "$TEST") |> cmd_background(stdout = "")
proc$kill()
Sys.unsetenv("TEST")
```

> We use some tricks to capture the output from the background process.
> The actual implementation in the `README.Rmd` differs, but the output
> remains the same.

    #> Running command (2025-04-04 02:19:34): echo $TEST
    #> blit is awesome

`cmd_background()` is provided for completeness. Instead of using this
function, we recommend using `cmd_parallel()`, which can run multiple
commands in the background while ensuring that all processes are
properly cleaned up when the process exits.

``` r
# ip address are copied from quora <What are some famous IP addresses?>: https://qr.ae/pYlnbQ
address <- c("localhost", "208.67.222.222", "8.8.8.8", "8.8.4.4")
cmd_parallel(
    !!!lapply(address, function(ip) exec("ping", ip)),
    stdouts = TRUE,
    stdout_callbacks = lapply(
        seq_len(4),
        function(i) {
            force(i)
            function(text, proc) {
                sprintf("Connection %d: %s", i, text)
            }
        }
    ),
    timeouts = 4, # terminate after 4s
    threads = 4
)
#> Running command (2025-04-04 02:19:34): ping localhost
#> Running command (2025-04-04 02:19:34): ping 208.67.222.222
#> Running command (2025-04-04 02:19:34): ping 8.8.8.8
#> Running command (2025-04-04 02:19:34): ping 8.8.4.4
#> 
#> Connection 1: PING localhost (::1) 56 data bytes
#> Connection 1: 64 bytes from localhost (::1): icmp_seq=1 ttl=64 time=0.016 ms
#> ⠙ 0/4 [0/s] [elapsed in 83ms] @ 2025-04-04 02:19:34
#> ⠹ 0/4 [0/s] [elapsed in 299ms] @ 2025-04-04 02:19:34
#> ⠸ 0/4 [0/s] [elapsed in 509ms] @ 2025-04-04 02:19:34
#> ⠼ 0/4 [0/s] [elapsed in 719ms] @ 2025-04-04 02:19:35
#> ⠴ 0/4 [0/s] [elapsed in 941ms] @ 2025-04-04 02:19:35
#> ⠦ 0/4 [0/s] [elapsed in 1.1s] @ 2025-04-04 02:19:35
#> ⠧ 0/4 [0/s] [elapsed in 1.4s] @ 2025-04-04 02:19:35
#> ⠇ 0/4 [0/s] [elapsed in 1.6s] @ 2025-04-04 02:19:36
#> Connection 1: 64 bytes from localhost (::1): icmp_seq=2 ttl=64 time=0.033 ms
#> ⠏ 0/4 [0/s] [elapsed in 1.6s] @ 2025-04-04 02:19:36
#> ⠋ 0/4 [0/s] [elapsed in 1.8s] @ 2025-04-04 02:19:36
#> ⠙ 0/4 [0/s] [elapsed in 2s] @ 2025-04-04 02:19:36
#> ⠹ 0/4 [0/s] [elapsed in 2.2s] @ 2025-04-04 02:19:36
#> Connection 1: 64 bytes from localhost (::1): icmp_seq=3 ttl=64 time=0.041 ms
#> ⠸ 0/4 [0/s] [elapsed in 2.2s] @ 2025-04-04 02:19:36
#> ⠼ 0/4 [0/s] [elapsed in 2.4s] @ 2025-04-04 02:19:36
#> ⠴ 0/4 [0/s] [elapsed in 2.6s] @ 2025-04-04 02:19:37
#> ⠦ 0/4 [0/s] [elapsed in 2.9s] @ 2025-04-04 02:19:37
#> ⠧ 0/4 [0/s] [elapsed in 3.1s] @ 2025-04-04 02:19:37
#> ⠇ 0/4 [0/s] [elapsed in 3.3s] @ 2025-04-04 02:19:37
#> ⠏ 0/4 [0/s] [elapsed in 3.5s] @ 2025-04-04 02:19:37
#> ⠋ 0/4 [0/s] [elapsed in 3.7s] @ 2025-04-04 02:19:38
#> Connection 1: 64 bytes from localhost (::1): icmp_seq=4 ttl=64 time=0.032 ms
#> ⠙ 0/4 [0/s] [elapsed in 3.7s] @ 2025-04-04 02:19:38
#> ⠹ 0/4 [0/s] [elapsed in 3.9s] @ 2025-04-04 02:19:38
#> ⠸ 0/4 [0/s] [elapsed in 4.1s] @ 2025-04-04 02:19:38
#> ⠼ 0/4 [0/s] [elapsed in 4.1s] @ 2025-04-04 02:19:38
#> Command process finished
#> Command process finished
#> Command process finished
#> Command process finished
#> ⠼ 4/4 [0.96/s] [elapsed in 4.2s] @ 2025-04-04 02:19:38
#> Warning: [Command: 1] System command timed out in 4.1 secs (status: -9)
#> Warning: [Command: 2] System command timed out in 4.1 secs (status: -9)
#> Warning: [Command: 3] System command timed out in 4.1 secs (status: -9)
#> Warning: [Command: 4] System command timed out in 4.1 secs (status: -9)
```

### Environment context

The `blit` package provides several functions to manage and control the
environment context:

- `cmd_wd`: define the working directory.
- `cmd_envvar`: define the environment variables.
- `cmd_envpath`: define the `PATH`-like environment variables.
- `cmd_condaenv`: define the `PATH` environment variables with conda
  environment.

``` r
exec("echo", "$(pwd)") |>
    cmd_wd(tempdir()) |>
    cmd_run(stdout = "|")
#> Working Directory: '/tmp/RtmpmtrLuL'
#> Running command (2025-04-04 02:19:38): echo $(pwd)
#> 
#> /tmp/RtmpmtrLuL
#> Command process finished
#> System command succeed
```

``` r
exec("echo", "$TEST") |>
    cmd_envvar(TEST = "blit is very awesome") |>
    cmd_run(stdout = "|")
#> Setting environment variables: TEST
#> Running command (2025-04-04 02:19:38): echo $TEST
#> 
#> blit is very awesome
#> Command process finished
#> System command succeed
```

``` r
exec("echo", "$PATH") |>
    cmd_envpath("PATH_IS_HERE", action = "replace") |>
    cmd_run(stdout = "|")
#> Setting environment variables: PATH
#> Running command (2025-04-04 02:19:38): echo $PATH
#> 
#> PATH_IS_HERE
#> Command process finished
#> System command succeed
```

> Note: `echo` is a built-in command of the linux shell, so it remains
> available even after modifying the `PATH` environment variable.

`cmd_condaenv()` can add `conda`/`mamba` environment prefix to the
`PATH` environment variable.

`Conda`/`mamba` are open-source package and environment management
systems that facilitate the installation of multiple software versions
and their dependencies. They allow easy switching between environments
and are compatible with Linux, macOS, and Windows.

`cmd_condaenv()` function accepts multiple `conda`/`mamba` environment
prefixes and an optional `root` argument specifying the path to the
`conda`/`mamba` root prefix. If `root` is not provided, the function
searches for the root in the following order:

1.  the option: `blit.conda.root`.
2.  the environment variable: `BLIT_CONDA_ROOT`.
3.  the root prefix of \[`appmamba()`\] (Please see the
    `Software management` section for details).

The `cmd_condaenv()` function searches for the specified environment
prefix within the provided `root` path.

### Software management

The `blit` package integrates with `micromamba`, a lightweight version
of the mamba package manager, for efficient software environment
management.

You can install `micromamba` with `install_appmamba()`.

``` r
install_appmamba()
#> Installing appmamba
#> Downloading from 'https://micro.mamba.pm/api/micromamba/linux-64/latest'
#> Install appmamba successfully!
```

The `appmamba()` function executes specified `micromamba` commands.
Running it without arguments shows the help document:

``` r
appmamba()
#> Running command (2025-04-04 02:19:41):
#> /home/runner/.local/share/R/blit/apps/appmamba/bin/micromamba --root-prefix
#> /home/runner/.local/share/R/blit/appmamba --help
```

To create a new environment named `samtools` and install `samtools` from
`Bioconda`, use:

``` r
appmamba("create", "--yes", "--name samtools", "bioconda::samtools")
#> Running command (2025-04-04 02:19:41):
#> /home/runner/.local/share/R/blit/apps/appmamba/bin/micromamba --root-prefix
#> /home/runner/.local/share/R/blit/appmamba create --yes --name samtools
#> bioconda::samtools
```

Once the environment is created, you can execute commands within it. The
following example locates the samtools binary within the specified
environment:

``` r
exec("which", "samtools") |>
    cmd_condaenv("samtools") |>
    cmd_run()
#> Setting environment variables: PATH
#> Running command (2025-04-04 02:19:53): which samtools
#> Command process finished
#> System command succeed
```

You may want to clean the created environment-`samtools`.

``` r
appmamba("env", "remove", "--yes", "--name samtools")
#> Running command (2025-04-04 02:19:53):
#> /home/runner/.local/share/R/blit/apps/appmamba/bin/micromamba --root-prefix
#> /home/runner/.local/share/R/blit/appmamba env remove --yes --name samtools
```

For more details, please see
<https://mamba.readthedocs.io/en/latest/user_guide/micromamba.html>.

### Schedule expressions

Several functions allow you to schedule expressions:

- `cmd_on_start`/`cmd_on_exit`: define the startup, or exit code of the
  command.
- `cmd_on_succeed`/`cmd_on_fail`: define the code to be run when command
  succeed or fail.

``` r
file <- tempfile()
file.create(file)
#> [1] TRUE
file.exists(file)
#> [1] TRUE
exec("ping", "localhost") |>
    cmd_on_exit(file.remove(file)) |>
    cmd_run(timeout = 5, stdout = "|") # terminate it after 5s
#> Running command (2025-04-04 02:19:54): ping localhost
#> 
#> PING localhost (::1) 56 data bytes
#> 64 bytes from localhost (::1): icmp_seq=1 ttl=64 time=0.018 ms
#> 64 bytes from localhost (::1): icmp_seq=2 ttl=64 time=0.032 ms
#> 64 bytes from localhost (::1): icmp_seq=3 ttl=64 time=0.033 ms
#> 64 bytes from localhost (::1): icmp_seq=4 ttl=64 time=0.032 ms
#> 64 bytes from localhost (::1): icmp_seq=5 ttl=64 time=0.033 ms
#> Command process finished
#> Warning: System command timed out in 5 secs (status: -9)
file.exists(file)
#> [1] FALSE
```

We can also register code for succeessful or failure command
respectively (Timeout means command fail):

``` r
file <- tempfile()
file.create(file)
#> [1] TRUE
file.exists(file)
#> [1] TRUE
exec("ping", "localhost") |>
    cmd_on_fail(file.remove(file)) |>
    cmd_run(timeout = 5, stdout = "|") # terminate it after 5s
#> Running command (2025-04-04 02:19:59): ping localhost
#> 
#> PING localhost (::1) 56 data bytes
#> 64 bytes from localhost (::1): icmp_seq=1 ttl=64 time=0.019 ms
#> 64 bytes from localhost (::1): icmp_seq=2 ttl=64 time=0.032 ms
#> 64 bytes from localhost (::1): icmp_seq=3 ttl=64 time=0.031 ms
#> 64 bytes from localhost (::1): icmp_seq=4 ttl=64 time=0.032 ms
#> 64 bytes from localhost (::1): icmp_seq=5 ttl=64 time=0.039 ms
#> Command process finished
#> Warning: System command timed out in 5 secs (status: -9)
file.exists(file)
#> [1] FALSE
```

``` r
file <- tempfile()
file.create(file)
#> [1] TRUE
file.exists(file)
#> [1] TRUE
exec("ping", "localhost") |>
    cmd_on_succeed(file.remove(file)) |>
    cmd_run(timeout = 5, stdout = "|") # terminate it after 5s
#> Running command (2025-04-04 02:20:04): ping localhost
#> 
#> PING localhost (::1) 56 data bytes
#> 64 bytes from localhost (::1): icmp_seq=1 ttl=64 time=0.016 ms
#> 64 bytes from localhost (::1): icmp_seq=2 ttl=64 time=0.031 ms
#> 64 bytes from localhost (::1): icmp_seq=3 ttl=64 time=0.032 ms
#> 64 bytes from localhost (::1): icmp_seq=4 ttl=64 time=0.034 ms
#> 64 bytes from localhost (::1): icmp_seq=5 ttl=64 time=0.033 ms
#> Command process finished
#> Warning: System command timed out in 5 secs (status: -9)
file.exists(file) # file remain exist as timeout means command failed
#> [1] TRUE
file.remove(file)
#> [1] TRUE
```

### Built-in commands

`blit` provides several built-in functions for directly executing
specific commands., these include: [samtools](https://www.htslib.org/),
[alleleCounter](https://github.com/cancerit/alleleCount),
[cellranger](https://www.10xgenomics.com/cn/support/software/cell-ranger/latest),
[fastq_pair](https://github.com/linsalrob/fastq-pair),
[gistic2](https://broadinstitute.github.io/gistic2/),
[KrakenTools](https://github.com/jenniferlu717/KrakenTools),
[kraken2](https://github.com/DerrickWood/kraken2/wiki/Manual),
[perl](https://www.perl.org/),
[pySCENIC](https://github.com/aertslab/pySCENIC),
[python](https://www.python.org/),
[seqkit](https://bioinf.shenwei.me/seqkit/),
[trust4](https://github.com/liulab-dfci/TRUST4).

For these commands, you can also use `cmd_help()` to print the help
document.

``` r
python() |> cmd_help(stdout = "|")
#> Running command (2025-04-04 02:20:09): /usr/bin/python --help
#> 
#> usage: /usr/bin/python [option] ... [-c cmd | -m mod | file | -] [arg] ...
#> Options (and corresponding environment variables):
#> -b     : issue warnings about converting bytes/bytearray to str and comparing
#>          bytes/bytearray with str or bytes with int. (-bb: issue errors)
#> -B     : don't write .pyc files on import; also PYTHONDONTWRITEBYTECODE=x
#> -c cmd : program passed in as string (terminates option list)
#> -d     : turn on parser debugging output (for experts only, only works on
#>          debug builds); also PYTHONDEBUG=x
#> -E     : ignore PYTHON* environment variables (such as PYTHONPATH)
#> -h     : print this help message and exit (also -? or --help)
#> -i     : inspect interactively after running script; forces a prompt even
#>          if stdin does not appear to be a terminal; also PYTHONINSPECT=x
#> -I     : isolate Python from the user's environment (implies -E and -s)
#> -m mod : run library module as a script (terminates option list)
#> -O     : remove assert and __debug__-dependent statements; add .opt-1 before
#>          .pyc extension; also PYTHONOPTIMIZE=x
#> -OO    : do -O changes and also discard docstrings; add .opt-2 before
#>          .pyc extension
#> -P     : don't prepend a potentially unsafe path to sys.path; also
#>          PYTHONSAFEPATH
#> -q     : don't print version and copyright messages on interactive startup
#> -s     : don't add user site directory to sys.path; also PYTHONNOUSERSITE=x
#> -S     : don't imply 'import site' on initialization
#> -u     : force the stdout and stderr streams to be unbuffered;
#>          this option has no effect on stdin; also PYTHONUNBUFFERED=x
#> -v     : verbose (trace import statements); also PYTHONVERBOSE=x
#>          can be supplied multiple times to increase verbosity
#> -V     : print the Python version number and exit (also --version)
#>          when given twice, print more information about the build
#> -W arg : warning control; arg is action:message:category:module:lineno
#>          also PYTHONWARNINGS=arg
#> -x     : skip first line of source, allowing use of non-Unix forms of #!cmd
#> -X opt : set implementation-specific option
#> --check-hash-based-pycs always|default|never:
#>          control how Python invalidates hash-based .pyc files
#> --help-env: print help about Python environment variables and exit
#> --help-xoptions: print help about implementation-specific -X options and exit
#> --help-all: print complete help information and exit
#> 
#> Arguments:
#> file   : program read from script file
#> -      : program read from stdin (default; interactive mode if a tty)
#> arg ...: arguments passed to program in sys.argv[1:]
#> Command process finished
```

``` r
perl() |> cmd_help(stdout = "|")
#> Running command (2025-04-04 02:20:09): /usr/bin/perl --help
#> 
#> 
#> Usage: /usr/bin/perl [switches] [--] [programfile] [arguments]
#>   -0[octal/hexadecimal] specify record separator (\0, if no argument)
#>   -a                    autosplit mode with -n or -p (splits $_ into @F)
#>   -C[number/list]       enables the listed Unicode features
#>   -c                    check syntax only (runs BEGIN and CHECK blocks)
#>   -d[t][:MOD]           run program under debugger or module Devel::MOD
#>   -D[number/letters]    set debugging flags (argument is a bit mask or alphabets)
#>   -e commandline        one line of program (several -e's allowed, omit programfile)
#>   -E commandline        like -e, but enables all optional features
#>   -f                    don't do $sitelib/sitecustomize.pl at startup
#>   -F/pattern/           split() pattern for -a switch (//'s are optional)
#>   -g                    read all input in one go (slurp), rather than line-by-line (alias for -0777)
#>   -i[extension]         edit <> files in place (makes backup if extension supplied)
#>   -Idirectory           specify @INC/#include directory (several -I's allowed)
#>   -l[octnum]            enable line ending processing, specifies line terminator
#>   -[mM][-]module        execute "use/no module..." before executing program
#>   -n                    assume "while (<>) { ... }" loop around program
#>   -p                    assume loop like -n but print line also, like sed
#>   -s                    enable rudimentary parsing for switches after programfile
#>   -S                    look for programfile using PATH environment variable
#>   -t                    enable tainting warnings
#>   -T                    enable tainting checks
#>   -u                    dump core after parsing program
#>   -U                    allow unsafe operations
#>   -v                    print version, patchlevel and license
#>   -V[:configvar]        print configuration summary (or a single Config.pm variable)
#>   -w                    enable many useful warnings
#>   -W                    enable all warnings
#>   -x[directory]         ignore text before #!perl line (optionally cd to directory)
#>   -X                    disable all warnings
#>   
#> Run 'perldoc perl' for more help with Perl.
#> Command process finished
```

And it is very easily to extend for other commands.

### Pipe

One of the great features of `blit` is its ability to translate the R
pipe (`%>%` or `|>`) into the Linux pipe (`|`). All functions used to
create a `command` object can accept another `command` object. The
internal will capture the first unnamed input value. If it is a
`command` object, it will be removed from the call and saved. When the
`command` object is run, the saved command will be passed through the
pipe (`|`) to the command. Here we take the `gzip` command as an example
(assuming you’re using a Linux system).

``` r
tmpdir <- tempdir()
file <- tempfile(tmpdir = tmpdir)
writeLines(letters, con = file)
file2 <- tempfile()
exec("gzip", "-c", file) |>
    exec("gzip", "-d", ">", file2) |>
    cmd_run(stdout = "|")
#> Running command (2025-04-04 02:20:09): gzip -c /tmp/RtmpmtrLuL/file1da94c2c4763
#> | gzip -d > /tmp/RtmpmtrLuL/file1da9427e7232
#> Command process finished
#> System command succeed
identical(readLines(file), readLines(file2))
#> [1] TRUE
```

In the last we clean the temporary files.

``` r
file.remove(file)
#> [1] TRUE
file.remove(file2)
#> [1] TRUE
```

## Development

To add a new command, use the `make_command` function. This helper
function is designed to assist developers in creating functions that
initialize new `command` objects. A `command` object is a bundle of
multiple `Command` R6 objects (note the uppercase `"C"` in `Command`,
which distinguishes it from the `command` object) and the associated
running environment (including the working directory and environment
variables).

The `make_command` function accepts a function that initializes a new
`Command` object and, when necessary, validates the input arguments. The
core purpose is to create a new `Command` R6 object, so familiarity with
the R6 class system is essential.

There are several private methods or fields you may want to override
when creating a new `Command` R6 object. The first method is
`command_locate`, which determines how to locate the command path. By
default, it will attempt to use the `cmd` argument provided by the user.
If no `cmd` argument is supplied, it will try to locate the command
using the `alias` method. In most cases, you will only need to provide
values for the `alias` method, rather than overriding the
`command_locate` method.

For example, consider the `ping` command. Here is how you can define it:

``` r
Ping <- R6::R6Class(
    "Ping",
    inherit = Command,
    private = list(alias = function() "ping")
)
ping <- make_command("ping", function(..., ping = NULL) {
    Ping$new(cmd = ping, ...)
})
ping("8.8.8.8") |> cmd_run(timeout = 5, stdout = "|") # terminate it after 5s
#> Running command (2025-04-04 02:20:09): /usr/bin/ping 8.8.8.8
#> Command process finished
#> Warning: System command timed out in 5 secs (status: -9)
```

For command-line tools, the input parameters should always be
characters. The core principle of the `Command` object is to convert all
R objects (such as data frames) into characters—typically file paths of
R objects that have been saved to disk.

## Session Informations

``` r
sessionInfo()
#> R version 4.4.3 (2025-02-28)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.2 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] blit_0.2.0.9000
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.37     R6_2.6.1          fastmap_1.2.0     xfun_0.52        
#>  [5] knitr_1.50        parallel_4.4.3    htmltools_0.5.8.1 rmarkdown_2.29   
#>  [9] ps_1.9.0          cli_3.6.4         processx_3.8.6    data.table_1.17.0
#> [13] compiler_4.4.3    tools_4.4.3       evaluate_1.0.3    yaml_2.3.10      
#> [17] rlang_1.1.5
```
