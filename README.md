
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

To build a `command`, simply use `exec`. The first argument is the
command name, and you can also provide the full path. After that, pass
the command parameters. This will create a `command` object:

``` r
exec("echo", "$PATH")
#> <Execute: echo>
```

To run the command, just pass the `command` object to the `cmd_run()`
(Note: `stdout = "|"` is always used in the vignette to ensure that the
standard output is captured by knitr.)

``` r
Sys.setenv(TEST = "blit is awesome")
exec("echo", "$TEST") |> cmd_run(stdout = "|")
#> Running command (2025-03-31 09:41:36): echo $TEST
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

    #> Running command (2025-03-31 09:41:36): echo $TEST
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
#> Running command (2025-03-31 09:41:36): ping localhost
#> Running command (2025-03-31 09:41:36): ping 208.67.222.222
#> Running command (2025-03-31 09:41:36): ping 8.8.8.8
#> Running command (2025-03-31 09:41:36): ping 8.8.4.4
#> 
#> Connection 1: PING localhost (127.0.0.1) 56(84) bytes of data.
#> Connection 1: 64 bytes from localhost (127.0.0.1): icmp_seq=1 ttl=64 time=0.008 ms
#> ⠙ 0/4 [0/s] [elapsed in 39ms] @ 2025-03-31 09:41:36
#> Connection 2: PING 208.67.222.222 (208.67.222.222) 56(84) bytes of data.
#> Connection 2: 64 bytes from 208.67.222.222: icmp_seq=1 ttl=48 time=57.4 ms
#> ⠹ 0/4 [0/s] [elapsed in 78ms] @ 2025-03-31 09:41:36
#> Connection 3: PING 8.8.8.8 (8.8.8.8) 56(84) bytes of data.
#> Connection 3: 64 bytes from 8.8.8.8: icmp_seq=1 ttl=106 time=45.5 ms
#> ⠸ 0/4 [0/s] [elapsed in 83ms] @ 2025-03-31 09:41:36
#> Connection 4: PING 8.8.4.4 (8.8.4.4) 56(84) bytes of data.
#> Connection 4: 64 bytes from 8.8.4.4: icmp_seq=1 ttl=104 time=153 ms
#> ⠼ 0/4 [0/s] [elapsed in 191ms] @ 2025-03-31 09:41:36
#> ⠴ 0/4 [0/s] [elapsed in 401ms] @ 2025-03-31 09:41:36
#> ⠦ 0/4 [0/s] [elapsed in 606ms] @ 2025-03-31 09:41:37
#> ⠧ 0/4 [0/s] [elapsed in 813ms] @ 2025-03-31 09:41:37
#> ⠇ 0/4 [0/s] [elapsed in 1s] @ 2025-03-31 09:41:37
#> Connection 1: 64 bytes from localhost (127.0.0.1): icmp_seq=2 ttl=64 time=0.019 ms
#> ⠏ 0/4 [0/s] [elapsed in 1s] @ 2025-03-31 09:41:37
#> Connection 2: 64 bytes from 208.67.222.222: icmp_seq=2 ttl=48 time=51.9 ms
#> ⠋ 0/4 [0/s] [elapsed in 1.1s] @ 2025-03-31 09:41:37
#> Connection 3: 64 bytes from 8.8.8.8: icmp_seq=2 ttl=106 time=45.4 ms
#> ⠙ 0/4 [0/s] [elapsed in 1.1s] @ 2025-03-31 09:41:37
#> Connection 4: 64 bytes from 8.8.4.4: icmp_seq=2 ttl=104 time=154 ms
#> ⠹ 0/4 [0/s] [elapsed in 1.2s] @ 2025-03-31 09:41:37
#> ⠸ 0/4 [0/s] [elapsed in 1.4s] @ 2025-03-31 09:41:37
#> ⠼ 0/4 [0/s] [elapsed in 1.6s] @ 2025-03-31 09:41:38
#> ⠴ 0/4 [0/s] [elapsed in 1.8s] @ 2025-03-31 09:41:38
#> ⠦ 0/4 [0/s] [elapsed in 2s] @ 2025-03-31 09:41:38
#> Connection 1: 64 bytes from localhost (127.0.0.1): icmp_seq=3 ttl=64 time=0.014 ms
#> ⠧ 0/4 [0/s] [elapsed in 2s] @ 2025-03-31 09:41:38
#> Connection 2: 64 bytes from 208.67.222.222: icmp_seq=3 ttl=48 time=52.0 ms
#> ⠇ 0/4 [0/s] [elapsed in 2.1s] @ 2025-03-31 09:41:38
#> Connection 3: 64 bytes from 8.8.8.8: icmp_seq=3 ttl=106 time=45.1 ms
#> ⠏ 0/4 [0/s] [elapsed in 2.1s] @ 2025-03-31 09:41:38
#> Connection 4: 64 bytes from 8.8.4.4: icmp_seq=3 ttl=104 time=153 ms
#> ⠋ 0/4 [0/s] [elapsed in 2.2s] @ 2025-03-31 09:41:38
#> ⠙ 0/4 [0/s] [elapsed in 2.4s] @ 2025-03-31 09:41:38
#> ⠹ 0/4 [0/s] [elapsed in 2.6s] @ 2025-03-31 09:41:39
#> ⠸ 0/4 [0/s] [elapsed in 2.8s] @ 2025-03-31 09:41:39
#> ⠼ 0/4 [0/s] [elapsed in 3s] @ 2025-03-31 09:41:39
#> Connection 1: 64 bytes from localhost (127.0.0.1): icmp_seq=4 ttl=64 time=0.013 ms
#> ⠴ 0/4 [0/s] [elapsed in 3.1s] @ 2025-03-31 09:41:39
#> Connection 2: 64 bytes from 208.67.222.222: icmp_seq=4 ttl=48 time=53.6 ms
#> ⠦ 0/4 [0/s] [elapsed in 3.1s] @ 2025-03-31 09:41:39
#> Connection 3: 64 bytes from 8.8.8.8: icmp_seq=4 ttl=106 time=45.9 ms
#> ⠧ 0/4 [0/s] [elapsed in 3.1s] @ 2025-03-31 09:41:39
#> Connection 4: 64 bytes from 8.8.4.4: icmp_seq=4 ttl=104 time=155 ms
#> ⠇ 0/4 [0/s] [elapsed in 3.2s] @ 2025-03-31 09:41:39
#> ⠏ 0/4 [0/s] [elapsed in 3.4s] @ 2025-03-31 09:41:39
#> ⠋ 0/4 [0/s] [elapsed in 3.6s] @ 2025-03-31 09:41:40
#> ⠙ 0/4 [0/s] [elapsed in 3.8s] @ 2025-03-31 09:41:40
#> ⠹ 0/4 [0/s] [elapsed in 4s] @ 2025-03-31 09:41:40
#> ⠸ 0/4 [0/s] [elapsed in 4s] @ 2025-03-31 09:41:40
#> ⠼ 0/4 [0/s] [elapsed in 4s] @ 2025-03-31 09:41:40
#> ⠴ 0/4 [0/s] [elapsed in 4s] @ 2025-03-31 09:41:40
#> ⠦ 0/4 [0/s] [elapsed in 4s] @ 2025-03-31 09:41:40
#> Command process finished
#> Command process finished
#> Command process finished
#> Command process finished
#> ⠦ 4/4 [0.98/s] [elapsed in 4.1s] @ 2025-03-31 09:41:40
#> Warning: [Command: 1] System command timed out in 4 secs (status: -9)
#> Warning: [Command: 2] System command timed out in 4 secs (status: -9)
#> Warning: [Command: 3] System command timed out in 4 secs (status: -9)
#> Warning: [Command: 4] System command timed out in 4 secs (status: -9)
```

Several functions allow you to control the environment or schedule
expressions:

- `cmd_wd`: define the working directory.
- `cmd_envvar`: define the environment variables.
- `cmd_envpath`: define the `PATH`-like environment variables.
- `cmd_condaenv`: define the `PATH` environment variables with conda
  environment.
- `cmd_on_start`/`cmd_on_exit`: define the startup, or exit code of the
  command.
- `cmd_on_succeed`/`cmd_on_fail`: define the code to be run when command
  fail or succeed.

``` r
exec("echo", "$(pwd)") |>
    cmd_wd(tempdir()) |>
    cmd_run(stdout = "|")
#> Working Directory: '/tmp/RtmpNwcuWW'
#> Running command (2025-03-31 09:41:40): echo $(pwd)
#> 
#> /tmp/RtmpNwcuWW
#> Command process finished
#> System command succeed
```

``` r
exec("echo", "$TEST") |>
    cmd_envvar(TEST = "blit is very awesome") |>
    cmd_run(stdout = "|")
#> Setting environment variables: TEST
#> Running command (2025-03-31 09:41:40): echo $TEST
#> 
#> blit is very awesome
#> Command process finished
#> System command succeed
```

``` r
file <- tempfile()
file.create(file)
#> [1] TRUE
file.exists(file)
#> [1] TRUE
exec("ping", "localhost") |>
    cmd_on_exit(file.remove(file)) |>
    cmd_run(timeout = 5, stdout = "|") # terminate it after 5s
#> Running command (2025-03-31 09:41:40): ping localhost
#> 
#> PING localhost (127.0.0.1) 56(84) bytes of data.
#> 64 bytes from localhost (127.0.0.1): icmp_seq=1 ttl=64 time=0.008 ms
#> 64 bytes from localhost (127.0.0.1): icmp_seq=2 ttl=64 time=0.020 ms
#> 64 bytes from localhost (127.0.0.1): icmp_seq=3 ttl=64 time=0.016 ms
#> 64 bytes from localhost (127.0.0.1): icmp_seq=4 ttl=64 time=0.018 ms
#> 64 bytes from localhost (127.0.0.1): icmp_seq=5 ttl=64 time=0.012 ms
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
#> Running command (2025-03-31 09:41:45): ping localhost
#> 
#> PING localhost (127.0.0.1) 56(84) bytes of data.
#> 64 bytes from localhost (127.0.0.1): icmp_seq=1 ttl=64 time=0.010 ms
#> 64 bytes from localhost (127.0.0.1): icmp_seq=2 ttl=64 time=0.026 ms
#> 64 bytes from localhost (127.0.0.1): icmp_seq=3 ttl=64 time=0.012 ms
#> 64 bytes from localhost (127.0.0.1): icmp_seq=4 ttl=64 time=0.011 ms
#> 64 bytes from localhost (127.0.0.1): icmp_seq=5 ttl=64 time=0.011 ms
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
#> Running command (2025-03-31 09:41:50): ping localhost
#> 
#> PING localhost (127.0.0.1) 56(84) bytes of data.
#> 64 bytes from localhost (127.0.0.1): icmp_seq=1 ttl=64 time=0.007 ms
#> 64 bytes from localhost (127.0.0.1): icmp_seq=2 ttl=64 time=0.017 ms
#> 64 bytes from localhost (127.0.0.1): icmp_seq=3 ttl=64 time=0.013 ms
#> 64 bytes from localhost (127.0.0.1): icmp_seq=4 ttl=64 time=0.014 ms
#> 64 bytes from localhost (127.0.0.1): icmp_seq=5 ttl=64 time=0.013 ms
#> Command process finished
#> Warning: System command timed out in 5 secs (status: -9)
file.exists(file) # file remain exist as timeout means command failed
#> [1] TRUE
file.remove(file)
#> [1] TRUE
```

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
#> Running command (2025-03-31 09:41:55): /usr/bin/python3 --help
#> 
#> usage: /usr/bin/python3 [option] ... [-c cmd | -m mod | file | -] [arg] ...
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
#> Running command (2025-03-31 09:41:55): /usr/bin/perl --help
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
#> Running command (2025-03-31 09:41:55): gzip -c
#> /tmp/RtmpNwcuWW/filed4a221f61373a | gzip -d > /tmp/RtmpNwcuWW/filed4a22730790
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
#> Running command (2025-03-31 09:41:55): /usr/bin/ping 8.8.8.8
#> 
#> PING 8.8.8.8 (8.8.8.8) 56(84) bytes of data.
#> 64 bytes from 8.8.8.8: icmp_seq=1 ttl=106 time=45.7 ms
#> 64 bytes from 8.8.8.8: icmp_seq=2 ttl=106 time=45.0 ms
#> 64 bytes from 8.8.8.8: icmp_seq=3 ttl=106 time=51.4 ms
#> 64 bytes from 8.8.8.8: icmp_seq=4 ttl=106 time=46.3 ms
#> 64 bytes from 8.8.8.8: icmp_seq=5 ttl=106 time=47.2 ms
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
#> R version 4.4.2 (2024-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.1 LTS
#> 
#> Matrix products: default
#> BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/libmkl_rt.so;  LAPACK version 3.8.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: Asia/Shanghai
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] blit_0.2.0.9000
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.37      R6_2.5.1           fastmap_1.2.0      xfun_0.49         
#>  [5] knitr_1.49         parallel_4.4.2     htmltools_0.5.8.1  rmarkdown_2.29    
#>  [9] ps_1.8.1           cli_3.6.3          processx_3.8.6     data.table_1.16.99
#> [13] compiler_4.4.2     tools_4.4.2        evaluate_1.0.1     yaml_2.3.10       
#> [17] rlang_1.1.4
```
