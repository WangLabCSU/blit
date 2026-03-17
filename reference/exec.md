# Invoke a System Command

Invoke a System Command

## Usage

``` r
exec(cmd, ...)
```

## Arguments

- cmd:

  Command to be invoked, as a character string.

- ...:

  \<[dynamic dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Additional arguments passed to `cmd` command. Empty arguments are
  automatically trimmed. If a single argument, such as a file path,
  contains spaces, it must be quoted, for example using
  [`shQuote()`](https://rdrr.io/r/base/shQuote.html).

## Value

A `command` object.

## `command` collections

- [`allele_counter()`](https://wanglabcsu.github.io/blit/reference/allele_counter.md)

- [`bcftools()`](https://wanglabcsu.github.io/blit/reference/bcftools.md)

- [`bedtools()`](https://wanglabcsu.github.io/blit/reference/bedtools.md)

- [`bowtie2()`](https://wanglabcsu.github.io/blit/reference/bowtie2.md)

- [`bwa()`](https://wanglabcsu.github.io/blit/reference/bwa.md)

- [`cellranger()`](https://wanglabcsu.github.io/blit/reference/cellranger.md)

- [`conda()`](https://wanglabcsu.github.io/blit/reference/conda.md)

- [`fastp()`](https://wanglabcsu.github.io/blit/reference/fastp.md)

- [`fastq_pair()`](https://wanglabcsu.github.io/blit/reference/fastq_pair.md)

- [`freebayes()`](https://wanglabcsu.github.io/blit/reference/freebayes.md)

- [`gistic2()`](https://wanglabcsu.github.io/blit/reference/gistic2.md)

- [`kraken_tools()`](https://wanglabcsu.github.io/blit/reference/kraken_tools.md)

- [`kraken2()`](https://wanglabcsu.github.io/blit/reference/kraken2.md)

- [`minimap2()`](https://wanglabcsu.github.io/blit/reference/minimap2.md)

- [`perl()`](https://wanglabcsu.github.io/blit/reference/perl.md)

- [`pyscenic()`](https://wanglabcsu.github.io/blit/reference/pyscenic.md)

- [`python()`](https://wanglabcsu.github.io/blit/reference/python.md)

- [`samtools()`](https://wanglabcsu.github.io/blit/reference/samtools.md)

- [`seqkit()`](https://wanglabcsu.github.io/blit/reference/seqkit.md)

- [`snpEff()`](https://wanglabcsu.github.io/blit/reference/snpEff.md)

- [`strelka()`](https://wanglabcsu.github.io/blit/reference/strelka.md)

- [`trust4()`](https://wanglabcsu.github.io/blit/reference/trust4.md)

- [`varscan()`](https://wanglabcsu.github.io/blit/reference/varscan.md)

## See also

- [`cmd_wd()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_envvar()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_envpath()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_condaenv()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)

- [`cmd_on_start()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)/[`cmd_on_exit()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)

- [`cmd_on_succeed()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)/[`cmd_on_fail()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)

- [`cmd_parallel()`](https://wanglabcsu.github.io/blit/reference/cmd_parallel.md)

## Examples

``` r
cmd_run(exec("echo", "$PATH"))
#> Running command (2026-03-17 16:14:13): echo $PATH
#> 
#> Running scheduled exit task
#> Command process finished
#> System command succeed
```
