# Changelog

## blit (development version)

- Rename `cmd_conda` to `cmd_codnaenv()`.

- new command `fastp`

- new command `varscan`

- new command `bcftools`

- new command `snpEff`

- new command `bowtie2`

- new command `bwa`

- new command `bedtools`

- new command `strelka`

- new command `minimap2`

## blit 0.2.0

CRAN release: 2025-03-29

### New features

- new function `cmd_conda` to define the `PATH` environment variables
  with conda environment.

- new function `appmamba` to install software and manage Environment
  with `micromamba`.

- new function `cmd_on_fail` to define the expression to be evaluated
  when the command failed.

- new function `cmd_on_succeed`to define the expression to be evaluated
  when the command succeeded.

- new function
  [`cmd_on_start()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)
  to define the expressions which will be run when command started

- new function
  [`cmd_on_exit()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)
  to define the expressions which will be run when command finished

- new command `samtools`

- new function
  [`cmd_parallel()`](https://wanglabcsu.github.io/blit/reference/cmd_parallel.md)
  to run multiple commands meanwhile

- use `processx` package to execute the command and remove the `sys` and
  `withr` package from dependencies

## blit 0.1.0

CRAN release: 2025-02-27

- Initial CRAN submission.
