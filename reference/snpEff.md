# Genetic variant annotation, and functional effect prediction toolbox. It annotates and predicts the effects of genetic variants on genes and proteins (such as amino acid changes).

Genetic variant annotation, and functional effect prediction toolbox. It
annotates and predicts the effects of genetic variants on genes and
proteins (such as amino acid changes).

## Usage

``` r
snpEff(subcmd = NULL, ..., snpEff = NULL)
```

## Arguments

- subcmd:

  Sub-Command of snpEff. Details see: `cmd_help(snpEff())`.

- ...:

  \<[dynamic dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Additional arguments passed to `snpEff` command. Empty arguments are
  automatically trimmed. If a single argument, such as a file path,
  contains spaces, it must be quoted, for example using
  [`shQuote()`](https://rdrr.io/r/base/shQuote.html). Details see:
  `cmd_help(snpEff())`.

- snpEff:

  A string of path to `snpEff` command.

## Value

A `command` object.

## See also

- <https://pcingola.github.io/SnpEff/>

- [`cmd_wd()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_envvar()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_envpath()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_condaenv()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)

- [`cmd_on_start()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)/[`cmd_on_exit()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)

- [`cmd_on_succeed()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)/[`cmd_on_fail()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)

- [`cmd_parallel()`](https://wanglabcsu.github.io/blit/reference/cmd_parallel.md)

Other `commands`:
[`allele_counter()`](https://wanglabcsu.github.io/blit/reference/allele_counter.md),
[`bcftools()`](https://wanglabcsu.github.io/blit/reference/bcftools.md),
[`bedtools()`](https://wanglabcsu.github.io/blit/reference/bedtools.md),
[`bowtie2()`](https://wanglabcsu.github.io/blit/reference/bowtie2.md),
[`bwa()`](https://wanglabcsu.github.io/blit/reference/bwa.md),
[`cellranger()`](https://wanglabcsu.github.io/blit/reference/cellranger.md),
[`conda()`](https://wanglabcsu.github.io/blit/reference/conda.md),
[`fastp()`](https://wanglabcsu.github.io/blit/reference/fastp.md),
[`fastq_pair()`](https://wanglabcsu.github.io/blit/reference/fastq_pair.md),
[`freebayes()`](https://wanglabcsu.github.io/blit/reference/freebayes.md),
[`gistic2()`](https://wanglabcsu.github.io/blit/reference/gistic2.md),
[`kraken2()`](https://wanglabcsu.github.io/blit/reference/kraken2.md),
[`kraken_tools()`](https://wanglabcsu.github.io/blit/reference/kraken_tools.md),
[`minimap2()`](https://wanglabcsu.github.io/blit/reference/minimap2.md),
[`perl()`](https://wanglabcsu.github.io/blit/reference/perl.md),
[`pyscenic()`](https://wanglabcsu.github.io/blit/reference/pyscenic.md),
[`python()`](https://wanglabcsu.github.io/blit/reference/python.md),
[`samtools()`](https://wanglabcsu.github.io/blit/reference/samtools.md),
[`seqkit()`](https://wanglabcsu.github.io/blit/reference/seqkit.md),
[`strelka()`](https://wanglabcsu.github.io/blit/reference/strelka.md),
[`trust4()`](https://wanglabcsu.github.io/blit/reference/trust4.md),
[`varscan()`](https://wanglabcsu.github.io/blit/reference/varscan.md)
