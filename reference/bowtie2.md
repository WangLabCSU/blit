# Run bowtie2

Bowtie 2 is an ultrafast and memory-efficient tool for aligning
sequencing reads to long reference sequences. It is particularly good at
aligning reads of about 50 up to 100s or 1,000s of characters, and
particularly good at aligning to relatively long (e.g. mammalian)
genomes. Bowtie 2 indexes the genome with an FM Index to keep its memory
footprint small: for the human genome, its memory footprint is typically
around 3.2 GB. Bowtie 2 supports gapped, local, and paired-end alignment
modes.

## Usage

``` r
bowtie2(index, reads, ofile, ..., bowtie2 = NULL)
```

## Arguments

- index:

  Path to bowtie2 index prefix (without file extensions).

- reads:

  A character vector of FASTQ files used as input to bowtie2.

- ofile:

  A string of path to the output sam file.

- ...:

  \<[dynamic dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Additional arguments passed to `bowtie2` command. Empty arguments are
  automatically trimmed. If a single argument, such as a file path,
  contains spaces, it must be quoted, for example using
  [`shQuote()`](https://rdrr.io/r/base/shQuote.html). Details see:
  `cmd_help(bowtie2())`.

- bowtie2:

  A string of path to `bowtie2` command.

## Value

A `command` object.

## See also

- <https://bowtie-bio.sourceforge.net/bowtie2/index.shtml>

- [`cmd_wd()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_envvar()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_envpath()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)/[`cmd_condaenv()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)

- [`cmd_on_start()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)/[`cmd_on_exit()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)

- [`cmd_on_succeed()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)/[`cmd_on_fail()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)

- [`cmd_parallel()`](https://wanglabcsu.github.io/blit/reference/cmd_parallel.md)

Other `commands`:
[`allele_counter()`](https://wanglabcsu.github.io/blit/reference/allele_counter.md),
[`bcftools()`](https://wanglabcsu.github.io/blit/reference/bcftools.md),
[`bedtools()`](https://wanglabcsu.github.io/blit/reference/bedtools.md),
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
[`snpEff()`](https://wanglabcsu.github.io/blit/reference/snpEff.md),
[`strelka()`](https://wanglabcsu.github.io/blit/reference/strelka.md),
[`trust4()`](https://wanglabcsu.github.io/blit/reference/trust4.md),
[`varscan()`](https://wanglabcsu.github.io/blit/reference/varscan.md)
