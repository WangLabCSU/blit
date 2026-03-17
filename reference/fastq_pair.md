# FASTQ PAIR

Rewrite paired end fastq files to make sure that all reads have a mate
and to separate out singletons.

Usually when you get paired end read files you have two files with a /1
sequence in one and a /2 sequence in the other (or a /f and /r or just
two reads with the same ID). However, often when working with files from
a third party source (e.g. the SRA) there are different numbers of reads
in each file (because some reads fail QC). Spades, bowtie2 and other
tools break because they demand paired end files have the same number of
reads.

## Usage

``` r
fastq_pair(
  fq1,
  fq2,
  ...,
  hash_table_size = NULL,
  max_hash_table_size = NULL,
  fastq_pair = NULL
)

fastq_read_pair(fastq_files)
```

## Arguments

- fq1, fq2:

  A string of fastq file path.

- ...:

  \<[dynamic dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Additional arguments passed to `fastq_pair` command. Empty arguments
  are automatically trimmed. If a single argument, such as a file path,
  contains spaces, it must be quoted, for example using
  [`shQuote()`](https://rdrr.io/r/base/shQuote.html). Details see:
  `cmd_help(fastq_pair())`.

- hash_table_size:

  Size of hash table to use.

- max_hash_table_size:

  Maximal hash table size to use.

- fastq_pair:

  A string of path to `fastq_pair` command.

- fastq_files:

  A character of the fastq file paths.

## Value

A `command` object.

## See also

- <https://github.com/linsalrob/fastq-pair>

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
