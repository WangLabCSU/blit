# Running Kraken2

Kraken is a taxonomic sequence classifier that assigns taxonomic labels
to DNA sequences. Kraken examines the k-mers within a query sequence and
uses the information within those k-mers to query a database. That
database maps k-mers to the lowest common ancestor (LCA) of all genomes
known to contain a given k-mer.

## Usage

``` r
kraken2(
  reads,
  ...,
  ofile = "kraken_output.txt",
  report = "kraken_report.txt",
  classified_out = NULL,
  unclassified_out = NULL,
  odir = getwd(),
  kraken2 = NULL
)
```

## Arguments

- reads:

  A character vector of FASTQ files used as input to Kraken2. Can be one
  file (single-end) or two files (paired-end).

- ...:

  \<[dynamic dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Additional arguments passed to `kraken2` command. Empty arguments are
  automatically trimmed. If a single argument, such as a file path,
  contains spaces, it must be quoted, for example using
  [`shQuote()`](https://rdrr.io/r/base/shQuote.html). Details see:
  `cmd_help(kraken2())`.

- ofile:

  A string of path to save kraken2 output.

- report:

  A string of path to save kraken2 report.

- classified_out:

  A string of path to save classified sequences, which should be a fastq
  file.

- unclassified_out:

  A string of path to save unclassified sequences, which should be a
  fastq file.

- odir:

  A string of path to the output directory.

- kraken2:

  A string of path to `kraken2` command.

## Value

A `command` object.

## See also

- <https://github.com/DerrickWood/kraken2/wiki/Manual>

- <https://benlangmead.github.io/aws-indexes/k2>

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
