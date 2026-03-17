# TRUST4: immune repertoire reconstruction from bulk and single-cell RNA-seq data

TRUST4: immune repertoire reconstruction from bulk and single-cell
RNA-seq data

## Usage

``` r
trust4(
  file1,
  ref_coordinate,
  ...,
  file2 = NULL,
  mode = NULL,
  ref_annot = NULL,
  ofile = NULL,
  odir = getwd(),
  trust4 = NULL
)

trust4_imgt_annot(
  species = "Homo_sapien",
  ...,
  ofile = "IMGT+C.fa",
  odir = getwd(),
  perl = NULL
)

trust4_gene_names(imgt_annot, ofile = "bcr_tcr_gene_name.txt", odir = getwd())
```

## Arguments

- file1:

  Path to bam file or fastq file.

- ref_coordinate:

  Path to the fasta file coordinate and sequence of V/D/J/C genes.

- ...:

  - `trust4`: \<[dynamic
    dots](https://rlang.r-lib.org/reference/dyn-dots.html)\> Additional
    arguments passed to `run-trust4` command. Empty arguments are
    automatically trimmed. If a single argument, such as a file path,
    contains spaces, it must be quoted, for example using
    [`shQuote()`](https://rdrr.io/r/base/shQuote.html). Details see:
    `cmd_help(run-trust4())`.

  - `trust4_imgt_annot`: \<[dynamic
    dots](https://rlang.r-lib.org/reference/dyn-dots.html)\> Additional
    arguments passed to `trust4_imgt_annot` command. Empty arguments are
    automatically trimmed. If a single argument, such as a file path,
    contains spaces, it must be quoted, for example using
    [`shQuote()`](https://rdrr.io/r/base/shQuote.html). Details see:
    `cmd_help(trust4_imgt_annot())`.

- file2:

  Path to the second paired-end read fastq file, only used for
  `mode = "fastq"`.

- mode:

  One of "bam" or "fastq". If `NULL`, will be inferred from `file1`.

- ref_annot:

  Path to detailed V/D/J/C gene reference file, such as from IMGT
  database. (default: not used). (recommended).

- ofile:

  - `trust4`: Prefix of output files. (default: inferred from file
    prefix).

  - `trust4_imgt_annot`: Output file name.

  - `trust4_gene_names`: Output file name.

- odir:

  A string of path to the output directory.

- trust4:

  A string of path to `run-trust4` command.

- species:

  Species to extract IMGT annotation, details see
  <https://www.imgt.org//download/V-QUEST/IMGT_V-QUEST_reference_directory/>.

- perl:

  A string of path to `perl` command.

- imgt_annot:

  Path of IMGT annotation file, created via `trust4_imgt_annot`.

## Value

A `command` object.

## See also

- <https://github.com/liulab-dfci/TRUST4>

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
[`snpEff()`](https://wanglabcsu.github.io/blit/reference/snpEff.md),
[`strelka()`](https://wanglabcsu.github.io/blit/reference/strelka.md),
[`varscan()`](https://wanglabcsu.github.io/blit/reference/varscan.md)
