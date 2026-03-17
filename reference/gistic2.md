# Run GISTIC2

The GISTIC module identifies regions of the genome that are
significantly amplified or deleted across a set of samples. Each
aberration is assigned a G-score that considers the amplitude of the
aberration as well as the frequency of its occurrence across samples.
False Discovery Rate q-values are then calculated for the aberrant
regions, and regions with q-values below a user-defined threshold are
considered significant. For each significant region, a "peak region" is
identified, which is the part of the aberrant region with greatest
amplitude and frequency of alteration. In addition, a "wide peak" is
determined using a leave-one-out algorithm to allow for errors in the
boundaries in a single sample. The "wide peak" boundaries are more
robust for identifying the most likely gene targets in the region. Each
significantly aberrant region is also tested to determine whether it
results primarily from broad events (longer than half a chromosome arm),
focal events, or significant levels of both. The GISTIC module reports
the genomic locations and calculated q-values for the aberrant regions.
It identifies the samples that exhibit each significant amplification or
deletion, and it lists genes found in each "wide peak" region.

## Usage

``` r
gistic2(seg, refgene, ..., odir = getwd(), gistic2 = NULL)
```

## Arguments

- seg:

  A data.frame of segmented data.

- refgene:

  Path to reference genome data input file (REQUIRED, see below for file
  description).

- ...:

  \<[dynamic dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Additional arguments passed to `gistic2` command. Empty arguments are
  automatically trimmed. If a single argument, such as a file path,
  contains spaces, it must be quoted, for example using
  [`shQuote()`](https://rdrr.io/r/base/shQuote.html). Details see:
  `cmd_help(gistic2())`.

- odir:

  A string of path to the output directory.

- gistic2:

  A string of path to `gistic2` command.

## Value

A `command` object.

## See also

- <https://broadinstitute.github.io/gistic2/>

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
