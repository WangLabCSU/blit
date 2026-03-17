# Package index

## R6 class

Core R6 class for encapsulating command parameters and building command
objects.

- [`Command`](https://wanglabcsu.github.io/blit/reference/Command.md) :
  R6 Class to prepare command parameters.

## Execute command

Functions for executing commands, including foreground, background, and
parallel execution.

- [`exec()`](https://wanglabcsu.github.io/blit/reference/exec.md) :
  Invoke a System Command
- [`cmd_run()`](https://wanglabcsu.github.io/blit/reference/cmd_run.md)
  [`cmd_help()`](https://wanglabcsu.github.io/blit/reference/cmd_run.md)
  [`cmd_background()`](https://wanglabcsu.github.io/blit/reference/cmd_run.md)
  : Execute command
- [`cmd_parallel()`](https://wanglabcsu.github.io/blit/reference/cmd_parallel.md)
  : Execute a list of commands

## Environment context

Set up the execution context, such as working directory, environment
variables, and dependency management.

- [`cmd_wd()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)
  [`cmd_envvar()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)
  [`cmd_envpath()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)
  [`cmd_condaenv()`](https://wanglabcsu.github.io/blit/reference/cmd_wd.md)
  : Setup the context for the command

- [`conda()`](https://wanglabcsu.github.io/blit/reference/conda.md) :
  Run conda

- [`appmamba()`](https://wanglabcsu.github.io/blit/reference/appmamba.md)
  [`install_appmamba()`](https://wanglabcsu.github.io/blit/reference/appmamba.md)
  [`uninstall_appmamba()`](https://wanglabcsu.github.io/blit/reference/appmamba.md)
  [`appmamba_rc()`](https://wanglabcsu.github.io/blit/reference/appmamba.md)
  :

  Manage Environment with `micromamba`

- [`cmd_conda()`](https://wanglabcsu.github.io/blit/reference/cmd_conda.md)
  **\[deprecated\]** :

  Set `conda-like` environment prefix to the `PATH` environment
  variables

## Schedule expressions

Define hooks to run before, after, or on success/failure of a command.

- [`cmd_on_start()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)
  [`cmd_on_exit()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)
  [`cmd_on_fail()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)
  [`cmd_on_succeed()`](https://wanglabcsu.github.io/blit/reference/cmd_on_start.md)
  : Schedule expressions to run

## Genomic tools

Wrappers for common genomic data processing and analysis tools.

- [`allele_counter()`](https://wanglabcsu.github.io/blit/reference/allele_counter.md)
  : Run alleleCount
- [`bcftools()`](https://wanglabcsu.github.io/blit/reference/bcftools.md)
  : BCFtools is a program for variant calling and manipulating files in
  the Variant Call Format (VCF) and its binary counterpart BCF. All
  commands work transparently with both VCFs and BCFs, both uncompressed
  and BGZF-compressed.
- [`bedtools()`](https://wanglabcsu.github.io/blit/reference/bedtools.md)
  : Run bedtools
- [`bowtie2()`](https://wanglabcsu.github.io/blit/reference/bowtie2.md)
  : Run bowtie2
- [`bwa()`](https://wanglabcsu.github.io/blit/reference/bwa.md) : Run
  BWA
- [`samtools()`](https://wanglabcsu.github.io/blit/reference/samtools.md)
  : Python is a programming language that lets you work quickly and
  integrate systems more effectively.
- [`seqkit()`](https://wanglabcsu.github.io/blit/reference/seqkit.md) :
  Run seqkit
- [`varscan()`](https://wanglabcsu.github.io/blit/reference/varscan.md)
  : VarScan is a platform-independent software tool developed at the
  Genome Institute at Washington University to detect variants in NGS
  data.
- [`snpEff()`](https://wanglabcsu.github.io/blit/reference/snpEff.md) :
  Genetic variant annotation, and functional effect prediction toolbox.
  It annotates and predicts the effects of genetic variants on genes and
  proteins (such as amino acid changes).
- [`gistic2()`](https://wanglabcsu.github.io/blit/reference/gistic2.md)
  : Run GISTIC2
- [`strelka()`](https://wanglabcsu.github.io/blit/reference/strelka.md)
  : Run strelka
- [`minimap2()`](https://wanglabcsu.github.io/blit/reference/minimap2.md)
  : Run minimap2
- [`freebayes()`](https://wanglabcsu.github.io/blit/reference/freebayes.md)
  : Run freebayes

## Transcriptomic tools

Wrappers for transcriptome sequencing data processing and analysis
tools.

- [`cellranger()`](https://wanglabcsu.github.io/blit/reference/cellranger.md)
  : Run cellranger
- [`fastp()`](https://wanglabcsu.github.io/blit/reference/fastp.md) :
  Run fastp
- [`fastq_pair()`](https://wanglabcsu.github.io/blit/reference/fastq_pair.md)
  [`fastq_read_pair()`](https://wanglabcsu.github.io/blit/reference/fastq_pair.md)
  : FASTQ PAIR
- [`kraken2()`](https://wanglabcsu.github.io/blit/reference/kraken2.md)
  : Running Kraken2
- [`kraken_tools()`](https://wanglabcsu.github.io/blit/reference/kraken_tools.md)
  : KrakenTools is a suite of scripts to be used alongside the Kraken,
  KrakenUniq, Kraken 2, or Bracken programs.
- [`trust4()`](https://wanglabcsu.github.io/blit/reference/trust4.md)
  [`trust4_imgt_annot()`](https://wanglabcsu.github.io/blit/reference/trust4.md)
  [`trust4_gene_names()`](https://wanglabcsu.github.io/blit/reference/trust4.md)
  : TRUST4: immune repertoire reconstruction from bulk and single-cell
  RNA-seq data
- [`pyscenic()`](https://wanglabcsu.github.io/blit/reference/pyscenic.md)
  : Run pyscenic

## Development

Developer utilities for extending functionality and creating new command
interfaces.

- [`make_command()`](https://wanglabcsu.github.io/blit/reference/make_command.md)
  : Helper function to create new command.
- [`arg()`](https://wanglabcsu.github.io/blit/reference/arg.md)
  [`arg0()`](https://wanglabcsu.github.io/blit/reference/arg.md) :
  Deliver arguments of command

## Scripting interfaces

Interfaces to external scripting languages for seamless integration with
existing scripts.

- [`python()`](https://wanglabcsu.github.io/blit/reference/python.md) :
  Python is a programming language that lets you work quickly and
  integrate systems more effectively.
- [`perl()`](https://wanglabcsu.github.io/blit/reference/perl.md) : Perl
  is a highly capable, feature-rich programming language with over 36
  years of development.
