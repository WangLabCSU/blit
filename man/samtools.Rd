% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cmd-samtools.R
\name{samtools}
\alias{samtools}
\title{Python is a programming language that lets you work quickly and integrate
systems more effectively.}
\usage{
samtools(subcmd = NULL, ..., samtools = NULL)
}
\arguments{
\item{subcmd}{Sub-Command of samtools. Details see: \code{cmd_help(samtools())}.}

\item{...}{<\link[rlang:dyn-dots]{dynamic dots}> Additional arguments passed to \code{samtools} command. Empty arguments are automatically trimmed. If a single argument, such as a file path, contains spaces, it must be quoted, for example using \code{\link[=shQuote]{shQuote()}}. Details see: \code{cmd_help(samtools())}.}

\item{samtools}{A string of path to \code{samtools} command.}
}
\value{
A \code{command} object.
}
\description{
Python is a programming language that lets you work quickly and integrate
systems more effectively.
}
\seealso{
\itemize{
\item \url{https://www.htslib.org/}
\item \code{\link[=cmd_wd]{cmd_wd()}}/\code{\link[=cmd_envvar]{cmd_envvar()}}/\code{\link[=cmd_envpath]{cmd_envpath()}}/\code{\link[=cmd_on_exit]{cmd_on_exit()}}
\item \code{\link[=cmd_run]{cmd_run()}}/\code{\link[=cmd_help]{cmd_help()}}/\code{\link[=cmd_background]{cmd_background()}}
}

Other \code{commands}:
\code{\link{allele_counter}()},
\code{\link{cellranger}()},
\code{\link{conda}()},
\code{\link{fastq_pair}()},
\code{\link{gistic2}()},
\code{\link{kraken2}()},
\code{\link{kraken_tools}()},
\code{\link{perl}()},
\code{\link{pyscenic}()},
\code{\link{python}()},
\code{\link{seqkit}()},
\code{\link{trust4}()}
}
\concept{command}
