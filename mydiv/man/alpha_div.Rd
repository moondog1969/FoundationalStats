\name{alpha.div}
\alias{alpha.div}
\alias{plot.a_div}
\alias{print.a_div}

\title{
Calculate alpha-diversity
}
\description{
Calculates two types of \eqn{\alpha}-diversity: the Simpson index and and the Shannon Weiner index.
}
\usage{
alpha.div(x, method = "simpson")
}
\arguments{
  \item{x}{
Either a dataframe of taxa abudances in sites, with sites comprising rows and columns comprising taxa (rownames of the dataframe should give site names), or a numeric vector comprising taxa abundances for a single site.  
}
\item{method}{ One of \code{"simpson"} or \code{"shannon"}.  Partial matches allowed.
}
}
\value{
The output is a list with the following components:
\enumerate{
\item{\code{p.i} A dataframe containing proportional abundances of taxa in sites.}
\item{\code{method} The diversity method applied.}
\item{\code{rn} The rownames of the original community dataset (i.e., the site names).}
\item{\code{div} A dataframe of actual computed diversities.}
  }
}
\references{
Magurran, A. E. (1988). Ecological diversity and its measurement. Princeton university press.
}
\author{
Intro to R class
}
\examples{
taxa <- c(sp1 = 3, sp2 = 4, sp3 = 9)
x <- alpha.div(taxa)
plot(x)
plot(x, TRUE)
}