\name{Source}
\Rdversion{1.1}
\alias{Source}
\title{An enhanced version of the source function}
\description{
  This version of the \code{\link[base]{source}} command
  attempts to provide several useful enhancements.
  Firstly, it is vectorized so one can pass it several
  files to read in a single call.
  Secondly, it can find files  outside of the current directory
  even if they are not actual file paths. To do this, it looks
  in one or more "commonly" used directories. This is useful when
  working with  source in a future package, for example, or project area
  but running the R session in a different directory.
  Thirdly, it can handle recursive calls to \code{Source} using
  relative file paths and names.  One can do this with
  the \code{chdir} argument of \code{\link[base]{source}} but the
  semantics and mechanism are different.
  
}
\usage{
Source(..., dir = getOption("DefaultSourceDir"), file = unlist(list(...)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{a collection of file names.  These can be explict path names or relative to dir.}
  \item{dir}{a character vector of directory names which is used to find files.}
  \item{file}{the collection of file names to source provided as a single list/vector}
}
\details{
}
\value{

}
\author{
Duncan Temple Lang
}
\seealso{
 \code{\link{source}}
}
\examples{
}
\keyword{programming}
