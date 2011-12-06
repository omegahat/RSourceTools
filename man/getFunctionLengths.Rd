\name{getFunctionLengths}
\Rdversion{1.1}
\alias{getFunctionLengths}
\title{
Compute the lengths of functions in a package's environment or source file.
}
\description{
 This function computes a measure of the "complexity"
 or length of each of the functions
}
\usage{
getFunctionLengths(pkg, 
                   isPackage = is.integer(pkg) || pkg \%in\% c(search(), gsub("package:", "", search())),
                   op = function(fun) length(body(fun)), ...)
}
\arguments{
  \item{pkg}{a character vector or environment object. If this is a character vector, it is treated
  either as the name of an R package in the search path, or the name of a source file containing function definitions.}
  \item{isPackage}{a logical value indicating whether the value of 
     \code{pkg} identifies an R  package on the search path or 
     the name of an R source file.
   }
  \item{op}{}
  \item{\dots}{}

}
\details{

}
\value{
A named integer vector giving the length of each of the functions.
}
\references{
}
\author{
Duncan Temple Lang
}

\seealso{

}
\examples{
  getFunctionLengths("package:RSourceTools")
  getFunctionLengths("RSourceTools")

  f = system.file("Rcode", "sampleCode.R", package = "RSourceTools")
  getFunctionLengths(f)

  getFunctionLengths("RSourceTools", op = functionLength)
}
\keyword{programming}

