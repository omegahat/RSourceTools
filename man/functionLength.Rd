\name{functionLength}
\Rdversion{1.1}
\alias{functionLength}
\title{Compute the length or "complexity" of a function}
\description{
 This function computes the "length" of the function.
 This can be the number of top-level expressions
 in the body or the total number of expressions computed
 recursively on all of the body.

 When working recursively, we can get  the total number of expressions or
 the number for each of the top-level expressions separately as a vector.
}
\usage{
functionLength(e, sum = TRUE, recursive = TRUE)
}
\arguments{
  \item{e}{the function object whose length is of interest. 
   This can also be the name of the function which is then used via \code{\link[base]{get}} to fetch
   the function object. Alternatively, this can be an expression which defines a function.
   This is useful when working with functions in source files. }
  \item{sum}{a logical value indicating whether to cumulate the 
     results from the individual top-level expressions. This is only 
     used when \code{recursive} is \code{TRUE}.}
  \item{recursive}{a logical value controlling whether we 
   return the number of top-level expressions (\code{FALSE})
   or recursively examine each of these top-level expressions and compute the number of 
   sub-expressions they contain, and those sub-expressions contain and so on.
  }
}
\value{
 An integer vector.
 If \code{recursive} is \code{FALSE},  this is a single number
 giving the number of top-level expressions in the body.

 If \code{recursive} is \code{TRUE} and \code{sum} is \code{TRUE},
 this is the total number of sub-expressions at all levels in the body.
 If \code{sum} is \code{FALSE}, the resulting vector has as many elements
 as there are top-level expressions in the body. The value for each element is the number of
 recursive sub-expressions for the corresponding top-level expression.
}
\author{
Duncan Temple Lang
}

\seealso{
  \code{\link{getFunctionLengths}}
}
\examples{
 functionLength(functionLength)
 functionLength(functionLength, recursive = FALSE)
 functionLength(functionLength, FALSE)
}
\keyword{programming}
