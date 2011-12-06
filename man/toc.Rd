\name{ptoc}
\alias{ptoc}
\alias{toc}

\title{Table of contents for R source files}

\description{ These two functions process R source files and determine the
       top-level objects that are created by that code,
       i.e. the assignments, class and method definitions, etc.
       The intent is that these functions help us become familiar with the
       layout of code within an R package or a loose collection of files.
       This is helpful when revisting your own code or when trying to 
       get an overview of somebody else's code.
    
      The functions can take one or more files or a directory.  Directories are processed
      by finding all the files that have a .R, .S, .r, .s or .q extension and processing these
      individually.
    
     The two functions work quite differently.
     
     \code{toc}
    evaluates the R code in a "sandbox" and 
     look at the resulting objects. 
     Class, generic function and method  definitions  are also
     evaluated in the sandbox using alternate implementations of the
     regular functions.
     
     \code{ptoc}
    on the other hand
     just parses the R code.  It then examines the top-level expressions in each 
     file for assignment operations, calls to \code{\link{setClass}}, \code{\link{setGeneric}}
      and \code{\link{setMethod}}.
     This avoids lengthy computations or computations that could produce errors,
      e.g. if the files are processed out of order and there are dependencies.
      It also identifies calls to \code{\link{library}}.
      It attempts to determine the type of the value being assigned.
      It can determine functions easily and different types of literals.
      It ignores assignments to elements of an object or more specifically
      it discards assignment expressions where the left-hand-side is not a simple name.
      For example, class(f) = "bob" and
       f[1] = 2 are ignored as they change f
      and do not impact the location of the original definition.
    }

\usage{
ptoc(file, as.data.frame = TRUE, addGenericNames = TRUE)
toc(file, getSize = TRUE, addFile = length(file) > 1, ...,
                 ignore = character(), parent = NULL)
}

\arguments{\item{file}{
   a vector of file or directory names.  Directories are processed by 
  operating on all of the R source files within it as found by 
  the pattern "\\.[RSrsq]".  Directories are not processed recursively
  at present.
}
 \item{as.data.frame}{}
 \item{addGenericNames}{}
\item{getSize}{
    a logical value which determines whether the "length"
   of each object is reported in the result. For a function,
   this is the number of top-level expressions in the body.
}\item{addFile}{
     this controls whether the file name is added as a column to the 
     data frame. When processing multiple files, this is useful
     so that one can determine the location of the resulting variables.
     If one is looking at just one file, this information is probably
      superflous and distracting.
}
\item{ignore}{}
\item{parent}{}
\item{\dots}{
    This parmater allows the caller to add  R objects
    to the parent environment in which the R code is evaluate within
    
     \code{toc}
   .  This allows one to define 
    variables that control how the code is interepreted, e.g. 
   variables that would be determined and added to the code via a configuration 
   script.  One can also provide functions
   such as setClass, setGeneric and setMethod to control what is added to the environment.
   For example, if one wants to discard S4 methods from the table of contents, one
   could provide the argument setMethod = function(...){}.
}}

\value{
      If more than one source code file is processed, the result is a data frame.
      Otherwise, 
     \code{ptoc}
    returns a character vector giving the types
      of the definitions and the names identifying the definitions.
     }

\seealso{}

  \author{}
 
\examples{
   #  Processing a file
 #  This example illustrates the results for
 #  both toc and ptoc
 #  when applied to a single file.
          f = system.file("Rcode", "sampleCode.R", package = "RTools") 
          tt.p = ptoc(f)

          tt = toc(f)
	 #  Directories
 #  This illustrates how the functions can process a directory.
 #  The result when there is more than one file in the directory
 #  is a data frame.
	 dir = system.file("Rcode", package = "RTools")
         toc(dir)
         ptoc(dir)
        #  Ignoring S4 code
 #  In this example, we discard S4 class definitions,
 #  generic functions and methods.
 #  We do this by providing implementations of the
 #  setClass,
 #  setGeneric and
 #  setMethod
 #  functions that are used to evaluate the code.
 #  These just do nothing.
          f = system.file("Rcode", "sampleCode.R", package = "RTools") 
          toc(f, setClass = function(...){}, 
                  setMethod = function(...) {},
                  setGeneric = function(...) {})
	
}

\keyword{programming}
