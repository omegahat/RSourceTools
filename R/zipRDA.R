saveZIP =
function(..., list = character(0), file = stop("'file' must be specified"),
          ascii = FALSE, version = NULL, envir = parent.frame(), compress = !ascii, 
           eval.promises = TRUE, precheck = TRUE) 
{
  dir = tempdir()

  if(missing(list)) {
     els <- list(...)
     names(els) = as.character(substitute(list(...)))[-1]
  } else
     els = structure(lapply(list, get),  names = list)

  sapply(names(els), function(id) {
                       #serialize
                       save(els[[id]], file = paste(dir, id, sep = .Platform$file.sep))
                     })

  cur = getwd()
  setwd(dir)
  on.exit(setwd(cur))
  system(paste("zip", file, paste(names(els), collapse = " ")))
  file
}
