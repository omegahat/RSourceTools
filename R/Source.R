DefaultSourceDir = "../../../R/"

Source = function(..., dir = getOption("DefaultSourceDir"), file = unlist(list(...)))
{
  if(missing(dir) && is.null(dir))
     dir = DefaultSourceDir
  
  if(length(file) > 1)
    return(sapply(file, Source))

  if(!file.exists(file)) {

    tmp = expand.grid(dir, file, c(".R", ".S", ""))
    f = apply(tmp, 1, function(x) sprintf("%s%s%s%s", x[1], .Platform$file.sep, x[2], x[3]))
  
#    f = paste(dir, file, c(".R", ".S", ""), sep = "")
    e = file.exists(f)

    if(!any(e)) {
      names = list.files(dir, pattern = "[^~]$")
      e = agrep(file, names, val = TRUE)
      if(length(e) > 1)
        e = grep(paste("^", file, sep =""), e, val = TRUE)
      if(length(e) == 1){
        f = paste(dir, e, sep = "")
        source(f)
        return(f)
      }
      else if(length(e) > 1)
        stop("one of ", paste(e, collapse = ", "))
      e = FALSE
    }
  } else {
     e = 1
     f = file
 }

  if(!any(e)) {
    poss = agrep(file,  list.files(dir, pattern = "[^~]$"), val = TRUE)
    stop("cannot match ", file, ". Possibly ", paste(poss, collapse = ", "))
  }
  source(f[e][1])
  f[e][1]
}
