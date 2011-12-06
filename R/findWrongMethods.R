findErroneousMethodDefs =
function(file, which = NA)
{
  e = parse(file)
  generics = e[sapply(e, function(x) is.call(x) && as.character(x[[1]]) == "setGeneric")]
  names(generics) = sapply(generics, function(x) x[[2]])
  if(is.na(which)) {
    if(length(generics) == 1)
      generic = generics[[1]]
    else
      stop("need to know which one")
  } else
    generic = generics[[which]]

    # Get the generic parameters.
  genericParams = generic[[3]][[2]]
  
  m = e[sapply(e, function(x) is.call(x) && as.character(x[[1]]) == "setMethod")]
  w = !sapply(m, function(x) all(names(x[[4]][[2]]) == names(genericParams)))
  m[w] 
}  
