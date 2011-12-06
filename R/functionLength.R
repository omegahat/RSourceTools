getFunctionLengths =
  #
  # Given a package, get all the functions and compute the lengths of the body, i.e. number of expressions.
  # This is a simple flat measurement that does not measure the complexity of the expressions themselves,
  # e.g. if statements with bodies that are long.  This is relativel easy to do, just not done yet.
  #
function(pkg, isPackage = is.integer(pkg) || pkg %in% c(search(), gsub("package:", "", search())),
          op = function(fun) length(body(fun)), ...)
{
   if(isPackage && is.character(pkg)) {
        i = which(pkg == search())
        if(!any(i))
          i = which(pkg == gsub("package:", "", search()))
        pkg = search()[i]
   }

   if(isPackage) {
      funs = sapply(objects(pkg), function(x) { v = get(x, pkg);  is.function(v) })
      sapply(names(funs)[funs], function(x) { v = get(x, pkg);  op(v, ...) })
   } else {
      info = file.info(pkg)
        # If this is a directory, look at all the R code files in that directory
      if(info[1, "isdir"]) {
          files = list.files(pkg, pattern = "\\.[RrSsq]", full.names = TRUE)
          funs = unlist(lapply(files, findFunctionsInFile), recursive = FALSE)
      } else
          funs = findFunctionsInFile(pkg)
      sapply(funs, function(x)  op(eval(x), ...))
   }
}

findFunctionsInFile =
function(file, toplevel = FALSE)
{
  exprs = parse(file)

  exprs = exprs[!sapply(exprs, isFalse)]
  
  type = unlist(lapply(exprs, ptype))
  exprs = exprs[type == "function"]
  if(toplevel)
     exprs
  else {
    ids = unlist(lapply(exprs, pname))  
    structure(lapply(exprs, function(x) x[[3]]), names = ids)
  }
}


functionLength =
  #
  # functionLength(functionLength)
  # functionLength(functionLength, recursive = FALSE)
  # functionLength(functionLength, FALSE)
  #
function(e, sum = TRUE, recursive = TRUE)
{
  if(is.function(e))
    bdy = body(e)
  else if(is.call(e) && as.character(e[[1]]) == "function")
    bdy = e[[3]]
  else if(is.character(e))
    e = body(get(e, mode = "function"))
  else 
     stop("Not sure how to find length of function from ", class(e))

  if(!recursive)
    return(length(bdy))
  
  ans = if(as.character(bdy[[1]]) != "{") {
           getExpressionLength(bdy[[1]])
        } else
           sapply(bdy, getExpressionLength)

  if(sum)
     sum(ans)
  else
    ans
}


isBuiltinLiteral =
function(x)  
  typeof(x) %in% c("double", "numeric", "integer", "logical", "complex", "character")  

getExpressionLength =
  # Get the length of an expression, working recursively if we want.
function(x, recursive = TRUE)
{
  if(recursive) {
     tmp = if(length(x) > 1 && (is.call(x) || is(x, "if") || is(x, "while")))
             sapply(structure(x[-1], class = "list"), getExpressionLength)
           else
             0
   } else
     tmp = 0
  
   sum(tmp) + if((is.symbol(x) || is.name(x) || isBuiltinLiteral(x))) length(x) else 0
}
