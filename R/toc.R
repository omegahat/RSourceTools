createParentEnv =
function(..., parent = NULL, file = "") # , env = new.env())
{
  if(is.null(parent)) {
      # We have to use parent$.env rather than env as the functions are defined just once and
      # bind to the env in that call, not the subsequent ones.
    parent = new.env()
    parent$setClass = function(name, ...) { assign(name, structure(name, class = "ClassDef"), envir = parent$.env) }
    parent$setGeneric = function(name, ...) { assign(name, structure(name, class = "GenericFunction"), parent$.env) }
    parent$setMethod = function(name, sig, ...) { assign(name, structure(list(name = name, sig = sig), class = "MethodDef"), parent$.env) }
    parent$setOldClass = function(name, ...) { assign(name[1], structure(name, class = "S3ClassDef"), parent$.env) }
    parent$library = function(name, ...) { assign(deparse(substitute(name)), structure(name, class = "Library"), parent$.env) }    

    args = list(...)
    for(i in names(args))
         assign(i, args[[i]], parent)

  }

  env = new.env(parent = parent)

  parent$.env = env      
  parent
}

toc =
function(file, getSize = TRUE, addFile = length(file) > 1, ..., ignore = character(), parent = NULL)
{
  info = file.info(file)
  if(info[1, "isdir"])  {
     file = list.files(file, pattern = ".*\\.[RSrsq]$", full.names = TRUE)
     if(length(ignore)) {
       i = grep(ignore, file)
       if(length(i))
         file = file[-i]
     }
  }

  if(length(file) > 1) {
    p = createParentEnv(..., parent = parent)
    ans = lapply(file, toc, getSize, addFile, parent = p)
    return(structure(do.call("rbind", ans), class = unique(c("RCodeTOC", class(ans[[1]])))))
  }

  ex = parse(file)  
  
  parent = createParentEnv(..., parent = parent, file = file)
  env = parent$.env
  parent$file = file
  eval(ex, env = env)

  ids = objects(env, all = TRUE)

  ans = data.frame(type = sapply(ids, function(x)
                                       class(get(x, envir = env, inherits = FALSE))),
                   row.names = ids)

  if(addFile)
    ans$file = rep(basename(file), nrow(ans))

  if(getSize)
     ans$size = sapply(ids, function(x) {
                              v = get(x, envir = env, inherits = FALSE)
                              if(is.function(v))
                                length(body(v))
                              else
                                length(v)
                            })

   structure(ans, class = c("RCodeTOC", class(ans)))
}

functions =
function(x, ...)
  UseMethod("functions")

functions.RCodeTOC =
function(x, ...)  
 subset(x, type == "function")

classDefs =
function(x,  types = c("ClassDef", "S3ClassDef"), ...)  
  UseMethod("classDefs")

classDefs.RCodeTOC =
function(x, types = c("ClassDef", "S3ClassDef"), ...)  
 subset(x, type %in% types)


summary.RCodeTOC =
function(object, ...)
{
  structure(
            list(types = table(object$type),
                 files = table(object$file)),
            class = "RCodeTOCSummary")
       
}

print.RCodeTOCSummary =
function(x, ...)
{
  print(unclass(x), ...)
}  

checkForDuplicates =
function(file)
{
 e =  parse(file)

   # expression not handled by split.
   #  tapply(e, sapply(e, class), function(x) {length(x)})
 
 tapply(1:length(e), sapply(e, class),
          function(idx) {
              x = breakDown(e, idx)
              if(is.list(x)) {
                lapply(x, getDuplicated)
              } else
                getDuplicated(x)
            })
}

getDuplicated =
function(x)
{  
  d = duplicated(x)
  x[which(d)]
}
 
breakDown = 
function(e, idx = seq(along = e)) {

           if(class(e[[idx[1]]]) %in% c("=", "<-")) {
             sapply(e[idx], function(x) as.character(x[[2]]))
           }
          else if(class(e[[idx[1]]]) == "call") {
            els = e[idx]
           
            tapply(1:length(idx), sapply(els, function(x) as.character(x[[1]])),
                    function(sub) {
                       type = as.character(els[[sub[1]]][[1]])
                       fun = switch(type,
                                      "library" = function(x) as.character(x[[2]]),
                                      "setGeneric" = function(x) as.character(x[[2]]),
                                      "setClass" = function(x) as.character(x[[2]]),                         
                                      "setAs" =  function(x) paste(as.character(x[[2]]), as.character(x[[3]]), sep = "->"),
                                      "makeActiveBinding" = function(x) as.character(x[[2]]),
                                      NULL) # want to compare the environments
                       if(!is.null(fun))
                         sapply(els[sub], fun)
                       else
                         character()
                     })
          }
         }


is.literal =
function(x, isExpression = TRUE)
{
   # Complex elements won't show up here in the parse
  ans = typeof(x) %in% c("double", "numeric", "integer", "logical", "complex", "character")
  if(ans)
    return(ans)

  if(is.symbol(x))
    return(TRUE)
  
  if(isExpression && is(x, "language")) {
     ans = any(sapply(x[-1], is.literal))
  }
  ans
}

isComplex =
function(expr)
  is(expr, "language") && any(sapply(expr, typeof) == "complex")

getVectorType =
function(els)
{
  if(all(sapply(els, is.literal))) {
     ans = unique(sapply(els, typeof))
     if(length(ans) == 1)
       ans
     else {
       if(any(ans == "character"))
         "character"
       else if(any(sapply(els, isComplex)))
         "complex"       
       else if(any(ans == "double"))
         "double"
       else if(any(ans == "integer"))
         "integer"
       else
          as.character(NA)
#       paste(ans, collapse = " | ")
     }
   } else
     "vector"
}

ptype =
function(e)
{
   if(is.literal(e, FALSE))
       typeof(e)
  else if(class(e) %in% c("<-", "=")  && (is.name(e[[2]]) || is.character(e[[2]]))) {
     if(is.call(e[[3]])) {
       if(as.character(e[[3]][[1]]) == "function")
          "function"
       else if(as.character(e[[3]][[1]]) == "c")
          getVectorType(e[[3]][-1])
       else if(as.character(e[[3]][[1]]) == "list")
         "list"
       else
         "?"
     } else if(is.literal(e[[3]]))
       typeof(e[[3]])
     else
        "?"
  } else {
        # Handle pkg::foo(a, b, c)
     what = if(is.call(e[[1]]) && as.character(e[[1]][[1]]) == "::")
               as.character(e[[1]][[3]])
            else
               as.character(e[[1]])
     switch(what,
               "setClass" = "ClassDef",
               "setOldClass" = "S3ClassDef",
               "setMethod" = "MethodDef",
               "setGeneric" = "GenericFunction",
               library = "Library", # deparse(e[[2]]),
               character(0) # as.character(e[[1]])
              )
   }
 }  

pname =
function(e) {
                   # see if it is a function
  if((class(e) == "<-" || class(e) == "=" ) && (is.name(e[[2]]) || is.character(e[[2]])))
     as.character(e[[2]]) 
  else if(is.call(e)) {
    what = if(is.call(e[[1]]) && as.character(e[[1]][[1]]) == "::")
               as.character(e[[1]][[3]])
            else
               as.character(e[[1]])
    switch(what,
            "setClass" = e[[2]],
            "setOldClass" = if(is.call(e[[2]])) e[[2]][[2]][1] else e[[2]][1],
            "setMethod" = paste(e[[2]], if(is.character(e[[3]])) e[[3]] else paste(e[[3]][-1], collapse = ","), sep = "-"),
            "setGeneric" = e[[2]],
            "library" = as.character(e[[2]]),
            character(0))
  } else
    character(0)      
}  

isFalse =
  # Find top-level expressions of the form if(FALSE)
  # which act to comment out code to stop it being evaluated.
function(e)
{
 (class(e) == "if" && is.logical(e[[2]]) && !e[[2]])
}

ptoc =
  #
  # addGenericNames controls whether an extra column is added
  # which has the name of the generic not the variable name
  # for an S3 method, i.e. turns kml.formula into kml because
  # there is a function named kml.
  #
function(file, as.data.frame = TRUE, addGenericNames = TRUE)
{
  info = file.info(file)
  if(info[1, "isdir"]) 
    file = list.files(file, pattern = ".*\\.[RSrsq]$", full.names = TRUE)

  if(length(file) > 1) {
    ans = lapply(file, ptoc, FALSE, FALSE)
    ans = data.frame(varName = unlist(lapply(ans, names)),
                      type = unlist(ans),
                      file = basename(rep(file, sapply(ans, length))))
    if(addGenericNames)
        ans$genericNames = identifyS3Methods(ans)
    return(ans)
  }
  
  exprs = parse(file)

  exprs = exprs[!sapply(exprs, isFalse)]
  
  type = unlist(lapply(exprs, ptype))
  
  ids = unlist(lapply(exprs, pname))

  if(as.data.frame) {
     ans = data.frame(varName = ids, types = type)
     if(addGenericNames) {
       ans$genericNames = identifyS3Methods(ans)
     }
     
     ans
  } else
     structure(type, names = ids)
}



identifyS3Methods =
  #
  #  toc = ptoc("R", TRUE)
  #  toc$functioNames = identifyS3Methods(toc)
  #
function(objs)
{
  if(nrow(objs) == 0)
     return(character())
  
  funcNames = as.character(objs$varName)
  i = grep("\\..*", funcNames)
  trunc = gsub("\\..*", "", funcNames[i])
  j = trunc %in% funcNames
  if(any(j))
       funcNames[i[j]] = trunc[j]


  i = !is.na(as.character(objs$type)) & as.character(objs$type) == "MethodDef"
  funcNames[i]= gsub("-.*", "", as.character(objs$varName[i]))
  
  funcNames
}
