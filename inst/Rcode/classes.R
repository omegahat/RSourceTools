
setClass("Foo", contains = "integer")
setOldClass("Old")
setOldClass(c("Younger", "Old"))

setGeneric("foo", function(x, y) standardGeneric("foo"))
setMethod("foo", c("A", "B"), function(x, y)  x + y)
setMethod("foo", "A", function(x, y)  x + y)
setMethod("foo", c("B"), function(x, y)  x + y)
