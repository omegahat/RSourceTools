f =
function(x)
{
  x + 1
}

constants = c(1, 2, 3L)

integer.constants = c(1L, 2L, 3L)

mixed.constants = c("1", 2L, 3L, TRUE)
mixed.constants = c(1, 2L, 3L, TRUE)

mixed.constants = c(1+0i, 2, 3L, TRUE)


Names = c("a", "b")

Flags = c( a = TRUE, b = FALSE, c = FALSE)

FunTable =
   c(a = function() {},
     b = function(...) {})

f =
  # Duplicate
function(y)
{
   y ^ 2
}



