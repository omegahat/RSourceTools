setClass("ABC", representation(foo = "character",
                               bar = "integer"))

setClass("DEF", representation(x = "character",
                               y = "numeric",
                               z = "ABC"))

xyz = new("ABC", foo = "a string", bar = 1:10)

fff = new("DEF", x = "another string", y = c(1, 2, 3), z = xyz)
save(xyz, fff, file = system.file("sampleData", "S4objects.rda", package = "RSourceTools"))


################################

con = gzfile("inst/sampleData/S4class.rda", 'rb')
readHeader(con)
getFlags(con)
readTag(con)

readItem(con)
