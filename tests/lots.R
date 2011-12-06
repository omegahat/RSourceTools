save(foo, x, z, degenerate, y, file = "inst/sampleData/lots.rda")

# "foo"        "x"          "z"          "degenerate" "y"         

tocRDA("inst/sampleData/lots.rda")

######################
con = gzfile("inst/sampleData/lots.rda", 'rb')
readHeader(con)

getFlags(con)
readTag(con)

tp = getFlags(con)
readItem(con, tp)





