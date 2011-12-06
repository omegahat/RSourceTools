con = gzfile("inst/sampleData/mtcars.rda", 'rb')
readHeader(con)

getFlags(con)
readTag(con)

tp = getFlags(con)
x = readItem(con, tp)



############################################
readXDRInteger(con)

tp = getFlags(con)
readItem(con, tp)

tp = getFlags(con)
readItem(con, tp)
