
con = gzfile("/tmp/foo.rda", "rb")
readHeader(con)

getFlags(con)
id = readTag(con)

getFlags(con)
structure(list(readCharacterVector(con)), names = id)

