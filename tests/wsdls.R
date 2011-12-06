con = gzfile("/Users/duncan/Projects/org/omegahat/XML/SOAP/WSDLs/.RData", "rb")
readHeader(con)
getFlags(con)
readTag(con)
getFlags(con)
readXDRInteger(con)
tp = getFlags(con)

readDottedPair(con, verbose = TRUE)
