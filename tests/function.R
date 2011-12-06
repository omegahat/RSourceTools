
con = gzfile("inst/sampleData/functions.rda", "rb")

readHeader(con)
getFlags(con) # 2
readTag(con)

# Read the  CLOSXP flag
tp = getFlags(con)
readItem(con, tp)


#######################
# 
getFlags(con)
readDottedPair(con, ignore = TRUE)



# The attributes end with a 254 and then a 253
# Then next we have a LISTSXP.
# This is the parameter list and is a dotted pair
#   a tag and a 251
#   LISTSXP


