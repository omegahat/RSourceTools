tocRDA("inst/sampleData/degenerateFunction.rda", FALSE)

#########################
#
# Manual
#
con = gzfile("inst/sampleData/degenerateFunction.rda", "rb")

readHeader(con)
getFlags(con) # 2
readTag(con)

# Read the  CLOSXP flag
tp = getFlags(con)

readAttributes(con)
getFlags(253)  # environment

getFlags(con)

##########################
readItem(con, tp)
