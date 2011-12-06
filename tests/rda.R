if(!file.exists("../inst/sampleData/triple.rda")) {
}  
con = gzfile("../inst/sampleData/triple.rda", "rb")
vals = list()

# The integer vector
readHeader(con)

 # The  identifier for the DOTTED PAIR
readXDRInteger(con)

tmp = readTag(con)
if(FALSE) {
 tp = getFlags(con)
 tp["len"] = readXDRInteger(con)
 vals[[tmp]] = tp
 readVector(con, tp["type"], tp["len"])
} else
 vals[[tmp]] = readItem(con)

# The character vector
tt = getFlags(con)
tmp = readTag(con)
if(FALSE) {
   tp = getFlags(con)
   tp["len"] = readXDRInteger(con)
   vals[[tmp]] = tp
   readCharacterVector(con, len = tp["len"])
} else
   vals[[tmp]] = readItem(con)


#
tt = getFlags(con)
tmp = readTag(con)
if(FALSE) {
   tp = getFlags(con)
   tp["len"] = readXDRInteger(con)
   vals[[tmp]] = tp
   readVector(con, tp["type"], tp["len"])
} else
 vals[[tmp]] = readItem(con)

if(!isIncomplete(con))
  return(vals)
  
