con = gzfile("inst/sampleData/triple.rda", 'rb')
readHeader(con)

replicate(3,  {
               getFlags(con)
               print(readTag(con))
               tp = getFlags(con)
               readItem(con, tp)
             })
