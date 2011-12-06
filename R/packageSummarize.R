foo =
function() {  
tars = list.files(".", pattern = "\\.tar\\.gz$")
pkgs = sub("_[0-9]+\\.[0-9]+(-[0-9]+(\\.[0-9]+)?)?\\.tar\\.gz$", "", tars)

pkgs = gsub("_.*$", "", tars)

getCodeFiles = 
function(dir, subdir = "R", pattern = "\\.[RSrs]$") {
                          r.dir = paste(dir, subdir, sep = .Platform$file.sep)
                          if(file.exists(r.dir))
                             list.files(r.dir, pattern = pattern)
                          else
                             character()
                       }

rfiles = lapply(pkgs, getCodeFiles)    

numRFiles = sapply(rfiles, length)
names(numRFiles) = pkgs
table(numRFiles)

cfiles = lapply(pkgs, getCodeFiles, "src", "\\.(c|f|cc|cxx)$")    
numNativeFiles = sapply(cfiles, length)
names(numNativeFiles) = pkgs
which.max(numNativeFiles)

i = numNativeFiles > 0
cor(numRFiles[i], numNativeFiles[i])

plot(numRFiles[i], numNativeFiles[i])


r.code.info =
 lapply(paste(pkgs, "R", sep = .Platform$file.sep),
       function(dir) {
         lapply(list.files(dir, pattern = "\\.[RSsr]$", full.names = TRUE),
                 toc)
       })
}
