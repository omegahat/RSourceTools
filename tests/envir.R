f = function() x
g = function() y
environment(f) = environment(g) = e = new.env()
e$x = 1
e$y = 1
z = 1:10

save(f, g, e, z, file = "~/Projects/org/omegahat/R/RTools/inst/sampleData/sharedEnv.rda")

#################################

f = function() x
g = function() y
environment(f) = environment(g) = e = new.env(hash = TRUE)
e$x = 1
e$y = 1

save(f, g, z, file = "~/Projects/org/omegahat/R/RTools/inst/sampleData/sharedHashEnv.rda")


f = function() x
g = function() y
top = new.env()
environment(f) = environment(g) = e = new.env(hash = TRUE, parent = top)
e$x = 1
e$y = 1

save(f, g, z, file = "~/Projects/org/omegahat/R/RTools/inst/sampleData/sharedNestedEnv.rda")


