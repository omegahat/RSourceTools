getPackageObjects =
function(which)
{
  objs = objects(which)
  gens = getGenerics(which)
     #get the MethodsList
  methods = lapply(lapply(gens, function(x) getMethods(x, which)), function(x) names(x@methods))
  classes = getClasses(which)
  list(objects = objs, generics = gens, methods = methods, classes = classes)
}
