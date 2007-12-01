load("Examples/clouds.rda") # can't require(HSAUR) really silently
formula <- rainfall ~ seeding *   (sne + cloudcover + prewetness + echomotion) + time
mf <- model.frame(formula, clouds)
class(mf)
names(attributes(mf))
