library(RNetLogo)
nlDir = "D:\\programming\\NetLogo\\installations\\NetLogo 5.1.0"
setwd(nlDir)

nl.path = getwd()
NLStart(nl.path)

model.path = file.path("models", "Sample Models", "Earth Science", "Fire.nlogo")
NLLoadModel(file.path(nl.path, model.path))

NLCommand("set density 70")    # set density value
NLCommand("setup")             # call the setup routine 
NLCommand("go")                # launch the model from R