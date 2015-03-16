#====================================================================
# purpose: simple examples of SEM models in R
# author: tirthankar chakravarty
# created: 16th march 2015
# revised:
# comments:
# 1. fit an SEM one factor model, and save the results as a dot file. 
# 2. the semPlot package is not able to handle results returned by OpenMx.
#====================================================================

library(OpenMx)
# library(semPlot)
library(Rgraphviz)

data(demoOneFactor)
manifests = names(demoOneFactor)
latents = c("G")
factorModel = mxModel(model = "One Factor",
                       type = "RAM",
                       manifestVars = manifests,
                       latentVars = latents,
                       mxPath(from=latents, to=manifests),
                       mxPath(from=manifests, arrows=2),
                       mxPath(from=latents, arrows=2,
                              free=FALSE, values=1.0),
                       mxData(cov(demoOneFactor), type="cov",
                              numObs=500))
summary(fitFactorModel <- mxRun(factorModel))
omxGraphviz(fitFactorModel, dotFilename = "./results/oneFactorGraph.dot")


