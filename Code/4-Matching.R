require(Matching)
data(lalonde)
# Estimate the propensity model
glm1  <- glm(treat~age + I(age^2) + educ + I(educ^2) + black +
                   +                   hisp + married + nodegr + re74  + I(re74^2) + re75 + I(re75^2) +
                   +                   u74 + u75, family=binomial, data=lalonde)
#save data objects
X  <- glm1$fitted
Y  <- lalonde$re78
Tr  <- lalonde$treat

# one-to-two matching with replacement
rr  <- Match(Y=NULL, Tr=Tr, X=X, M=2, ties=F, caliper=0.01);
summary(rr)

#Obtain the matched data set
matched <- rbind(lalonde[rr$index.treated,], lalonde[rr$index.control,])

nrow(matched)

#==========================================================
#==========================================================

dfTC = data.frame(idxTreated = rr$index.treated, idxControl = rr$index.control,
                  numControl = factor(rep(1:2), labels = paste0("Control", 1:2)))
dfTCWide = reshape2::dcast(dfTC, idxTreated ~ numControl,
                           value.var = "idxControl")


head(dfTCWide)
