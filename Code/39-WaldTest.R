library(lmtest)
dfS = read.csv("Data/SAFETY.csv")
logitS = glm(Unsafe ~ factor(Size) + Weight +
               Region, data = dfS, family = binomial())
summary(logitS)
waldtest(logitS, test = "Chisq")