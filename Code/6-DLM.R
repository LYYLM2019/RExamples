library(KFAS)
library(ggplot2)
library(dlm)

# run the code below once only
download.file(url = "http://dl.dropbox.com/s/4w0utkqdhqribl4/fishdata.csv", 
              destfile = "Data/fishdata.csv")
download.file(url = "http://shazam.econ.ubc.ca/intro/P.txt", 
              destfile = "Data/shazamdata.txt")

#================================================
# simple linear regression model: fisheries data
#================================================
dfFish = read.csv("Data/fishdata.csv", header = TRUE)
lmFish = lm(inlandfao ~ marinefao, data = dfFish)

summary(lmFish)

# plot with a smooth
ggplot(dfFish, aes(x = inlandfao, y = marinefao)) +
  geom_line() + theme_bw() + 
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "green")

## NOTE: in the plot above, he is motivating the fact that 
#   a time-varying coefficient is more appropriate than a constant coefficient

#================================================
# estimating a TVC model: Shazam data
#================================================
dfC = read.table("P.txt", header = TRUE)
capm.ts <- ts(capm, start = c(1978, 1), frequency = 12)
colnames(capm)
plot(capm.ts)
IBM <- capm.ts[, "IBM"]  - capm.ts[, "RKFREE"]
x <- capm.ts[, "MARKET"] - capm.ts[, "RKFREE"]
x
plot(x)
outLM <- lm(IBM ~ x)
outLM$coef
acf(outLM$res)
qqnorm(outLM$res)
sig <- var(outLM$res)
sig

mod <- dlmModReg(x,dV = sig, m0 = c(0, 1.5), C0 = diag(c(1e+07, 1)))
outF <- dlmFilter(IBM, mod)
outF$m
plot(outF$m)
outF$m[ 1 + length(IBM), ]

########## PAGES 124-125
buildCapm <- function(u){
  dlmModReg(x, dV = exp(u[1]), dW = exp(u[2:3]))
}

outMLE <- dlmMLE(IBM, parm = rep(0,3), buildCapm)
exp(outMLE$par)
outMLE
outMLE$value
mod <- buildCapm(outMLE$par)
outS <- dlmSmooth(IBM, mod)
plot(dropFirst(outS$s))
outS$s