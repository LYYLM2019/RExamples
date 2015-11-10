library(samplesize)

vals = data.frame(
  affected = c(1, 2),
  mean = c(-0.8007305, 4.5799913),
  sd = c(7.887657, 6.740781),
  length = c(57, 16))

power <- 0.90
alpha <- 0.05
mean.diff <- vals[1,2]-vals[2,2]
sd1 <- vals[1,3]
sd2 <- vals[2,3]
k <- vals[2,4]/(vals[1,4]+vals[2,4])
k <- vals[2,4]/vals[1,4]

design <- "unpaired"
fraction <- "unbalanced"
variance <- "equal"

# Get the sample size
tt1 = n.ttest(power = power,
        alpha = alpha,
        mean.diff = mean.diff,
        sd1 = sd1,
        sd2 = sd2,
        k = k,
        design = design,
        fraction = fraction,
        variance = variance)

assertthat::are_equal(ceiling(tt1$`Sample size group 1`*tt1$Fraction),
                      tt1$`Sample size group 2`)