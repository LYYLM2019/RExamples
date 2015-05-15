#====================================================================
# purpose: simple example of permutation testing
# author: tirthankar chakravarty
# created:
# comments: original here: http://spark.rstudio.com/ahmed/permutation/
#====================================================================

library(ggplot2)
a = c(86, 88, 89, 89, 92, 93, 94, 94, 94, 95, 95, 96, 96, 97, 97, 98,
      98, 99, 99, 101, 106, 107, 110, 113, 116, 118) # Homozygous (BB)
b = c(89, 90, 92, 93, 93, 96, 99, 99, 99, 102, 103, 104, 105, 106, 106,
      107, 108, 108, 110, 110, 112, 114, 116, 116) # Heterozygous (BA)
t.test(a, b, var.equal = FALSE)

# function to compute the permutation statistics
permuteTTest = function(vX1, vX2, times) {
  replicate(times, {
    vX = c(vX1, vX2)
    vXP = sample(vX, replace = FALSE)
    vX1P = vXP[1:length(vX1)]
    vX2P = vXP[length(vX1)+1:length(vXP)]
    t.test(vX1P, vX2P, var.equal = FALSE)$statistic
  })
}

# compute the permutation statistics
vTP = permuteTTest(a, b, 1000)
names(vTP) = seq.int(1000)

# kernel density plot of permuted estimates
# original statistic in green
qplot(vTP, geom = "line", stat = "density") + theme_bw() +
  geom_segment(aes(y = 0, yend = density(vTP)$y[which(abs(density(vTP)$x-0)==min(abs(density(vTP)$x-0)))],
               x = 0, xend = 0), color = "red") +
  geom_segment(aes(x = t.test(a, b, var.equal = FALSE)$statistic,
                   xend = t.test(a, b, var.equal = FALSE)$statistic,
                   y = 0,
                   yend = density(vTP)$y[which(abs(density(vTP)$x - t.test(a, b, var.equal = FALSE)$statistic)
                                               == min(abs(density(vTP)$x -
                                                            t.test(a, b, var.equal = FALSE)$statistic)))]),
               color = "green")

# p-value
1- mean(abs(t.test(a, b, var.equal = FALSE)$statistic) > abs(vTP))
