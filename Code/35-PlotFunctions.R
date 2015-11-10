library(ggplot2)

# inverse hyperbolic since function
ihs <- function(x) {
  y <- log(x + sqrt(x^2 + 1))
  return(y)
}

# hyperbolic sine function
hs = function(x) {
  0.5*exp(-x)*(exp(2*x) - 1)
}

# data
dfX = data_frame(x = seq(-2, 2, 0.01),
                 ihs = ihs(x),
                 hs1 = sinh(x),
                 hs2 = hs(x))

# plot
ggplot(data = dfX, aes(x = x)) +
  stat_function(aes(color = "Inverse Hyperbolic Sine"), fun = ihs, ) +
  stat_function(aes(color = "Hyperbolic Sine (Manual)"), fun = hs) +
  stat_function(aes(color = "Hyperbolic Sine (Base)"), fun = sinh) +
  theme_bw() +
  scale_colour_manual("Function", values = c("red", "darkblue", "darkgreen"))

ggsave("Output/IHS.png")
