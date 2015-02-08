load("Data//Distance_Matrix.RData")

mX = Distance_Matrix
rm(Distance_Matrix)
vDist = apply(mX, 2, sum)
plot(vDist, type = "l", col = "red")
mXR = mX[-(order(-vDist)[1:20]), -(order(-vDist)[1:20])]
vDistR = apply(mXR, 2, sum)
lines(vDistR, col = "blue")

# retain the top 20 trips
mXR2 = mXR[(order(vDistR)[1:20]), (order(vDistR)[1:20])]

# the probability that these
