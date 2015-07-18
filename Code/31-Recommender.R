# create a binary data matrix
numUsers = 1000
numLinks = 100
mUserItem = matrix(sample(c(0, 1), size = numUsers*numLinks, 
                          replace = TRUE, prob = c(0.7, 0.3)), 
                   nrow = numUsers, ncol = numLinks, 
                   dimnames = list(rownames = paste0("user", 1:numUsers),
                                   colnames = paste0("link", 1:numLinks)))

# create a distance matrix 
mUserDist = dist(mUserItem, method = "binary")
clustUser = hclust(mUserDist)
plot(clustUser, labels = FALSE)

userCluster = cutree(clustUser, k = 10)
