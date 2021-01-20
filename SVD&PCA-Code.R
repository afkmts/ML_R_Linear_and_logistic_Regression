library(MASS)

#Example 1
a <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1), 9, 4)

a.svd <- svd(a)
a.svd$d

ds <- diag(1/a.svd$d[1:3])
u <- a.svd$u
v <- a.svd$v
us <- as.matrix(u[, 1:3])
vs <- as.matrix(v[, 1:3])

(a.ginv <- vs %*% ds %*% t(us))

# using the function ginv defined in MASS
ginv(a)

#Example 2
library(foreign)
auto <- read.dta("http://statistics.ats.ucla.edu/stat/data/auto.dta")

pca.m1 <- prcomp(~trunk + weight + length + headroom, data = auto,
                 scale = TRUE)

screeplot(pca.m1)

# spectral decomposition: eigen values and eigen vectors
xvars <- with(auto, cbind(trunk, weight, length, headroom))
corr <- cor(xvars)
a <- eigen(corr)
(std <- sqrt(a$values))

(rotation <- a$vectors)

# svd approach
df <- nrow(xvars) - 1
zvars <- scale(xvars)
z.svd <- svd(zvars)
z.svd$d/sqrt(df)

z.svd$v

#Example 3
cnut <- read.dta("http://statistics.ats.ucla.edu/stat/data/cerealnut.dta")

# centering the variables
mds.data <- as.matrix(sweep(cnut[, -1], 2, colMeans(cnut[, -1])))
dismat <- dist(mds.data)
mds.m1 <- cmdscale(dismat, k = 8, eig = TRUE)
mds.m1$eig

mds.m1 <- cmdscale(dismat, k = 2, eig = TRUE)
x <- mds.m1$points[, 1]
y <- mds.m1$points[, 2]
plot(x, y)
text(x + 20, y, label = cnut$brand)


