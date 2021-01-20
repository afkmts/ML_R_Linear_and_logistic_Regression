#1.	Load factoextra for visualization 
library(factoextra)
data(mtcars)
mtcars.active <- mtcars[1:27, 1:7]
head(mtcars.active[, 1:7])

#2. Compute PCA and View summary.
res.pca <- prcomp(mtcars.active, center = TRUE,scale. = TRUE)
summary(res.pca)

#Let's call str() to have a look at PCA object.
str(res.pca)

#3. Plotting PCA
fviz_eig(res.pca)

#4.	Graph of individuals. 
fviz_pca_ind(res.pca,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

#5.	Graph of variables. 
fviz_pca_var(res.pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
             
#6.	Biplot of individuals and variables 
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF",
                col.ind = "#696969" )


#Contribution of variables to Dim-1
fviz_contrib(res.pca, choice = "var", axes = 1) 

#Contribution of variables to Dim-2
fviz_contrib(res.pca, choice = "var", axes = 2) 


# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

# Data for the supplementary individuals
ind.sup <- mtcars[28:32, 1:7]
ind.sup[, 1:7]

#2.	Predict the coordinates of new individuals data
ind.sup.coord <- predict(res.pca, newdata = ind.sup)
ind.sup.coord[, 1:5]

# Plot of active individuals
p <- fviz_pca_ind(res.pca, repel = TRUE)
# Add supplementary individuals
fviz_add(p, ind.sup.coord, color ="blue")

# Centering and scaling the supplementary individuals
ind.scaled <- scale(ind.sup, 
                    center = res.pca$center,
                    scale = res.pca$scale)

# Coordinates of the individividuals
coord_func <- function(ind, loadings){
  r <- loadings*ind
  apply(r, 2, sum)
}
pca.loadings <- res.pca$rotation
ind.sup.coord <- t(apply(ind.scaled, 1, coord_func, pca.loadings ))
ind.sup.coord[, 1:5]

#Grouping by qualitative data
#For group mtcars$vs
groups <- as.factor(mtcars$vs[1:27])
fviz_pca_ind(res.pca,
             col.ind = groups,
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, 
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE)

#Calculate the coordinates for the levels of grouping variables
library(magrittr) # for pipe %>%
library(dplyr)   # everything else
# 1. Individual coordinates
res.ind <- get_pca_ind(res.pca)
# 2. Coordinate of groups for column vs
coord.groups <- res.ind$coord %>%
  as_tibble() %>%
  select(Dim.1, Dim.2) %>%
  mutate(vs = groups) %>%
  group_by(vs) %>%
  summarise(
    Dim.1 = mean(Dim.1),
    Dim.2 = mean(Dim.2)
  )
coord.groups

#For group mtcars$am
groups <- as.factor(mtcars$am[1:27])
fviz_pca_ind(res.pca,
             col.ind = groups,
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, 
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE)

# 3. Coordinate of groups for column am
coord.groups <- res.ind$coord %>%
  as_tibble() %>%
  select(Dim.1, Dim.2) %>%
  mutate(am = groups) %>%
  group_by(am) %>%
  summarise(
    Dim.1 = mean(Dim.1),
    Dim.2 = mean(Dim.2)
  )
coord.groups

#Quantitative variables
quanti.sup <- mtcars[1:27, 10:11, drop = FALSE]
head(quanti.sup)

# Predict coordinates and compute cos2
quanti.coord <- cor(quanti.sup, res.pca$x)
quanti.cos2 <- quanti.coord^2
# Graph of variables including supplementary variables
p <- fviz_pca_var(res.pca)
fviz_add(p, quanti.coord, color ="blue", geom="arrow")

# Helper function 
var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}
# Compute Coordinates
loadings <- res.pca$rotation
sdev <- res.pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
head(var.coord[, 1:5])

# Compute Cos2
var.cos2 <- var.coord^2
head(var.cos2[, 1:5])

# Compute contributions
comp.cos2 <- apply(var.cos2, 2, sum)
contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
head(var.contrib[, 1:5])

# Coordinates of individuals
ind.coord <- res.pca$x
head(ind.coord[, 1:5])

# Cos2 of individuals
# 1. square of the distance between an individual and the
# PCA center of gravity
center <- res.pca$center
scale<- res.pca$scale
getdistance <- function(ind_row, center, scale){
  return(sum(((ind_row-center)/scale)^2))
}
d2 <- apply(mtcars.active,1,getdistance, center, scale)
# 2. Compute the cos2. The sum of each row is 1
cos2 <- function(ind.coord, d2){return(ind.coord^2/d2)}
ind.cos2 <- apply(ind.coord, 2, cos2, d2)
head(ind.cos2[, 1:5])

# Contributions of individuals
contrib <- function(ind.coord, comp.sdev, n.ind){
  100*(1/n.ind)*ind.coord^2/comp.sdev^2
}
ind.contrib <- t(apply(ind.coord, 1, contrib, 
                       res.pca$sdev, nrow(ind.coord)))
head(ind.contrib[, 1:5])

