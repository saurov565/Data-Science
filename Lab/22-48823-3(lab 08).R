#Load Dataset
data(penguins)
head(penguins)

#PCA(principal Component Analysis)
library(palmerpenguins)

peng2 <- na.omit(peng[, c("species",
                          "bill_length_mm","bill_depth_mm",
                          "flipper_length_mm","body_mass_g")])
penguins_pca <- prcomp(
  na.omit(peng[, c("bill_length_mm","bill_depth_mm","flipper_length_mm","body_mass_g")]),
  scale. = TRUE
)
summary(penguins_pca)

#Visualize PCA
library(FactoMineR)
library(factoextra)
fviz_eig(penguins_pca)

#Biplot of PCA
fviz_pca_biplot(penguins_pca,
                geom.ind = "point",
                col.ind = peng2$species,
                palette = "jco",
                addEllipses = TRUE,
                legend.title = "Species")

#K-means Clustering
set.seed(123)

peng <- palmerpenguins::penguins
peng2 <- na.omit(peng[, c("bill_length_mm","bill_depth_mm","flipper_length_mm","body_mass_g")])

km_res <- kmeans(peng2, centers = 3, nstart = 25)
km_res

#Visualize k-means
library(ggplot2)
fviz_cluster(km_res, data = peng2,
             palette = "jco",
             ggtheme = theme_minimal())

#Compare Clusters with True Labels
library(palmerpenguins)

peng <- palmerpenguins::penguins
vars <- c("bill_length_mm","bill_depth_mm","flipper_length_mm","body_mass_g")

peng2 <- na.omit(peng[, c("species", vars)])   # includes species now

set.seed(123)
km_res <- kmeans(peng2[, vars], centers = 3, nstart = 25)

table(km_res$cluster, peng2$species)





