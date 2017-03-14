# PCA
dta <- read.csv("Sample PCA Data.csv", check.names = FALSE)
library(FactoMineR)
pdf("Factor map.pdf")
res.pca <- PCA(dta[ , 3:11], scale.unit = TRUE)
dev.off()
dimdesc(res.pca)
