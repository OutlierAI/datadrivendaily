# LCA
dta <- read.csv("Sample PCA Data.csv", check.names = FALSE)
library(poLCA)
f <- cbind(Age, Source) ~ 1
model <- list()
for (i in seq_len(3)) {
	pdf(paste0("LCA ", i, ".pdf"))
	model[[i]] <- poLCA(f, dta, nclass = i, graphs = TRUE)
	dev.off()
}
pdf("BIC.pdf")
plot(unlist(lapply(model, function(m) { m$bic })),
		 xlab = "Number of Classes", ylab = "Bayesian Information Criterion (BIC)")
dev.off()
