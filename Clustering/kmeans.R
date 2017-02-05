library(ggplot2)
library(scales)
p <- list()

# Read the raw data from file
dta <- read.csv("sales_data.csv")

# Plot the data to get an idea of what it looks like
p[["data"]] <-
	ggplot(dta) +
	geom_point(aes(x = Age, y = Dollars.per.purchase, color = factor(Average.table.size), size = Purchases.per.year)) +
	scale_size_area(name = "Purchases\nper year") +
	scale_color_discrete(name = "Average table\nsize") +
	scale_y_continuous(labels = dollar) +
	xlab("Age") +
	ylab("Amount per purchase")

# Run k-means clustering for different k's
ks         <- 2:29
results    <- list()
result_dta <- data.frame()
for (k in ks) {
	cluster_dta <- dta
	results[[paste0("k", k)]] <- kmeans(cluster_dta, k)
	cluster_dta$K <- k
	cluster_dta$Cluster <- results[[paste0("k", k)]]$cluster
	result_dta <- dplyr::bind_rows(result_dta, cluster_dta)
}
p[["clusters"]] <-
	p[["data"]] %+%
	dplyr::filter(result_dta, K %in% 2:4) +
	aes(shape = factor(Cluster)) +
	facet_wrap(~ K, 1) +
	scale_shape_manual(name = "Cluster", values = c(15, 18, 8, 3))

# Plot the within groups sum of squares
errors <- data.frame(K = ks, Within = plyr::laply(results, function(rs){ sum(rs$withinss) }), stringsAsFactors = FALSE)
p[["error"]] <-
	ggplot(errors) +
	geom_hline(yintercept = 0, color = "darkgrey") +
	geom_point(aes(x = K, y = Within), size = rel(4)) +
	xlab("Number of clusters (k)") +
	ylab("Within groups sum of squares")

# Write plots to disk
for (p_name in names(p)) {
	ggsave(paste0(p_name, ".png"), p[[p_name]], width = 16, height = 9)
}
