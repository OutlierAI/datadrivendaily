# Read the raw data from file
dta <- read.csv("sales_data.csv")

# Run hclust algorithm
hc <- hclust(dist(dta))
p  <- ggdendrogram(hc, rotate = TRUE)

# Write plot to disk
ggsave("dendrogram.png", p, width = 16, height = 9)
