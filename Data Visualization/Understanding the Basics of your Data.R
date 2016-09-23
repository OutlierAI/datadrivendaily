source(file.path(".", "generateData.R"))
dta         <- generateTransactionalData()
output_path <- file.path("~", "Desktop")

#--- Understanding the Basics of your Data ---#
library(ggplot2)
library(scales)
# Define constants for all plots
base_size    <- 24
image_width  <- 16
image_height <- 9
point_size   <- 6
point_alpha  <- 0.5
rug_alpha    <- 0.2

ggplot(dta, aes(x = Time, y = Revenue)) + 
	theme_grey(base_size = base_size) + 
	geom_point(size = point_size) + 
	scale_y_continuous(labels = dollar)
ggsave(file.path(output_path, "Scatter.png"), width = image_width, height = image_height)

ggplot(dta, aes(x = Time, y = Revenue)) + 
	theme_grey(base_size = base_size) + 
	geom_point(size = point_size, alpha = point_alpha) + 
	scale_y_continuous(labels = dollar) + 
	geom_rug(alpha = rug_alpha)
ggsave(file.path(output_path, "Scatter alpha rug.png"), width = image_width, height = image_height)

ggplot(dta, aes(x = Time, y = Revenue)) + 
	theme_grey(base_size = base_size) + 
	geom_point(aes(color = Channel), size = point_size, alpha = point_alpha) + 
	scale_y_continuous(labels = dollar) + 
	geom_rug(aes(color = Channel), alpha = rug_alpha)
ggsave(file.path(output_path, "Scatter alpha color rug.png"), width = image_width, height = image_height)

ggplot(dta) + 
	theme_grey(base_size = base_size) + 
	geom_histogram(binwidth = 5, aes(x = Revenue, fill = Channel), alpha = 0.5, position = "identity") + 
	ylab("Number of observations") + 
	scale_x_continuous(labels = dollar)
ggsave(file.path(output_path, "Histogram.png"), width = image_width, height = image_height)

ggplot(dta, aes(x = Channel, y = Revenue, color = Channel)) + 
	theme_grey(base_size = base_size) + 
	geom_boxplot(outlier.color = NA) + 
	geom_dotplot(aes(fill = Channel), binaxis = "y", binwidth = 1.7, stackdir = "center", alpha = 0.2) + 
	stat_summary(fun.y = "mean", geom = "point", color = "darkgrey", size = 4) + 
	scale_y_continuous(labels = dollar)
ggsave(file.path(output_path, "Boxplot.png"), width = image_width, height = image_height)

# Calculate exact median and mean by channel
library(dplyr)
dta %>% group_by(Channel) %>% summarize(Median = median(Revenue), Mean = mean(Revenue))
