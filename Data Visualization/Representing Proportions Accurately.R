source(file.path(".", "generateData.R"))
output_path <- file.path("~", "Desktop")

#--- Representing Proportions Accurately ---#
library(ggplot2)
library(scales)
# Define constants for all plots
base_size    <- 24
image_width  <- 16
image_height <- 9

dta <- generateRevenueShareData()

ggplot(dta, aes(x = 1, y = Revenue, fill = Product)) + 
	theme_grey(base_size = base_size) + 
	geom_bar(stat = "identity") + 
	scale_fill_manual(values = c("orange", "darkgrey"))  + 
	theme(axis.ticks=element_blank(), panel.grid = element_blank(), axis.title=element_blank(), axis.text.y=element_blank(), axis.text.x = element_blank()) + 
	geom_text(aes(y = Revenue / 2 + c(0, cumsum(Revenue)[-length(Revenue)]), label = paste(Product, percent(plyr::round_any(Percent, 0.01)), sep = "\n")), size = 8) + 
	coord_polar(theta = "y") + 
	guides(fill = FALSE)
ggsave(file.path(output_path, "Pie chart - simple.png"), width = image_width, height = image_height)

dta <- generateStateData()

ggplot(dta, aes(x = 1, y = Revenue, fill = State)) + 
	theme_grey(base_size = base_size) + 
	geom_bar(stat = "identity") + 
	theme(axis.ticks = element_blank(), panel.grid = element_blank(), axis.title = element_blank(), axis.text.y=element_blank(), axis.text.x = element_blank()) + 
	geom_text(aes(y = Revenue / 2 + c(0, cumsum(Revenue)[-length(Revenue)]), label = paste(State, percent(plyr::round_any(Percent, 0.01)), sep = "\n")), size = 5.5) + 
	coord_polar(theta = "y") + 
	guides(fill = FALSE)
ggsave(file.path(output_path, "Pie chart - large.png"), width = image_width, height = image_height)

ggplot(dta, aes(x = State, y = Percent)) + 
	theme_grey(base_size = 24) + geom_bar(stat = "identity") + 
	scale_y_continuous(labels = percent)
ggsave(file.path(output_path, "Bar chart.png"), width = image_width, height = image_height)

dta$State <- factor(dta$State, levels = rev(dta$State.text))
ggplot(dta, aes(x = Percent, y = State)) + 
	theme_grey(base_size = base_size) + 
	geom_point(size = 6) + 
	theme(panel.grid.major.y = element_line(linetype = 2, color = "grey")) + 
	scale_x_continuous(labels = percent) + 
	xlab("Percent of Online Subscription Revenue") + 
	ylab("State")
ggsave(file.path(output_path, "Cleveland dot chart.png"), width = image_width, height = image_height)
