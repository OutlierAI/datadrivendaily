source(file.path(".", "generateData.R"))
dta         <- generateMonthlyData()
output_path <- file.path("~", "Desktop")

#--- Comparing Multiple Measures on the Same Plot ---#
library(ggplot2)
library(scales)
# Define constants for all plots
base_size    <- 24
image_width  <- 16
image_height <- 9
line_size    <- 3
point_size   <- 6

# Dual-scaled axes are not supported in ggplot2 (because they are not considered a good idea),
# so some work needs to be done to create one.
# The code here is a modified version of https://rpubs.com/kohske/dual_axis_in_ggplot2
# Note, the final image output shown has different axes labels that I manually adjusted by hand
# because it was a pain to do it in the code due to the lack of support for dual-scaled axes.
library(gtable)
library(grid)
grid.newpage()
p1 <- ggplot(dta, aes(x = Date, y = Revenue)) + 
	theme_grey(base_size = base_size) + 
	geom_line(color = "darkgreen", size = line_size) + 
	geom_point(size = point_size, color = "darkgreen") + 
	scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
	scale_y_continuous(labels = dollar)
p2 <- ggplot(dta, aes(x = Date, y = Customers)) + 
	theme_grey(base_size = base_size) + 
	geom_line(color = "blue", size = line_size) + 
	geom_point(size = point_size, color = "blue") + 
	scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
	theme(panel.background = element_rect(fill = NA))
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))
# Overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
# Axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
# draw it
grid.draw(g)
# ggsave does not save all elements, so save the image directly from the GUI

# Percentage plot
ggplot(dta) + 
	theme_grey(base_size = base_size) + 
	geom_hline(yintercept = 0, color = "darkgrey", size = 2) + 
	geom_line(aes(x = Date, y = Revenue.change, color = "darkgreen"), size = line_size) + 
	geom_point(aes(x = Date, y = Revenue.change, color = "darkgreen"), size = point_size) + 
	geom_line(aes(x = Date, y = Customers.change, color = "blue"), size = line_size) + 
	geom_point(aes(x = Date, y = Customers.change, color = "blue"), size = point_size) + 
	scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
	scale_y_continuous(name = "Percent change", labels = percent) + 
	scale_colour_manual(name = "", values = c("blue" = "blue", "darkgreen" = "darkgreen"), labels = c("Customers", "Revenue"))
ggsave(file.path(output_path, "Dual percent.png"), width = image_width, height = image_height)
