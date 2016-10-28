### 
# This script uses some packages that are not installed with the base version of R. Use the 
# function `install.packages` to install these extra packages, e.g., `install.packages("dplyr")`.
# If you already have these packages installed, it is a good idea to make sure you have the latest
# version using `update.packages`, e.g., `update.packages("dplyr")`.
###

# Read the raw data from a CSV file.
# This assumes the file is located on you Desktop on a Mac OS, change the file path as needed.
sales_data <- read.csv("~/Desktop/Sales Data.csv", stringsAsFactors = FALSE)
# The `str` function gives you some details about the data structure.
str(sales_data)
# The function `summary` gives you some of the metrics we are interested in.
summary(sales_data)
# The `mode` function in R does not compute the mathematical mode, 
# so write our own function.
my_mode <- function(x){
	unique_x <- unique(x)
	mode_value <- unique_x[which.max(tabulate(match(x, unique_x)))]
	return(mode_value)
}

#--- Central tendency ---#
mean(sales_data$Revenue)
median(sales_data$Revenue)
my_mode(sales_data$Revenue)

library(dplyr)
sales_data %>% summarize_all(.funs = funs(mean, median, my_mode)) 
sales_data %>% summarize_at(c("Revenue", "Time", "Channel"), .funs = funs(mean, median, my_mode)) 

#--- Dispersion ---#
var(sales_data$Revenue)
sd(sales_data$Revenue)

sales_data %>% summarize_all(.funs = funs(var, sd)) 
sales_data %>% summarize_at(c("Revenue", "Time", "Channel"), .funs = funs(var, sd)) 

#--- Relationship ---#
cov(sales_data$Revenue, sales_data$Time)
cor(sales_data$Revenue, sales_data$Time)

#--- Linear Regression ---#
model <- lm(Revenue ~ Time * Channel , data = sales_data)
summary(model)

#--- Charts ---#
library(ggplot2)
library(scales)
ggplot(sales_data, aes(x = Time, y = Revenue)) + 
	geom_point(size = 3) + 
	scale_y_continuous(labels = dollar)

ggplot(sales_data, aes(x = Time, y = Revenue)) + 
	geom_point(size = 3, alpha = 0.5) + 
	scale_y_continuous(labels = dollar) + 
	geom_rug(alpha = 0.2)

ggplot(sales_data, aes(x = Time, y = Revenue, color = Channel)) + 
	geom_point(size = 3, alpha = 0.5) + 
	scale_y_continuous(labels = dollar) + 
	geom_rug(alpha = 0.2)

ggplot(sales_data, aes(x = Revenue, fill = Channel)) + 
	geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") + 
	ylab("Number of observations") + 
	scale_x_continuous(labels = dollar)

ggplot(sales_data, aes(x = Channel, y = Revenue, color = Channel)) + 
	geom_boxplot(outlier.color = NA) + 
	geom_dotplot(aes(fill = Channel), binaxis = "y", binwidth = 1.3, stackdir = "center", alpha = 0.2) + 
	stat_summary(fun.y = "mean", geom = "point", color = "darkgrey", size = 4) +
	scale_y_continuous(labels = dollar)
