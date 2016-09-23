source(file.path(".", "generateData.R"))
dta         <- generateTransactionalData()
output_path <- file.path("~", "Desktop")

#--- Plotting Regression Results ---#
# Create the model
model <- lm(Revenue ~ Time * Channel , data = dta)
summary(model)
# Create a table of confidence intervals and coefficients (this step could have been done more robustly)
ci             <- as.data.frame(confint(model))
ci$Coefficient <- row.names(ci)
coef           <- data.frame(
	Estimate    = model$coefficients, 
	Significant = c(TRUE, FALSE, TRUE, FALSE), 
	stringsAsFactors = FALSE)
results        <- cbind(ci, coef)
# Make the names of the regression output more pretty
results$Coefficient[results$Coefficient == "(Intercept)"]       <- "Intercept"
results$Coefficient[results$Coefficient == "ChannelStore"]      <- "Channel (Store)"
results$Coefficient[results$Coefficient == "Time:ChannelStore"] <- "Time x Channel (Store)"
library(dplyr)
ordered_values      <- results %>% arrange(desc(Estimate))
results$Coefficient <- factor(results$Coefficient, levels = rev(ordered_values$Coefficient))
# Create plot
library(ggplot2)
library(scales)
ggplot(results) + 
	theme_grey(base_size = 24) + 
	geom_hline(yintercept = 0, color = "darkgrey", size = 1) + 
	geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`, x = Coefficient, color = Significant), size = 3) + 
	geom_point(aes(x = Coefficient, y = Estimate, color = Significant), size = 6) + 
	scale_y_continuous(labels = dollar, name = "Revenue") + 
	scale_color_manual(values = c("grey", "black")) + 
	scale_fill_manual(values = c("lightgrey", "black")) + 
	coord_flip() + 
	guides(color = FALSE, fill = FALSE)
ggsave(file.path(output_path, "Regression.png"), height = 9, width = 16)
