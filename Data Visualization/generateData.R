generateTransactionalData <- function(seed = 6, n = 1000){
	set.seed(seed)
	dta <- data.frame(
		Customer = paste("Customer", 1:n),
		Revenue  = c(rnorm(n / 2, 35, 10), rnorm(n / 2, 50, 15)),
		Time     = rgamma(n, 3),
		Channel  = c(sample(c("Store", "Online"), n / 2, replace = TRUE, prob = c(0.75, 0.25)),
								 sample(c("Store", "Online"), n / 2, replace = TRUE, prob = c(0.25, 0.75))),
		stringsAsFactors = FALSE)
	dta$Revenue[dta$Revenue <= 1] <- dta$Revenue[dta$Revenue <= 1] + 50  # enforce positive revenue
	
	return(dta)
}

generateMonthlyData <- function(seed = 6){
	set.seed(seed)
	library(lubridate)
	monthly_data <- data.frame(
		Date      = seq(ymd("2015-01-01"), ymd("2015-12-02"), by = "1 month"), 
		Revenue   = rnorm(12, 25000, 5000), 
		Customers = runif(12, 485, 515), 
		stringsAsFactors = FALSE)
	monthly_data$Revenue.change   <- (monthly_data$Revenue - monthly_data$Revenue[1]) / monthly_data$Revenue[1]
	monthly_data$Customers.change <- (monthly_data$Customers - monthly_data$Customers[1]) / monthly_data$Customers[1]
	
	return(monthly_data)
}

generateRevenueShareData <- function(){
	revenue_share <- data.frame(
		Product = c("Revenue from\nBaked Goods", "Revenue from\nOnline Subscriptions"), 
		Revenue = c(100000, 75000), 
		stringsAsFactors = FALSE)
	revenue_share$Percent <- revenue_share$Revenue / sum(revenue_share$Revenue)

	return(revenue_share)	
}

generateStateData <- function(seed = 6){
	revenue_per_state <- data.frame(
		State.text = c("CA", "NY", "PA", "FL", "CO", "VA", "MN", "NJ", "MA", "NM"), 
		Revenue    = c(100, 66, 50, 48, 45, 40, 33, 22, 20, 6), 
		stringsAsFactors = FALSE)
	revenue_per_state$State   <- factor(revenue_per_state$State.text, levels = revenue_per_state$State.text)
	revenue_per_state$Percent <- revenue_per_state$Revenue / sum(revenue_per_state$Revenue)
	
	return(revenue_per_state)
}
