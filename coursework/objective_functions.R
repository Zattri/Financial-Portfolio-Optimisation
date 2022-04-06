setwd("C:/Users/Zattri/Desktop/modern_opt/coursework")
# Read in data for eval function
df = read.csv("processed_financials.csv")

#minimise price per earnings
price_per_earnings = function(data, weightings){
  total_sum = 0
  for (w in weightings) {
    if (w > 0) {
      sector = as.double(which(weightings == w))
      sector_data = data[data$sector_index == sector,]
      price_earnings = sector_data$Price.Earnings
      total_sum = total_sum + (sum(price_earnings) * w)
    }
  }
  
  return(total_sum)
}


value_at_risk = function(data, weightings) {
  VaR = 0
  for (w in weightings) {
    if (w > 0) {
      sector = as.double(which(weightings == w))
      sector_data = data[data$sector_index == sector,]
      week_high = sector_data$X52.Week.High
      price = sector_data$Price
      risk_value = sum(price) - sum(week_high)
      VaR = VaR + (risk_value * w)
    }
  }
  
  return(VaR)
}

# Kinda works, might need more tweaking
budget_constraint = function(x) {
  return(c(sum(x) - 100))
}

# Basic, needs improving
eval = function(x) {
  return (c(price_per_earnings(df, x), value_at_risk(df, x))) # minimising cost per earnings and value at risk
}