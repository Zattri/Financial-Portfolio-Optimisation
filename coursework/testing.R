library("plyr")
setwd("C:/Users/Zattri/Desktop/modern_opt/coursework")

# Testing transforming data to give sector price/earnings sum
df = read.csv("financials.csv")
trouble_makers = df[rowSums(is.na(df)) != 0,] # Find the companies with null values
print(trouble_makers$Name)

df = df[rowSums(is.na(df)) == 0,] # Remove companies with any null values
df = df[is.na(df$Price.Earnings) == 0,] # Remove companies that only have null price/earnings

table(df$Sector)
# Mapping sector names to integer values 
sectors = unique(df$Sector)
df$sector_index = as.numeric(factor(df$Sector, levels=sectors))

#minimise price per earnings
sum_price_per_earnings = function(data, weightings){
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


sum_value_at_risk = function(data, weightings) {
  value_at_risk = 0
  for (w in weightings) {
    if (w > 0) {
      sector = as.double(which(weightings == w))
      sector_data = data[data$sector_index == sector,]
      week_high = sector_data$X52.Week.High
      price = sector_data$Price
      risk_value = sum(price) - sum(week_high)
      value_at_risk = value_at_risk + (risk_value * w)
    }
  }
  
  return(value_at_risk)
}

generateWeighting = function(groups = 11) {
  V = 100000 # The sum total of all numbers generated
  G = groups #The amount of numbers to generate
  x = c(rmultinom(1, V, rep.int(1 / G, G))/V)  # Dividing by V to get back to 0-1 range, vectorising for easier output
  return(x)
}

#weightings = c(0,0,0.3,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0) - Example weighting

lowest_price = 10000000000
for (i in 1:10000) {
  weightings = generateWeighting()
  cost = sum_price_per_earnings(data=df, weightings=weightings)
  VaR = sum_value_at_risk(data=df, weightings=weightings)
  profits = cost + VaR
  if (profits <= lowest_price) {
    best_weighting = weightings
    lowest_price = profits
  }
}

print(cat("Best weights:", best_weighting, "|", lowest_price))


# Stolen from Atif
#eval = function(x) {
#  return (c(sum_price_per_earnings(data, 0.5, 1))) # minimising price per earnings
#}