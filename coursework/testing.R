library("plyr")
# Testing transforming data to give sector price/earnings sum
data = read.csv("financials.csv")

# Mapping sector names to integer values 
sectors = unique(data$Sector)
data["sector_index"] = as.factor(data$Sector)
data["sector_index"] = mapvalues(x = data$sector_index, from=unique(data$Sector), to=seq(1,length(sectors)))



#minimise price per earnings
sum_price_per_earnings = function(data, allocation, sector){
  sector_data = data[data["sector_index"] == sector,]
  return (sum(sector_data["Price.Earnings"]) * allocation)
}

calculated_price = sum_price_per_earnings(data=data, allocation=0.5, sector=1)

eval = function(x) {
  return (c(sum_price_per_earnings(data, 0.5, 1))) # minimising price per earnings
}