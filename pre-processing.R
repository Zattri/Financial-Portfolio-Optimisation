library("plyr")

# Testing transforming data to give sector price/earnings sum
df = read.csv("financials.csv")

# Find the companies with null values
#trouble_makers = df[rowSums(is.na(df)) != 0,] 
#print(trouble_makers$Name)

df = df[rowSums(is.na(df)) == 0,] # Remove companies with any null values
#df = df[is.na(df$Price.Earnings) == 0,] # Remove companies that only have null price/earnings

# Mapping sector names to integer values 
# table(df$Sector)
sectors = unique(df$Sector)
df$sector_index = as.numeric(factor(df$Sector, levels=sectors))
write.csv(df, "processed_financials.csv")