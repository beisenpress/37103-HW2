#### 37301 Data Driven Marketing ######
#### Homework 2 #######

library(psych)
setwd("/Users/ben/Dropbox/Chicago Booth/37103 Data Driven Marketing/Homework 2/37103-HW2")
load("Detergent.RData")

####################################################
################## Part 1 ##########################

# Create variables of revenue
detergent_DF$r_tide128 = detergent_DF$p_tide128 * detergent_DF$q_tide128
detergent_DF$r_tide64 = detergent_DF$p_tide64 * detergent_DF$q_tide64
detergent_DF$r_wish64 = detergent_DF$p_wisk64 * detergent_DF$q_wisk64

# Calculate total revenue across all products
total_revenue = sum(detergent_DF$r_tide64) + sum(detergent_DF$r_wish64) + sum(detergent_DF$r_tide128) 

print("Revenue share for Tide 128:")
print(sum(detergent_DF$r_tide128)/total_revenue)
print("Revenue share for Tide 64:")
print(sum(detergent_DF$r_tide64)/total_revenue)
print("Revenue share for Wisk 64:")
print(sum(detergent_DF$r_wish64)/total_revenue)

# Create variables for price differences
detergent_DF$p_t128_t64 = detergent_DF$p_tide128 - detergent_DF$p_tide64
detergent_DF$p_t64_w64 = detergent_DF$p_tide64 - detergent_DF$p_wisk64

# Summary data for prices and price differnces
describe(detergent_DF[,c("p_tide128","p_tide64","p_wisk64","p_t128_t64","p_t64_w64")])

hist(detergent_DF$p_t128_t64, main = "Histogram of Tide 128 and Tide 64 Price Difference", xlab = "Price Difference ($)", col = "blue")
hist(detergent_DF$p_t64_w64, main = "Histogram of Tide 64 and Wisk 64 Price Difference", xlab = "Price Difference ($)", col = "blue")

# Tide sales are much higher than wisk.  
# Tide 128 is priced about $4 above Tide 64, but there is a fair amount of variety in price differences
# Wish 64 is usually priced lower than Tide 64, by 30 cents on average. However, tide has a lower price a fair amount of the time.

####################################################
################## Part 2 ##########################

# Calculate velocity as quantity / acv
detergent_DF$v_tide128 = detergent_DF$q_tide128 / detergent_DF$acv
detergent_DF$v_tide64 = detergent_DF$q_tide64 / detergent_DF$acv
detergent_DF$v_wish64 = detergent_DF$q_wisk64 / detergent_DF$acv

# The purpose of the velocity variable is putting sales of detergent in terms of store size.
# An equal price drop will drive more sales at larger stores.  We don't want this to bias our estimates of elasticity.

# Run regression to get price elasticities
reg.t128.1 <- lm(log(v_tide128) ~ log(p_tide128) + log(p_tide64) + log(p_wisk64), data = detergent_DF)
summary(reg.t128.1)
reg.t64.1 <- lm(log(v_tide64) ~ log(p_tide64) + log(p_tide128) + log(p_wisk64), data = detergent_DF)
summary(reg.t64.1)

# The estimates for Tide 128 make sense.  Own price elasticity is negative and corss price elasticity is positive.
# The magnititue of own price elasticity is -4.6, which seems high.

# The estimates for tide 64 don't make sense.  Cross price elasticity with Wisk is negative.

####################################################
################## Part 3 ##########################

# Add time trend to regression
reg.t128.2 <- lm(log(v_tide128) ~ log(p_tide128) + log(p_tide64) + log(p_wisk64) + week, data = detergent_DF)
summary(reg.t128.2)
reg.t64.2 <- lm(log(v_tide64) ~ log(p_tide64) + log(p_tide128) + log(p_wisk64) + week, data = detergent_DF)
summary(reg.t64.2)

# Adding a time trend to the regression is important because demand may be increasing or decreasing over time.
# If the change in demand over time correlates with price changes, it could cause ommitted variable bias.

# Demand for both tide products seems to be decreasing over time.

# The estimates for Tide 128 make sense.  Own price elasticity is negative and corss price elasticity is positive.
# The magnititue of own price elasticity increased to -4.8, which seems high.

# The estimates for Tide 24 make more sense.  Own price elasticity is negative and corss price elasticity is positive.
# The magnititue of own price elasticity is -3.7, which seems more reasonable.

####################################################
################## Part 4 ##########################

# Create new dataset with only non-promotion data
detergent_DF_2 <- subset(detergent_DF, promoflag == 0)

# Run regression on data without promotions
reg.t128.3 <- lm(log(v_tide128) ~ log(p_tide128) + log(p_tide64) + log(p_wisk64) + week, data = detergent_DF_2)
summary(reg.t128.3)
reg.t64.3 <- lm(log(v_tide64) ~ log(p_tide64) + log(p_tide128) + log(p_wisk64) + week, data = detergent_DF_2)
summary(reg.t64.3)

# The estimates for Tide 128 make less sense now.  Cross price elasticity with Tide 64 is now negative, when it should be positive.
# The own price elasticity has decreased to -3.5, which is more reasonable.

# The estimates for Tide 64 still do not make sense.  The cross price elasticity of Wisk 64 is negative.

####################################################
################## Part 5 ##########################

# Add store fixed effects to model
reg.t128.4 <- lm(log(v_tide128) ~ log(p_tide128) + log(p_tide64) + log(p_wisk64) + week + factor(store) - 1, data = detergent_DF_2)
summary(reg.t128.4)
reg.t64.4 <- lm(log(v_tide64) ~ log(p_tide64) + log(p_tide128) + log(p_wisk64) + week + factor(store) - 1, data = detergent_DF_2)
summary(reg.t64.4)

# For Tide 128, the direction of each elasticity makes sense.  The own price elasticity is -2.4, which seems very reasonable.
# The cross price elasticity for Tide 64 is not statistically significantly differnt from zero.
# The cross price elasticity for Wisk 64 is positive and statsiticaly significant.
# I would expect the opposite because Tide 64 should be a better substitute for Tide 128 than Wisk 64 is.

# For Tide 64, the cross price elasticity for Wisk 64 is negative, which does not make sense.

####################################################
################## Part 6 ##########################

# Calculate average prices
base_p_t128 <- mean(detergent_DF_2$p_tide128)
base_p_t64 <- mean(detergent_DF_2$p_tide64)
# Note, these prices are not weighted by ACV, per the instructions

# Calculate base volume 
base_q_t128 <- 86 * 52 * mean(detergent_DF_2$q_tide128)
base_q_t64 <- 86 * 52 * mean(detergent_DF_2$q_tide64)

# Part 1: New Total Volumes
# (a) A simultaneous 5 percent increase in the prices of Tide 128 and Tide 64
# (b) A simultaneous 5 percent decrease in the prices of Tide 128 and Tide 64
# (c) A simultaneous 5 percent increase in the price of Tide 128 and 5 percent decrease in the price of Tide 64
# (d) A simultaneous 5 percent decrease in the price of Tide 128 and 5 percent increase in the price of Tide 64

# Define Tide 128 to be product 1, Tide 64 to be product 2, and Wisk 64 to be product 3 

# Save beta cofficients in their own variables
beta11 <- summary(reg.t128.4)$coefficients[1,1]
beta12 <- summary(reg.t128.4)$coefficients[2,1]
beta13 <- summary(reg.t128.4)$coefficients[3,1]

beta22 <- summary(reg.t128.4)$coefficients[1,1]
beta21 <- summary(reg.t128.4)$coefficients[2,1]
beta23 <- summary(reg.t128.4)$coefficients[3,1]

# New Quantity of Tide128 = (1 + Price change for Tide128)^beta11 * (1 + Price change for Tide64)^beta12 * Old Quantity of Tide128

# Create matrix that will be used to calculate all price and volume changes
scenario <- c("Base", "a", "b", "c", "d")
p_change_t128 <- c(0,0.05,-0.05,0.05,-0.05)
p_change_t64 <- c(0,0.05,-0.05,-0.05,+0.05)
price_change_results <- data.frame(scenario, p_change_t128, p_change_t64)

# calculate new sales volumes
price_change_results$q_tide128 <- (1+price_change_results$p_change_t128)^beta11 * (1+price_change_results$p_change_t64)^beta12 * base_q_t128
price_change_results$q_tide64 <- (1+price_change_results$p_change_t128)^beta21 * (1+price_change_results$p_change_t64)^beta22 * base_q_t64

# Part 2: New Profits

retail_margin <- 0.25
variable_cost <- 0.027

# New Profit = New Quantity(New Price*(1-retail_margin) - variable_cost)

price_change_results$profit_tide128 <- price_change_results$q_tide128 * (
                                              base_p_t128*(1+price_change_results$p_change_t128)*(1-retail_margin) 
                                              - variable_cost*128
                                              )

price_change_results$profit_tide64 <- price_change_results$q_tide64 * (
                                            base_p_t64*(1+price_change_results$p_change_t64)*(1-retail_margin) 
                                            - variable_cost*64
                                            )



# Create columns of base profits
price_change_results$base_profit_t128 <- price_change_results$profit_tide128[1]
price_change_results$base_profit_t64 <- price_change_results$profit_tide64[1]

# Calculate total profit change
price_change_results$profit_change <- (price_change_results$profit_tide128 + price_change_results$profit_tide64
                                          - price_change_results$base_profit_t128 - price_change_results$base_profit_t64)

# Create a dataframe excluding the base price row
price_change_results_2 <- price_change_results[-1,c("p_change_t64", "p_change_t128", "profit_change")]


# Transpose data to make a 2x2 of profit changes under all 4 price scenarios
profit_changes = reshape(price_change_results_2,
                       timevar = "p_change_t128",
                       idvar = "p_change_t64",
                       direction = "wide")
colnames(profit_changes) = c("p_change_t64", "p_t128_increase_of_5%", "p_t128_decrease_of_5%")

####################################################
################## Part 7 ##########################

