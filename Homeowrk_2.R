#### 37301 Data Driven Marketing ######
#### Homework 2 #######

library(psych)
setwd("/Users/ben/Dropbox/Chicago Booth/37103 Data Driven Marketing/Homework 2/37103-HW2")
load("Detergent.RData")

####################################################
################## Part 1 ##########################

# Create variables of total revenue
detergent_DF$r_tide128 = detergent_DF$p_tide128 * detergent_DF$q_tide128
detergent_DF$r_tide64 = detergent_DF$p_tide64 * detergent_DF$q_tide64
detergent_DF$r_wish64 = detergent_DF$p_wisk64 * detergent_DF$q_wisk64

# Create variables for price differences
detergent_DF$p_t128_t64 = detergent_DF$p_tide128 - detergent_DF$p_tide64
detergent_DF$p_t64_w64 = detergent_DF$p_tide64 - detergent_DF$p_wisk64

describe(detergent_DF[,c("p_tide128","p_tide64","p_wisk64","p_t128_t64","p_t64_w64")])

hist(detergent_DF$p_t128_t64, main = "Histogram of Tide 128 and Tide 64 Price Difference", xlab = "Price Difference ($)", col = "blue")
hist(detergent_DF$p_t64_w64, main = "Histogram of Tide 64 and Wisk 64 Price Difference", xlab = "Price Difference ($)", col = "blue")

