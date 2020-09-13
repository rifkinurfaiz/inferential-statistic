###########################################     X^2 = (n - 1) * S^2 / sigma^2
#                                         #
#  One Sample Variance Test (Chi square)  #
#                             `           #
###########################################
library(readr)

perfume_volume <- read_csv('~/Documents/IDP/Data Science/Inferential Statistic/Data/bottle_volume.csv')
perfume_volume_variance <- var(perfume_volume$Volumes)

# Example: 25 bottles were selected and their variance was 5.
#          Has the variance increased from the historical 4 variance? Check with 95% confidence


###########################################
#  Solution 1: Using standard calculation #
###########################################
chi_square_df <- 25 -1
chi_square <- (chi_square_df)*var(perfume_volume$Volumes)/4 # 30
chi_square_critical <- qchisq(p = 0.05, df = chi_square_df, lower.tail = F) # 36.42

# Conclusion: Because chi_square value is less than chi_square_critical, 
#             then we accept H0, means variance has not increase

# Visualize
x <- seq(1, 50, by = 1)
y <- dchisq(x, 24)

plot(y, type='l', xlab='Chi Square', ylab="f(Chi Square)")
abline(v = 30)
text(30, 0.05, "Calculated")
abline(v = chi_square_critical)
text(chi_square_critical, 0.04, "Critical 95%")


#######################################
#  Solution 2: Using library EnvStats #
#######################################
library(EnvStats)

varTest(x = perfume_volume$Volumes, alternative = "greater", conf.level = 0.05, sigma.squared = 4) 
# Sigma squared is the historical variance (population)

# Conclusion: From code above, we find that p value is 0.1848, which is more than 5%
#             so we accept H0, means that variance has not increased