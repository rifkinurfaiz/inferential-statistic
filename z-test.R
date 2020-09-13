#####################
#                   #
# One Sample Z Test #
#                   #
#####################
library(readr)

perfume_volume <- read.csv('~/Documents/IDP/Data Science/Inferential Statistic/Data/original.csv')

# Exercise: Bottles are being produced with mean as 150cc and SD 2cc.
#           Sample of bottles show the mean as 152. Has the mean volume increase?
#           Check with 95% confidence level

# So H0: mean <= 150; H1: mean > 150cc


###########################################       z = (xbar - mu) / (sd / sqrt(n))
#  Solution 1: Using standard calculation #
###########################################
perfume_volume_mean <- mean(perfume_volume$Machine.1)

z_value <- (perfume_volume_mean-150)/(2/sqrt(100)) # 10
z_critical <- qnorm(0.05) # 1.644

# Conclusion: Because z value is way greater than z critical, 
#             then we reject H0 and accept H1: the mean volume increase


####################################
#  Solution 2: Using library BSDA  #
####################################
library(BSDA)

z.test(x = Perfume_volume$Machine.1, alternative = 'greater', mu = 150, sigma.x = 2)

# Conclusion: From code above we find that p value is really small = 2.2e-16, 
#             hence we reject H0 and accept H1: the mean volume increase
