#################################
#                               #
#       Two Sample Z Test       #
#                             ` #
#################################
library(BSDA)
library(readr)
volume_two_sample <- read_csv('~/Documents/IDP/Data Science/Inferential Statistic/Data/bottle_volume_2_sample.csv')

# Example: 2 machines volume data taken and want to check whether there is significant difference of mean
#          volume of machine

# H0: Mean of 2 machine is same; H1: Mean of 2 machine is different

z.test(x = volume_two_sample$`Machine 1`,
       y = volume_two_sample$`Machine 2`, 
       sigma.x = sd(volume_two_sample$`Machine 1`),
       sigma.y = sd(volume_two_sample$`Machine 2`)
)

# Conclusion: From code above, we find that p value is 0.0003238, which is less than 5%
#             so we reject H0, means that mean volume of 2 machine is different



# Example: 2 machines volume data taken and want to check whether 
#          the difference of mean volume of two machine is not equal to -1

# H0: Mean difference of 2 machine is equal to -1; 
# H1: Mean difference of 2 machine is not equal to -1

z.test(x = volume_two_sample$`Machine 1`,
       y = volume_two_sample$`Machine 2`, 
       sigma.x = sd(volume_two_sample$`Machine 1`),
       sigma.y = sd(volume_two_sample$`Machine 2`),
       mu = -1
)

# Conclusion: From code above, we find that p value is 0.942, which is more than 5%
#             so we accept H0, means that mean volume of 2 machine is equal to -1

# Visualize
boxplot(x = volume_two_sample)

hist(x = volume_two_sample$`Machine 1`,
     col = rgb(1,0,0,0.5), 
     xlim = c(140,160), 
     ylim = c(0,25), 
     xlab = 'Volume',
     main = 'Volume of machine 1 and machine 2')
hist(x = volume_two_sample$`Machine 2`, col = rgb(0,0,1,0.5), add = T)