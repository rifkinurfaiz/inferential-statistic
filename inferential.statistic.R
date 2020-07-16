#####################
#                   #
# One Sample Z Test #
#                   #
#####################
library(readr)

perfume_volume <- read.csv('~/Documents/IDP/Data Science/Data/original.csv')

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




#####################           t = (xbar - mu) / (sd of sample / sqrt(number of sample))
#                   #
# One Sample T Test #
#                   #
#####################
q <-seq(-4.0, 4.0, by=0.1)

dt(q, 19)

plot(q, dt(q, 19), type='l', lty='solid', xlab='t', ylab='f(t)')
lines(q, dt(q, 9), type='l', lty='dashed')
lines(q, dt(q, 3), type='l', lty='dotted')

# Example: Bottles are being produced with mean as 150cc and population SD is unknown
#          Sample of 4 bottles show the volume  as: 151, 153, 152, 152.
#          Has the volume change?
#          Check with 95% confidence

# So, H0: mean = 150cc; H1: mean < 150 cc || mean > 150 cc

library(visualize)

volume <- c(151, 153, 152, 152)
t.test(volume, mu = 150, conf.level = 0.95)

visualize.t(stat = c(-4.899, 4.899), df = 3, section = 'tails')

# Conclusion: From code above, we find that p value is 0.0163, which is less than 5%
#             hence we reject H0, and accept H1, means the volume of machine changes




###########################################     X^2 = (n - 1) * S^2 / sigma^2
#                                         #
#  One Sample Variance Test (Chi square)  #
#                             `           #
###########################################
library(readr)

perfume_volume <- read_csv('~/Documents/IDP/Data Science/Data/bottle_volume.csv')
perfume_volume_variance <- var(perfume_volume$Volumes)

# Example: 25 bottles were selected and their variance was 5.
#          Has the variance increased from the historical 4 variance? Check with 95% confidence


###########################################
#  Solution 1: Using standard calculation #
###########################################
chi_square <- (25-1)*var(perfume_volume$Volumes)/4 # 30
chi_square_critical <- qchisq(p = 0.05, df = 24, lower.tail = F) # 36.42

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




#################################
#                               #
#       Two Sample Z Test       #
#                             ` #
#################################
library(BSDA)
library(readr)
volume_two_sample <- read_csv('~/Documents/IDP/Data Science/Data/bottle_volume_2_sample.csv')

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




#################################
#                               #
#       Two Sample T Test       #
#                             ` #
#################################

# Example: Is there difference in means of the following sample data of machines

# Case 1: equal variance
machine1 <- c(150, 152, 154, 152, 151)
machine2 <- c(156, 155, 158, 155, 154)

var.test(x = machine1, y = machine2) # variance not equal

t.test(x = machine1, y = machine2, var.equal = T)

boxplot(machine1, machine2)

# Case 2: not equal variance
machine1 <- c(150, 152, 154, 152, 151)
machine3 <- c(144, 162, 177, 150, 140)

var.test(x = machine1, y = machine3) # variance equal

t.test(machine1, machine3, var.equal = F)

boxplot(machine1, machine3)




#################################   t = (mean of after - before) / (sd of after - before / sqrt(number of sample))
#                               #
#       Paired T Test           #
#                             ` #
#################################

# Use this method when we have dependent observation, like data of before and after

# Example: Medicine is consume by patients. 5 sample of blood pressure of patients
#          taken before and after consuming medicine. Does medicine affect blood presure changes?
#          before: 120, 122, 143, 100, 109
#          after: 122, 120, 141, 109, 109

# H0: Medicine does not affect blood pressure
# H1: Medicine does affect blood pressure

blood_pressure_before <- c(120, 122, 143, 100, 109)
blood_pressure_after <- c(122, 120, 141, 109, 109)
blood_pressure_diff <- blood_pressure_after - blood_pressure_before

###########################################
#  Solution 1: Using standard calculation #
###########################################
t_value <- mean(blood_pressure_diff) / (sd(blood_pressure_diff) / sqrt(5)) # 0.69
t_critical <- abs(qt(0.025, 4)) # 2.8

# Since t value is inside of t critical, then we fail to reject H0.


###########################################
#  Solution 2: Using built in library     #
###########################################
t.test(x = blood_pressure_before, y = blood_pressure_after, paired = T)

boxplot(blood_pressure_diff, main = "Effect of medicine to blood pressure", ylab = "Post medicine blood pressure")

# Conclusion: From code above, we find that p value is 0.53, which is more than 5%
#             so we accept H0, means that the medicine does not affect blood pressure




#################################
#                               #
#            F Test             #
#                             ` #
#################################

# Use this method to analyze variance from two different samples

# Example: We took 8 samples from machine A and the standard deviation was 1.1
#          From machine B we randomly picked 5 samples and the variance was 11.
#          Is there any difference in the variance for machine A and B
#          Check with 90% confidence

# H0: Variance machine a = variance machine b
# H1: Variance machine a != variance machine b

machine_a <- c(150, 150, 151, 149, 151, 151, 148, 151)
sd(machine_a)
var(machine_a)


machine_b <- c(152, 146, 152, 150, 155)
sd(machine_b)
var(machine_b)

###########################################
#  Solution 1: Using built in library     #
###########################################
var.test(x = machine_b, y = machine_a, ratio = 1, conf.level = 0.9) # Ratio = 1, means if var same then represent with 1

# Conclusion: From code above, we find that p value is 0.015, which is less than 10%
#             so we reject H0, means that variance not same


###########################################
#  Solution 1: Using manual calculation   #
###########################################
var_machine_b <- var(machine_b)
var_machine_a <- var(machine_a)

# Since variance of b > a, put b as denominatior
f_value <- var_machine_b/var_machine_a

# p = 0.05 because 90% confidence and both tail
# df: degree of freedom (n - 1)
f_critical <- qf(p = 0.05, df1 = 4, df2 = 7, lower.tail = F)

# Plot
x <- seq(0, 10)
vartical_value = df(x, df1 = 4, df2 = 7)
plot(vartical_value, type = 'l', xlab = 'F value', ylab = 'Density')

abline(v = f_value)
text(f_value, 0.05, "Calculated")

abline(v = f_critical)
text(f_critical, 0.04, "Critical 90%")

# Conclusion: From code above, we find that f calculated far outside of f critical
#             So we reject H0




