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