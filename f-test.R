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
#  Solution 2: Using manual calculation   #
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