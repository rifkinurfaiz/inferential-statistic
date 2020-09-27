#################################
#                               #
#       Goodness of Fit         #
#                             ` #
#################################

# To check if result of trial is part of a distribution

# Example 1
# If we flip coin 100 times and we goet 40 heads and 60 tails, is this result of trial bias?
# H0: result is not bias
# H1: result is bias

#####################
#   Using library   #
#####################
chisq.test(x=c(40, 60), p = c(0.5, 0.5))

# From above, we get p value 0.045, since it is less then 0.05 then we reject H0, means result is bias

################################
#   Using manual calculation   #
################################

# Calculate chi square
chi_square <- ((40-50)^2/50 + ((60-50)^2/50))
qchisq(p = 0.05, df = 1, lower.tail = F)

# Visualize
library(visualize)
visualize.chisq(stat = 4, df = 1, section = 'upper')


# Example 2
# Shirt manufacturing company expects the proportion of sale as follows:
# Small - 20%, medium - 40%, large - 30%, extra large - 10%
# Actual sale small - 211, medium - 402, large - 297, extra large - 80
# Is there a significants difference between expected and actual?

# H0: there is no significant difference
# H1: there is significant difference
shirt.expected <- c(0.2, 0.4, 0.3, 0.1)
shirt.actual <- c(211, 402, 297, 80)

chisq.test(x = shirt.actual, p = shirt.expected)

# From above, we get p value 0.2 which is less than 0.5, then we accept H0,
# Means there is no significant difference between expected and actual