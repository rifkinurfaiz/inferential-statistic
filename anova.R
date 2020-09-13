#################################
#                               #
#            ANOVA              #
#                             ` #
#################################

# We use ANOVA if we have to comparing mean of more than 2 samples
# To compare mean, we have to look at it's variance
# We look the variance of each sample whether it is massive different with other sample or not

# SST = SSbetween(treatment) + SSwithin(error) (SS = Sum of Square)
# Ratio = SSbetween(treatment) / SSwithin(error)
# MS = SS / df
# F = MSbetween(treatment) / MSwithin(error)
machine1 <- c(150, 151, 152, 152, 151, 150)
machine2 <- c(153, 152, 148, 151, 149, 152)
machine3 < c(156, 154, 155, 156, 157, 155)