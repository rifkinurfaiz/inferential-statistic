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

# Example: check thw following data if variance of the data is different from each other
# H0 = variance is not different, machine1 = machine2 = machine3
machine1 <- c(150, 151, 152, 152, 151, 150)
machine2 <- c(153, 152, 148, 151, 149, 152)
machine3 <- c(156, 154, 155, 156, 157, 155)

machines.volume <- c(machine1, machine2, machine3)

constants <- rep(c("machine1", "machine2", "machine3"),
                 times=c(length(machine1), length(machine2), length(machine3)))

constructed.data <- data.frame(machines.volume, constants)

anovaResult <- aov(data = constructed.data, formula = machines.volume ~ constants)
summaryOfAnova <- summary(anovaResult)

# RESULT summaryOfAnova
#              Df   Sum Sq            Mean Sq      F value   Pr(>F)    
# constants    2    (between) 84.11   42.06        22.27     3.24e-05 ***
# Residuals    15   (within) 28.33    1.89 

# Since F > P value, then we reject H0, so there is different in variance of data

#We can make sure the result by looking at the boxplot
boxplot(machine1, machine2, machine3)

#Other way we can make sure the result using Tueky Honest Significance Difference
TukeyHSD(x = anovaResult)

                    #diff     lwr       upr       p adj
#machine2-machine1 -0.1666667 -2.227739 1.894405 0.9760110
#machine3-machine1  4.5000000  2.438928 6.561072 0.0001241
#machine3-machine2  4.6666667  2.605595 6.727739 0.0000846

# We see p value of machine3-machine1 and machine3-machine2 is low.
# So we can conslude that machine 3 is having different variance

