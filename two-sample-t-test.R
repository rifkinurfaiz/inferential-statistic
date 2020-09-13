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
