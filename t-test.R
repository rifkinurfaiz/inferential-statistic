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