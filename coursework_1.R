setwd('~/R/statistics_for_geoscientists/Coursework 1/')
library(geostats)
library(tidyverse)

# locations.csv: a table with the eastings and northings of n samples as well as the local slope (S) and
# the number of trees (T) counted over a hectare at each location.
locations <- read.csv('data/locations.csv')

# kappa.csv: a table with m rows and n columns containing m replicate measurements of the erodibility
# (κ, in t/[yr·ha]), which is a parameter that depends on the soil composition and includes factors
# such as the sand, silt and clay fraction, as well as the organic content of the soil.
kappa <- read.csv('data/kappa.csv')

# Erosion rates can be modelled (i.e. predicted) using the following formula: ϵ = κS/T


# Part 1: KDE -------------------------------------------------------------

op <- par(mfrow = c(2,2))
for (i in 1:4) {
  dens <- density(kappa[,i])
  plot(dens, xlab = 'kappa', main = paste0("Sample ", i))
}
par(op)



# Part 2: Boxplots --------------------------------------------------------

stacked_kappa <- pivot_longer(kappa, sample_1:sample_90)
boxplot(stacked_kappa$value ~ stacked_kappa$name, xaxt = 'n', xlab = 'Sample', ylab = 'Kappa', main = 'Summary of Samples')




# Part 4: Summary Stats ---------------------------------------------------

kappa_st_err = rep(0, ncol(kappa))
kappa_mean = rep(0, ncol(kappa))

for (i in 1:ncol(kappa)) {
  kappa_st_err[i] = sd(kappa[,i]) / sqrt(length(kappa[,i]))
  kappa_mean[i] = mean(kappa[,i])
}

