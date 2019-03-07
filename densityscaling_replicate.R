# Author: Lei Dong

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

library(dplyr)
library(stringr)
library(ineq)
library(ggplot2)

setwd("~/Dropbox/research/density") # replace to your work dir

city <- "shanghai"

### Population scale factor
exponent <- NA
if (city == "beijing"){
  exponent <- 21.71 / 16.77 # Beijing
} else if (city == "shanghai") {
  exponent <- 23.45 / 15.27 # Shanghai
} else if (city == "chengdu"){
  exponent <- 9.58 / 8.92 # Chengdu
}



### Read files
popu.buil <- read.table(paste0("data/", city, "_popu_building.csv"), sep = ",", header = TRUE)
popu.dens <- read.table(paste0("data/", city, "_daynight_density.csv"), sep = ",", header = TRUE)
buil <- read.table(paste0("data/", city, "_building_v1.csv"), sep = ",", header = TRUE)
summary(buil)



### Supp Figure
ggplot(buil, aes(x=log10(floor))) + 
  geom_histogram(binwidth=0.1, color="black", fill="white") +
  theme_classic(base_size = 16)
#ggsave(paste0(city, "_hist.pdf"), width = 4.5, height = 4)



### Figure 1
p <- ggplot() +
  geom_point(aes(x = x, y = y, size = day), data = popu.buil, 
             color = "red", alpha = 0.6) +
  geom_point(aes(x = x, y = y, size = night), data = popu.buil, 
             color = "blue", alpha = 0.6) +
  scale_size(range = c(1, 10)) +
  theme_void()
p
#ggsave(paste0(city, "_distribution.pdf"), width = 5, height = 4)


### Table 1
m.gini <- popu.buil[popu.buil$day > 4000 & popu.buil$night > 4000 & popu.buil$footprint > 20000,]
gini.footprint <- ineq(m.gini$footprint, type="Gini")
gini.tfa <- ineq(m.gini$total, type="Gini")
gini.day <- ineq(m.gini$day, type="Gini")
gini.night <- ineq(m.gini$night, type="Gini")
gini.day.tfa <- ineq(m.gini$day/m.gini$total, type="Gini")
gini.night.tfa <- ineq(m.gini$night/m.gini$total, type="Gini")



### Decay curve
popu2 <- popu.dens
colnames(popu2) <- c("dist", "night", "day")

buil2 <- buil %>%
  group_by(as.integer(dist)) %>%
  summarise(footprint = sum(geom_area),
            total = sum(total_area))
colnames(buil2) <- c("dist", "footprint", "tfa")

dens <- merge(popu2, buil2, by = "dist")
dens$active <- dens$day * 1/3 + dens$night * 2/3

dens$daydens <- exponent * dens$day / 3.14 / (2*dens$dist + 1) / 1000
dens$nightdens <- exponent * dens$night / 3.14 / (2*dens$dist + 1) /1000
dens$activedens <- exponent * dens$active / 3.14 / (2*dens$dist + 1) /1000
dens$tfadens <- dens$tfa / 3.14 / (2*dens$dist + 1) 


### Daytime population
# exponentially
summary(lm(log(daydens) ~ I(dist+0.5), data = dens[dens$dist > 0,]))
# power
summary(lm(log(daydens) ~ log(dist+0.5), data = dens[dens$dist > 0,]))
# quadratic exponential 
summary(lm(log(daydens) ~ I(dist+0.5) + I((dist+0.5)^2), data = dens[dens$dist > 0,]))

### Nighttime population
# exponentially
summary(lm(log(nightdens) ~ I(dist+0.5), data = dens[dens$dist > 0,]))
# power
summary(lm(log(nightdens) ~ log(dist+0.5), data = dens[dens$dist > 0,]))
# quadratic exponential 
summary(lm(log(nightdens) ~ I(dist+0.5) + I((dist+0.5)^2), data = dens[dens$dist > 0,]))


# scale up to the whole population
dens$daydens_ <- exponent * dens$day / dens$tfa
dens$nightdens_ <- exponent * dens$night / dens$tfa
dens$activedens_ <- exponent * dens$active / dens$tfa

# statistical summary
dens.sub <- dens[dens$dist > 0,]
mean(dens.sub$activedens_)
summary(lm(dens.sub$activedens_ ~ dens.sub$dist))
summary(lm(log10(dens.sub$activedens) ~ log10(dens.sub$dist+0.5)))
cor.test(dens.sub$dist, dens.sub$activedens_)
cor.test(dens.sub$dist, dens.sub$nightdens_)


### Figure 2ABC
p <- ggplot() +
  geom_point(aes(x = dist + 0.5, y = daydens), data = dens, 
             color = "red", size = 3, alpha = 0.8) +
  geom_line(aes(x = dist + 0.5, y = daydens), data = dens, 
            color = "red", size = 1.5, alpha = 0.6) +
  geom_point(aes(x = dist + 0.5, y = nightdens), data = dens, 
             color = "blue", size = 3, alpha = 0.8) +
  geom_line(aes(x = dist + 0.5, y = nightdens), data = dens, 
            color = "blue", size = 1.5, alpha = 0.6) +
  geom_point(aes(x = dist + 0.5, y = activedens), data = dens, 
             size = 3, alpha = 0.8) +
  geom_line(aes(x = dist + 0.5, y = activedens), data = dens, 
            size = 1.5, alpha = 0.6) +
  xlab("Distance (km)") + ylab("Density [1000 people/km2]") + 
  scale_x_continuous(breaks = c(0, 5, 10, 15), limits = c(1, 15)) + 
  ylim(0, 65) +
  theme_classic(base_size = 16)
p
#ggsave(paste0(city, "_popu_density.pdf"), width = 4.5, height = 4)


### Figure 2DEF
p <- ggplot() +
  geom_point(aes(x = dist + 0.5, y = daydens_), data = dens, 
             color = "red", size = 3, alpha = 0.8) +
  geom_line(aes(x = dist + 0.5, y = daydens_), data = dens, 
            color = "red", size = 1.5, alpha = 0.6) +
  geom_point(aes(x = dist + 0.5, y = nightdens_), data = dens, 
             color = "blue", size = 3, alpha = 0.8) +
  geom_line(aes(x = dist + 0.5, y = nightdens_), data = dens, 
            color = "blue", size = 1.5, alpha = 0.6) +
  geom_point(aes(x = dist + 0.5, y = activedens_), data = dens, 
             size = 3, alpha = 0.8) +
  geom_line(aes(x = dist + 0.5, y = activedens_), data = dens, 
            size = 1.5, alpha = 0.6) +
  xlab("Distance (km)") + ylab("Density [TFA, people/m2]") + 
  scale_x_continuous(breaks = c(0, 5, 10, 15), limits = c(1, 15)) + 
  ylim(0, 0.04) +
  theme_classic(base_size = 16)
p
#ggsave(paste0(city, "_popu_tfa_density.pdf"), width = 4.5, height = 4)



### Figure 2 inserts
p <- ggplot() +
  geom_point(aes(x = dist + 0.5, y = tfadens/1000000), data = dens, 
             size = 3, alpha = 0.4) +
  geom_line(aes(x = dist + 0.5, y = tfadens/1000000), data = dens, 
            size = 1.5, alpha = 0.4) +
  xlab("Distance (km)") + ylab("TFA Density") + 
  scale_x_continuous(breaks = c(0, 5, 10, 15), limits = c(1, 15)) + 
  theme_classic(base_size = 14)
p
#ggsave(paste0(city, "_tfa_density.pdf"), width = 2, height = 2)



### Figure 3 Scaling
# regression
model.footprint <- lm(log10(footprint) ~ log10(active), data = popu.buil)
model.tfa <- lm(log10(total) ~ log10(active), data = popu.buil)
summary(model.footprint)
summary(model.tfa)

p <- ggplot() +
  geom_point(aes(x = log10(active), y = log10(total)), data = popu.buil, 
             color = "red", size = 2, alpha = 0.6) +
  geom_abline(intercept = model.tfa$coefficients[1], 
              slope = model.tfa$coefficients[2], 
              color = "red",
              size = 1.5) + 
  xlab("log10(Active Population)") + ylab("log10(Total Floor Area)") + 
  ylim(5.5, 7) + 
  scale_x_continuous(breaks = c(4, 4.5, 5), limits = c(4,5.1)) + 
  theme_classic(base_size = 16)
p
#ggsave(paste0(city, "_tfa_scaling.pdf"), width = 4.5, height = 4)



p <- ggplot() +
  geom_point(aes(x = log10(active), y = log10(footprint)), data = popu.buil, 
             color = "blue", size = 2, alpha = 0.6) +
  geom_abline(intercept = model.footprint$coefficients[1], 
              slope = model.footprint$coefficients[2], 
              color = "blue",
              size = 1.5) + 
  xlab("log10(Active Population)") + ylab("log10(Footprint Area)") + 
  ylim(5, 6.5) + 
  scale_x_continuous(breaks = c(4, 4.5, 5), limits = c(4,5.1)) + 
  theme_classic(base_size = 16)
p
#ggsave(paste0(city, "_fa_scaling.pdf"), width = 4.5, height = 4)

