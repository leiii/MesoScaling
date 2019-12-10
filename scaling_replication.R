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
library(ggplot2)

setwd("~/Dropbox/public/mesoscaling") # replace to your work dir


rstlist <- list()
citylist <- c("beijing", "shanghai", "chengdu", "nanjing", "zhengzhou",
              "hangzhou", "suzhou", "jinan", "shenzhen", "xian")

for (i in 1:10){
  city = citylist[i]
  m1 <- read.table(paste0(city, "_2km_population_area.csv"), sep = ",", header = TRUE)
  m2 <- read.table(paste0(city, "_2km_population_firm.csv"), sep = ",", header = TRUE)
  m3 <- read.table(paste0(city, "_2km_population_poi.csv"), sep = ",", header = TRUE)
  m4 <- read.table(paste0(city, "_2km_population_flow.csv"), sep = ",", header = TRUE)
  
  model.footprint <- summary(lm(log10(footprint) ~ log10(active), data = m1))
  model.area <- summary(lm(log10(total) ~ log10(active), data = m1))
  model.density <- summary(lm(log10(total) ~ log10(active) + log10(footprint), data = m1))
  
  model.area.day <- summary(lm(log10(total) ~ log10(day_adj), data = m1[m1$day_adj > 1000,]))
  model.area.night <- summary(lm(log10(total) ~ log10(night_adj), data = m1[m1$night_adj > 1000,]))
  model.firm <- summary(lm(log10(cnt) ~ log10(active), data = m2))
  model.firm.day <- summary(lm(log10(cnt) ~ log10(day_adj), data = m2[m2$day_adj > 1000,]))
  model.firm.night <- summary(lm(log10(cnt) ~ log10(night_adj), data = m2[m2$night_adj > 1000,]))
  model.poi <- summary(lm(log10(cnt) ~ log10(active), data = m3))
  model.poi.day <- summary(lm(log10(cnt) ~ log10(day_adj), data = m3[m3$day_adj > 1000,]))
  model.poi.night <- summary(lm(log10(cnt) ~ log10(night_adj), data = m3[m3$night_adj > 1000,]))
  
  popu.density.coef <- model.density$coefficients[2,1]
  popu.density.std <- model.density$coefficients[2,2]
  footprint.density.coef <- model.density$coefficients[3,1]
  footprint.density.std <- model.density$coefficients[3,2]
  density.r2 <- model.density$r.squared
  m1$y_hat <- model.density$coefficients[1,1] + popu.density.coef * log10(m1$active) + footprint.density.coef * log10(m1$footprint)
  
  
  footprint.coef <- model.footprint$coefficients[2,1]
  footprint.std <- model.footprint$coefficients[2,2]
  footprint.r2 <- model.footprint$r.squared
  area.coef <- model.area$coefficients[2,1]
  area.std <- model.area$coefficients[2,2]
  area.r2 <- model.area$r.squared
  area.day.coef <- model.area.day$coefficients[2,1]
  area.day.std <- model.area.day$coefficients[2,2]
  area.day.r2 <- model.area.day$r.squared
  area.night.coef <- model.area.night$coefficients[2,1]
  area.night.std <- model.area.night$coefficients[2,2]
  area.night.r2 <- model.area.night$r.squared
  firm.coef <- model.firm$coefficients[2,1]
  firm.std <- model.firm$coefficients[2,2]
  firm.r2 <- model.firm$r.squared
  firm.day.coef <- model.firm.day$coefficients[2,1]
  firm.day.std <- model.firm.day$coefficients[2,2]
  firm.day.r2 <- model.firm.day$r.squared
  firm.night.coef <- model.firm.night$coefficients[2,1]
  firm.night.std <- model.firm.night$coefficients[2,2]
  firm.night.r2 <- model.firm.night$r.squared
  
  poi.coef <- model.poi$coefficients[2,1]
  poi.std <- model.poi$coefficients[2,2]
  poi.r2 <- model.poi$r.squared
  poi.day.coef <- model.poi.day$coefficients[2,1]
  poi.day.std <- model.poi.day$coefficients[2,2]
  poi.day.r2 <- model.poi.day$r.squared
  poi.night.coef <- model.poi.night$coefficients[2,1]
  poi.night.std <- model.poi.night$coefficients[2,2]
  poi.night.r2 <- model.poi.night$r.squared
  
  flow.exponent <- c()
  flow.r2 <- c()
  for (k in c(3:19)){
    model.tmp <- summary(lm(log10(m4[,k]) ~ log10(m4$popu)))
    flow.exponent[k-2] <- model.tmp$coefficients[2,1]
    flow.r2[k-2] <- model.tmp$r.squared
  }
  
  
  tmp <- c(city, nrow(m2), footprint.coef, footprint.std, footprint.r2,
           area.coef, area.std, area.r2, 
           popu.density.coef, popu.density.std, footprint.density.coef, footprint.density.std, density.r2,
           firm.coef, firm.std, firm.r2, poi.coef, poi.std, poi.r2,
           area.day.coef, area.day.std, area.day.r2, area.night.coef, area.night.std, area.night.r2,
           firm.day.coef, firm.day.std, firm.day.r2, firm.night.coef, firm.night.std, firm.night.r2,
           poi.day.coef, poi.day.std, poi.day.r2, poi.night.coef, poi.night.std, poi.night.r2,
           flow.exponent, flow.r2)
  
  rstlist[[i]] <- tmp
  
  #plot
  #population area
  p <- ggplot() +
    geom_point(aes(x = log10(day_adj), y = log10(total)), data = m1, 
               color = "darkorange", size = 2, alpha = 0.6) +
    geom_point(aes(x = log10(night_adj), y = log10(total)), data = m1, 
               color = "slateblue", size = 2, alpha = 0.6) +
    geom_point(aes(x = log10(active), y = log10(total)), data = m1, 
               color = "black", size = 2, alpha = 0.6) +
    geom_abline(intercept = model.area$coefficients[1,1], 
                slope = model.area$coefficients[2,1], 
                size = 1.5, alpha = 0.8) + 
    geom_abline(intercept = model.area$coefficients[1,1], 
                slope = 1, linetype="dashed",
                size = 1.5, alpha = 0.6) + 
    xlab("log10(Population)") + ylab("log10(Area)") + 
    ylim(5, 7) + 
    scale_x_continuous(breaks = c(3, 3.5, 4, 4.5, 5, 5.5), limits = c(3,5.5)) + 
    theme_classic(base_size = 16)
  p
  #ggsave(paste0(city, "_area.pdf"), width = 4, height = 4)
  
  
  # CD predict
  p <- ggplot() +
    geom_point(aes(x = y_hat, y = log10(total)), data = m1, 
               size = 2, alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, size = 1.5, alpha = 0.8, color = "red") + 
    xlab("Predicted Infrastrcture") + ylab("Observed Infrastructure") + 
    ylim(5, 7) + 
    xlim(5, 7) + 
    theme_classic(base_size = 16)
  p
  #ggsave(paste0(city, "_CD_predict.pdf"), width = 4, height = 4)
  
  
  # population firm
  p <- ggplot() +
    geom_point(aes(x = log10(day_adj), y = log10(cnt)), data = m2, 
               color = "darkorange", size = 2, alpha = 0.6) +
    geom_point(aes(x = log10(night_adj), y = log10(cnt)), data = m2, 
               color = "slateblue", size = 2, alpha = 0.6) +
    geom_point(aes(x = log10(active), y = log10(cnt)), data = m2, 
               color = "black", size = 2, alpha = 0.6) +
    geom_abline(intercept = model.firm$coefficients[1,1], 
                slope = model.firm$coefficients[2,1], 
                size = 1.5, alpha = 0.8) + 
    geom_abline(intercept = model.firm$coefficients[1,1], 
                slope = 1, linetype="dashed",
                size = 1.5, alpha = 0.6) + 
    xlab("log10(Population)") + ylab("log10(Firm)") + 
    scale_x_continuous(breaks = c(3, 3.5, 4, 4.5, 5, 5.5), limits = c(3,5.5)) + 
    theme_classic(base_size = 16)
  p
  #ggsave(paste0(city, "_firm.pdf"), width = 4, height = 4)
  
  
  # population, POI
  p <- ggplot() +
    geom_point(aes(x = log10(day_adj), y = log10(cnt)), data = m3, 
               color = "darkorange", size = 2, alpha = 0.6) +
    geom_point(aes(x = log10(night_adj), y = log10(cnt)), data = m3, 
               color = "slateblue", size = 2, alpha = 0.6) +
    geom_point(aes(x = log10(active), y = log10(cnt)), data = m3, 
               color = "black", size = 2, alpha = 0.6) +
    geom_abline(intercept = model.poi$coefficients[1,1], 
                slope = model.poi$coefficients[2,1], 
                size = 1.5, alpha = 0.8) + 
    geom_abline(intercept = model.poi$coefficients[1,1], 
                slope = 1, linetype="dashed",
                size = 1.5, alpha = 0.6) + 
    xlab("log10(Population)") + ylab("log10(POI)") + 
    scale_x_continuous(breaks = c(3, 3.5, 4, 4.5, 5, 5.5), limits = c(3,5.5)) + 
    theme_classic(base_size = 16)
  p
  #ggsave(paste0(city, "_poi.pdf"), width = 4, height = 4)
  
}


### summary stat
rst <- as.data.frame(do.call(rbind, rstlist))
colnames(rst) <- c("city", "N", "footprint.coef", "footprint.std", "footprint.r2", "area.coef", "area.std", "area.r2", 
                   "popu.density.coef", "popu.density.std", "footprint.density.coef", "footprint.density.std", "density.r2",
                   "firm.coef", "firm.std", "firm.r2", "poi.coef", "poi.std", "poi.r2",
                   "area.day.coef", "area.day.std", "area.day.r2", "area.night.coef", "area.night.std", "area.night.r2",
                   "firm.day.coef", "firm.day.std", "firm.day.r2", "firm.night.coef", "firm.night.std", "firm.night.r2",
                   "poi.day.coef", "poi.day.std", "poi.day.r2", "poi.night.coef", "poi.night.std", "poi.night.r2",
                   'm1', 'm1.25', 'm1.5', 'm1.75', 'm2', 'm2.25', 'm2.5', 'm2.75', 'm3','m3.25', 'm3.5', 'm3.75', 'm4', 'm4.25', 'm4.5', 'm4.75', 'm5',
                   'm1r2', 'm1.25r2', 'm1.5r2', 'm1.75r2', 'm2r2','m2.25r2', 'm2.5r2', 'm2.75r2', 'm3r2','m3.25r2', 'm3.5r2', 'm3.75r2', 'm4r2','m4.25r2', 'm4.5r2', 'm4.75r2', 'm5r2')
rst[ ,c(2:ncol(rst))] <- apply(rst[, c(2:ncol(rst))], 2, function(x) as.numeric(as.character(x)))




### I = P * A
### coef
#summary(lm(footprint.density.coef ~ popu.density.coef, data = rst))
p <- ggplot(aes(x = popu.density.coef, y = footprint.density.coef, label = city), data = rst) +
  geom_point(size = 3, alpha = 0.8) + geom_text() +
  geom_abline(intercept = 1, slope = -1, size = 1.5, alpha = 0.8, color = "red") + 
  geom_vline(xintercept = mean(rst$popu.density.coef), linetype = "dashed", size = 1) +
  geom_hline(yintercept = mean(rst$footprint.density.coef), linetype = "dashed", size = 1) + 
  scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0,1)) +
  theme_classic(base_size = 16)
p
ggsave("scaling_CD_coef.pdf", width = 4, height = 4)


### r2
p <- ggplot(aes(x = density.r2, y = city), data = rst) +
  geom_point(aes(x = area.r2, y = city), color = "black", size = 3, alpha = 0.8) +
  geom_point(color = "red", size = 3, alpha = 0.8) +
  geom_vline(xintercept = mean(rst$area.r2), linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(rst$density.r2), color = "red", alpha = 0.8, linetype = "dashed", size = 1) + 
  scale_x_continuous(breaks = c(0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0.6, 1)) +
  theme_classic(base_size = 16)
p
ggsave("scaling_CD_r2.pdf", width = 4, height = 4)



### coef. of footprint area (A)
p <- ggplot(aes(x = footprint.coef, y = city), data = rst) +
  geom_point(color = "black", size = 3, alpha = 0.8) +
  geom_errorbarh(aes(xmin = footprint.coef - footprint.std, xmax = footprint.coef + footprint.std, height=0)) +
  geom_vline(xintercept = mean(rst$footprint.coef), linetype = "dashed", size = 1) + 
  scale_x_continuous(breaks = c(0.4, 0.6, 0.8, 1.0), limits = c(0.4,1.1)) +
  theme_classic(base_size = 16)
p
ggsave("scaling_footprint_coef_lambda_1_2.pdf", width = 4, height = 4)


### r2 of footprint area (A)
p <- ggplot(aes(x = footprint.r2, y = city), data = rst) +
  geom_point(color = "black", size = 3, alpha = 0.8) +
  geom_vline(xintercept = mean(rst$footprint.r2), linetype = "dashed", size = 1) + 
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0), limits = c(0.2, 1)) +
  theme_classic(base_size = 16)
p
ggsave("scaling_footprint_r2_lambda_1_2.pdf", width = 4, height = 4)



### coef. of building volume
p <- ggplot(aes(x = area.coef, y = city), data = rst) +
  geom_vline(xintercept = mean(rst$area.day.coef), color = "darkorange", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(rst$area.night.coef), color = "slateblue", linetype = "dashed", size = 1) + 
  geom_point(color = "black", size = 3, alpha = 0.8) +
  geom_errorbarh(aes(xmin = area.coef - area.std, xmax = area.coef + area.std, height=0)) +
  geom_vline(xintercept = mean(rst$area.coef), linetype = "dashed", size = 1) + 
  scale_x_continuous(breaks = c(0.4, 0.6, 0.8, 1.0), limits = c(0.4,1.1)) +
  theme_classic(base_size = 16)
p
ggsave("scaling_area_coef_2000.pdf", width = 4, height = 4)
mean(rst$area.coef)


### r2 of building volume
p <- ggplot(aes(x = area.r2, y = city), data = rst) +
  geom_point(color = "black", size = 3, alpha = 0.8) +
  geom_point(aes(x = area.day.r2, y = city), color = "darkorange", size = 3, alpha = 0.8, data =  rst) +
  geom_point(aes(x = area.night.r2, y = city), color = "slateblue", size = 3, alpha = 0.8, data =  rst) +
  geom_vline(xintercept = mean(rst$area.r2), linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(rst$area.day.r2), color = "darkorange", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(rst$area.night.r2), color = "slateblue", linetype = "dashed", size = 1) + 
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0), limits = c(0.2, 1)) +
  theme_classic(base_size = 16)
p
ggsave("scaling_area_r2_2000.pdf", width = 4, height = 4)
mean(rst$area.r2)


### coef. firm
p <- ggplot(aes(x = firm.coef, y = city), data = rst) +
  geom_vline(xintercept = mean(rst$firm.day.coef), color = "darkorange", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(rst$firm.night.coef), color = "slateblue", linetype = "dashed", size = 1) + 
  geom_point(color = "black", size = 3, alpha = 0.8) +
  geom_errorbarh(aes(xmin = firm.coef - firm.std, xmax = firm.coef + firm.std, height=0)) +
  geom_vline(xintercept = mean(rst$firm.coef), linetype = "dashed", size = 1) + 
  scale_x_continuous(breaks = c(1, 1.2, 1.4, 1.6), limits = c(1,1.6)) +
  theme_classic(base_size = 16)
p
ggsave("scaling_firm_coef_2000.pdf", width = 4, height = 4)
mean(rst$firm.coef)


### r2 of firm
p <- ggplot(aes(x = firm.r2, y = city), data = rst) +
  geom_point(color = "black", size = 3, alpha = 0.9) +
  geom_point(aes(x = firm.day.r2, y = city), color = "darkorange", size = 3, alpha = 0.8, data =  rst) +
  geom_point(aes(x = firm.night.r2, y = city), color = "slateblue", size = 3, alpha = 0.8, data =  rst) +
  geom_vline(xintercept = mean(rst$firm.r2), linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(rst$firm.day.r2), color = "darkorange", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(rst$firm.night.r2), color = "slateblue", linetype = "dashed", size = 1) + 
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0), limits = c(0.2, 1)) +
  theme_classic(base_size = 16)
p
ggsave("scaling_firm_r2_2000.pdf", width = 4, height = 4)
mean(rst$firm.r2)


### coef. of poi
p <- ggplot(aes(x = poi.coef, y = city), data = rst) +
  geom_vline(xintercept = mean(rst$poi.day.coef), color = "darkorange", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(rst$poi.night.coef), color = "slateblue", linetype = "dashed", size = 1) + 
  geom_point(color = "black", size = 3, alpha = 0.8) +
  geom_errorbarh(aes(xmin = poi.coef - poi.std, xmax = poi.coef + poi.std, height=0)) +
  geom_vline(xintercept = mean(rst$poi.coef), linetype = "dashed", size = 1) + 
  scale_x_continuous(breaks = c(1, 1.2, 1.4, 1.6, 1.7), limits = c(1,1.7)) +
  theme_classic(base_size = 16)
p
ggsave("scaling_poi_coef_1500.pdf", width = 4, height = 4)
mean(rst$poi.coef)


# r2 of poi
p <- ggplot(aes(x = poi.r2, y = city), data = rst) +
  geom_point(color = "black", size = 3, alpha = 0.9) +
  geom_point(aes(x = poi.day.r2, y = city), color = "darkorange", size = 3, alpha = 0.8, data =  rst) +
  geom_point(aes(x = poi.night.r2, y = city), color = "slateblue", size = 3, alpha = 0.8, data =  rst) +
  geom_vline(xintercept = mean(rst$poi.r2), linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(rst$poi.day.r2), color = "darkorange", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(rst$poi.night.r2), color = "slateblue", linetype = "dashed", size = 1) + 
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0), limits = c(0.2, 1)) +
  theme_classic(base_size = 16)
p
ggsave("scaling_poi_r2_2000.pdf", width = 4, height = 4)
mean(rst$poi.r2)


### gravity model
m.value <- c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25, 3.5, 3.75, 4, 4.25, 4.5, 4.75, 5)
m.mean <- apply(rst[, c(38:54)], 2, function(x) mean(x))
m.sd <- apply(rst[, c(38:54)], 2, function(x) sd(x))
m.r2 <- apply(rst[, c(55:71)], 2, function(x) mean(x))
m.df <- as.data.frame(cbind(m.value, m.mean, m.sd, m.r2))

summary(lm(m.mean~m.value, m.df[1:6,]))

p <- ggplot(aes(x = m.value, y = m.mean), data = m.df[1:13,]) +
  geom_point(color = "black", size = 3, alpha = 0.8) +
  geom_errorbar(aes(ymin = m.mean - m.sd, ymax = m.mean + m.sd, width = 0)) +
  geom_hline(yintercept = 7/6, linetype = "dashed", size = 1) + 
  geom_hline(yintercept = 4/3, linetype = "dashed", size = 1) + 
  ylim(1,1.8)+
  theme_classic(base_size = 16)
p
ggsave("gravity_m.pdf", width = 4, height = 4)


p <- ggplot(aes(x = m.value, y = m.r2), data = m.df) +
  geom_point(color = "black", size = 2, alpha = 0.8) +
  ylim(0.8,1)+
  theme_classic(base_size = 16)
p
ggsave("gravity_m_r2.pdf", width = 2.5, height = 2)


