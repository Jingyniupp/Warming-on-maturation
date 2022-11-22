#This is the code for Niu, J., Huss, H., Vasemägi, A. & A. Gårdmark. 2022. Decades of warming alters maturation and reproductive investment in fish. Ecology.
#This code calculates the maturity ogive, probability of maturing and gonado-somatic index and reproduces figures in the publication. 
#When using this code, please cite the original publication as "Niu, J., Huss, H., Vasemägi, A. & A. Gårdmark. 2022. Decades of warming alters maturation and reproductive investment in fish. Ecology."


library(dplyr)
library(tibble)
library(data.table)
library(ggplot2)
library(gridExtra)
library(grid)
library(rje)


#"BT" = heated area, "FM" = unheated area 
#only female perch
BT8003_2d <- read.csv("BT8003_2d.csv")
FM8098_2d_f_m789 <- read.csv("FM8098_2d_f_m789.csv")

#selecting perch by cohorts to calculate ogive
BT8084_2d <- BT8003_2d %>% filter (birth.year %in% 1980:1984) #1667 obs
BT9196_2d <- BT8003_2d %>% filter (birth.year %in% 1991:1996)
FM8084_2d  <- FM8098_2d_f_m789 %>% filter (birth.year %in% 1980:1984) #580 obs
FM9196_2d <- FM8098_2d_f_m789 %>% filter (birth.year %in% 1991:1996) #859 obs

#modelling ogive using calendar age and selecting the best model for each area and period
summary(o.bt.early.full <- glm(maturation ~ age * length.total.x, data = subset(BT8084_2d, age %in% 1:8), family = binomial())) 
summary(o.bt.eaarly.1 <- glm(maturation ~ age + length.total.x, data = subset(BT8084_2d, age %in% 1:8), family = binomial())) 
summary(o.bt.early.l <- glm(maturation ~ length.total.x, data = subset(BT8084_2d, age %in% 1:8), family = binomial())) 
summary(o.bt.early.a <- glm(maturation ~ age, data = subset(BT8084_2d, age %in% 1:8), family = binomial())) 

summary(o.fm.early.full <- glm(maturation ~ age * length.total.x, data = subset(FM8084_2d, age %in% 1:7), family = binomial())) 
summary(o.fm.early.1 <- glm(maturation ~ age + length.total.x, data = subset(FM8084_2d, age %in% 1:7), family = binomial())) 
summary(o.fm.early.l <- glm(maturation ~ length.total.x, data = subset(FM8084_2d, age %in% 1:7), family = binomial())) 
summary(o.fm.early.a <- glm(maturation ~ age, data = subset(FM8084_2d, age %in% 1:7), family = binomial())) 

summary(o.bt.late.full <- glm(maturation ~ age * length.total.x, data = subset(BT9196_2d, age %in% 2:8), family = binomial())) 
summary(o.bt.late.1 <- glm(maturation ~ age + length.total.x, data = subset(BT9196_2d, age %in% 2:8), family = binomial())) 
summary(o.bt.late.a <- glm(maturation ~ age, data = subset(BT9196_2d, age %in% 2:8), family = binomial())) 
summary(o.bt.late.l <- glm(maturation ~ length.total.x, data = subset(BT9196_2d, age %in% 2:8), family = binomial())) 

summary(o.fm.late.full <- glm(maturation ~ age * length.total.x, data = FM9196_2d, family = binomial())) #only has age 2-7
summary(o.fm.late.1 <- glm(maturation ~ age + length.total.x, data = FM9196_2d, family = binomial())) #only has age 2-7
summary(o.fm.late.a <- glm(maturation ~ age, data = FM9196_2d, family = binomial())) #only has age 2-7
summary(o.fm.late.l <- glm(maturation ~ length.total.x, data = FM9196_2d, family = binomial())) #only has age 2-7

#calculating Nagelkerke's R2 for the ogive models
library(fmsb)
NagelkerkeR2()

# Maturity ogives at each age and age -1 with the corresponding body size
#note that the ogive.fit needs to be changed to the corresponding model that fitted for each area and period, e.g. for growth_FM8084_s you choose o.fm.early.1 instead of o.fm.early.full
o_a = predict(ogive.fit, newdata = list(age = growth$age, length.total.x = growth$length), type = "response")
o_am1 = predict(ogive.fit, newdata = list(age = growth$age_m1, length.total.x = growth$length_m1), type = "response")

#getting probability of maturing, m_as 
#note that, "growth" needs to be changed to the corresponding data frame for each area and period
m_as = (o_a - o_am1)/(1 - o_am1)
growth$o_a <- o_a
growth$o_am1 <- o_am1
growth$m_as <- m_as

#creating data frames for each area and period
BT8084_2d_a28 <- subset(BT8084_2d, age %in% 2:8)
growth_BT8084 <- data.table(age = rep(1,1467), length = rep(1,1467), age_m1 = rep(1,1467), length_m1 = rep(1,1467))
i <- 1
for (i in 1:length(BT8084_2d_a28$age)) {
  growth[i,1] <- BT8084_2d_a28[i,5]
  growth[i,2] <- BT8084_2d_a28[i, c(BT8084_2d_a28[i,5]+10)]
  growth[i,3] <- BT8084_2d_a28[i,5]-1
  growth[i,4] <- BT8084_2d_a28[i, c(BT8084_2d_a28[i,5]+9)]
}

FM8084_2d_a27 <- subset(FM8084_2d, age %in% 2:7)
growth_FM8084 <- data.table(age = rep(1,546), length = rep(1,546), age_m1 = rep(1,546), length_m1 = rep(1,546))
i <- 1
for (i in 1:length(FM8084_2d_a27$age)) {
  growth[i,1] <- FM8084_2d_a27[i,30]
  growth[i,2] <- FM8084_2d_a27[i, c(FM8084_2d_a27[i,30]+10)]
  growth[i,3] <- FM8084_2d_a27[i,30]-1
  growth[i,4] <- FM8084_2d_a27[i, c(FM8084_2d_a27[i,30]+9)]
}

growth_BT9196 <- data.table(age = rep(1,709), length = rep(1,709), age_m1 = rep(1,709), length_m1 = rep(1,709)) #BT9196 only has age 2-8
i <- 1
for (i in 1:length(BT9196_2d$age)) {
  growth[i,1] <- BT9196_2d[i,5]
  growth[i,2] <- BT9196_2d[i, c(BT9196_2d[i,5]+10)]
  growth[i,3] <- BT9196_2d[i,5]-1
  growth[i,4] <- BT9196_2d[i, c(BT9196_2d[i,5]+9)]
}

growth_FM9196 <- data.table(age = rep(1,859), length = rep(1,859), age_m1 = rep(1,859), length_m1 = rep(1,859))
i <- 1
for (i in 1:length(FM9196_2d$age)) {
  growth[i,1] <- FM9196_2d[i,30]
  growth[i,2] <- FM9196_2d[i, c(FM9196_2d[i,30]+10)]
  growth[i,3] <- FM9196_2d[i,30]-1
  growth[i,4] <- FM9196_2d[i, c(FM9196_2d[i,30]+9)]
}

#plotting raw probability of maturation for each area and period at each age group 
mas_early_age2 <-ggplot() +
  geom_point(subset(growth_BT8084, age == 2), mapping = aes(y = m_as, x = length), col = alpha("#D9564C", 0.7)) +
  geom_point(subset(growth_FM8084, age == 2), mapping = aes(y = m_as, x = length), col = alpha("#4C53AA", 0.7)) +
  labs(title = "Age 2", tag = "a") +  
  xlab("")+
  xlim(80, 400)+
  ylab("")+
  ylim(-0.001, 1)+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text = element_text(size = 15), plot.margin = unit(c(1,1,0, 1), "cm"))
mas_early_age3 <-
  ggplot() +
  geom_point(subset(growth_BT8084, age == 3), mapping = aes(y = m_as, x = length), col = alpha("#D9564C", 0.7)) +
  geom_point(subset(growth_FM8084, age == 3), mapping = aes(y = m_as, x = length), col = alpha("#4C53AA", 0.7)) +
  labs(title = "Age 3", tag = "b") +  
  xlab("")+
  xlim(80, 400)+
  ylab("")+
  ylim(-0.001, 1)+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text = element_text(size = 15), plot.margin = unit(c(0,1,1, 1), "cm"))
mas_early_age4 <-
  ggplot() +
  geom_point(subset(growth_BT8084, age == 4), mapping = aes(y = m_as, x = length), col = alpha("#D9564C", 0.7)) +
  geom_point(subset(growth_FM8084, age == 4), mapping = aes(y = m_as, x = length), col = alpha("#4C53AA", 0.7)) +
  labs(title = "Age 4", tag = "c") +  
  xlab("")+
  xlim(80, 400)+
  ylab("")+
  ylim(-0.001, 1)+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text = element_text(size = 15), plot.margin = unit(c(1,1,0.3, 1), "cm"))
mas_early_age5 <-
  ggplot() +
  geom_point(subset(growth_BT8084, age == 5), mapping = aes(y = m_as, x = length), col = alpha("#D9564C", 0.7)) +
  geom_point(subset(growth_FM8084, age == 5), mapping = aes(y = m_as, x = length), col = alpha("#4C53AA", 0.7)) +
  labs(title = "Age 5", tag = "d") +  
  xlab("")+
  xlim(80, 400)+
  ylab("")+
  ylim(-0.001, 1)+
  theme_bw(base_size =12)+
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text = element_text(size = 15), plot.margin = unit(c(1,1,-0.3, 1), "cm"))
mas_late_age2 <-
  ggplot() +
  geom_point(subset(growth_BT9196, age == 2), mapping = aes(y = m_as, x = length), col = alpha("#D9564C", 0.7)) +
  geom_point(subset(growth_FM9196, age == 2), mapping = aes(y = m_as, x = length), col = alpha("#4C53AA", 0.7)) +
  labs(title = "Age 2", tag = "e") +  
  xlab("")+
  xlim(80, 400)+
  ylab("")+
  ylim(-0.001, 1)+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text = element_text(size = 15))
mas_late_age3 <-
  ggplot() +
  geom_point(subset(growth_BT9196, age == 3), mapping = aes(y = m_as, x = length), col = alpha("#D9564C", 0.7)) +
  geom_point(subset(growth_FM9196, age == 3), mapping = aes(y = m_as, x = length), col = alpha("#4C53AA", 0.7)) +
  labs(title = "Age 3", tag = "f") +  
  xlab("")+
  xlim(80, 400)+
  ylab("")+
  ylim(-0.001, 1)+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text = element_text(size = 15))
mas_late_age4 <-
  ggplot() +
  geom_point(subset(growth_BT9196, age == 4), mapping = aes(y = m_as, x = length), col = alpha("#D9564C", 0.7)) +
  geom_point(subset(growth_FM9196, age == 4), mapping = aes(y = m_as, x = length), col = alpha("#4C53AA", 0.7)) +
  labs(title = "Age 4", tag = "g") +  
  xlab("")+
  xlim(80, 400)+
  ylab("")+
  ylim(-0.001, 1)+
  theme_bw(base_size =12)+
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text = element_text(size = 15))
mas_late_age5 <-
  ggplot() +
  geom_point(subset(growth_BT9196, age == 5), mapping = aes(y = m_as, x = length), col = alpha("#D9564C", 0.7)) +
  geom_point(subset(growth_FM9196, age == 5), mapping = aes(y = m_as, x = length), col = alpha("#4C53AA", 0.7)) +
  labs(title = "Age 5", tag = "h") +  
  xlab("")+
  xlim(80, 400)+
  ylab("")+
  ylim(-0.001, 1)+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text = element_text(size = 15))

grid.arrange(arrangeGrob(mas_early_age2, mas_late_age2, mas_early_age3,mas_late_age3, mas_early_age4,mas_late_age4, mas_early_age5,mas_late_age5,
                         ncol = 2,
                         bottom = textGrob("Body size (mm)", gp = gpar(fontface = "bold", cex = 1.5)),
                         left = textGrob("Probability of maturing m(a,s)", rot = 90, vjust = 1, gp = gpar(fontface = "bold", cex = 1.5))))


#illustrating how different ogive models give similar FM8084 reaction norm
ggplot(subset(growth_FM8084, age <6) , aes(x=length, y=m_as)) + geom_point(col = alpha("#4C53AA", 0.3)) +
  geom_point(subset(growth_FM8084_s,age<6), mapping = aes(x=length, y=m_as), col = alpha("orange", 0.2)) +
  facet_grid(rows = vars(age)) + 
  theme_bw() + xlab("Body size (mm)") + ylab("Probability of maturing") +
  theme(axis.text = element_text(size = 13))

#plotting ogive prediction
newdata = list(age = c(rep(1,353),rep(2,353), rep(3,353), rep(4,353), rep(5,353)), length.total.x = rep(85:437,5))
o_bt_e = predict(o.bt.early.full, newdata = newdata, type = "response")
o_bt_l = predict(ogive.bt.late.full, newdata = newdata, type = "response")
o_fm_e = predict(o.fm.early.full, newdata = newdata, type = "response")
o_fm_l = predict(o.fm.late.full, newdata = newdata, type = "response")
o_predicted <- data.frame(age = c(rep(1,353),rep(2,353), rep(3,353), rep(4,353), rep(5,353)), length = rep(85:437,5), bte = o_bt_e,
                          btl = o_bt_l, fme = o_fm_e, fml = o_fm_l)

o_e <- ggplot(o_predicted, aes(x=length, y=bte)) + geom_point(col = alpha("#D9564C", 0.7)) +
  geom_point(o_predicted, mapping = aes(x=length, y=fme), col = alpha("#4C53AA", 0.7)) +
  facet_grid(rows = vars(age)) + 
  theme_bw() + xlab("Body size (mm)") + ylab("Maturity ogive") +
  labs(title = "Early")+
  theme(plot.title = element_text(hjust = 0.5, size = 13), axis.text = element_text(size = 13))

o_l <- ggplot(o_predicted, aes(x=length, y=btl)) + geom_point(col = alpha("#D9564C", 0.7)) +
  geom_point(o_predicted, mapping = aes(x=length, y=fml), col = alpha("#4C53AA", 0.7)) +
  facet_grid(rows = vars(age)) + 
  theme_bw() + xlab("Body size (mm)") +
  labs(title = "Late")+
  theme(plot.title = element_text(hjust = 0.5, size = 13), axis.title.y = element_blank(),axis.text = element_text(size = 13))

grid.arrange(o_e,o_l, ncol =2)

#assigning areas and periods
growth_BT8084$area <- "BT"
growth_FM8084$area <- "FM"
growth_BT9196$area <- "BT"
growth_FM9196$area <- "FM"

growth_BT8084$period <- "early"
growth_FM8084$period <- "early"
growth_BT9196$period <- "late"
growth_FM9196$period <- "late"

growth_2areas_2periods <- rbind(growth_BT8084, growth_BT9196, growth_FM8084, growth_FM9196[,c(1:5,8,6,7,9,10)])

#PMRN model selection 
growth_PMRN <- subset(growth_2areas_2periods, m_as > 0 & age < 6) #2955 obs
write.csv(growth_PMRN, "growth_PMRN.csv")
growth_PMRN <- read.csv("growth_PMRN.csv")

#only listing full and null models here, the full selection excel file is attached
full <- glm(m_as ~ age*length*area*period, data = growth_PMRN, family = binomial())
null <- glm(m_as ~ age*length, data = growth_PMRN, family = binomial())
summary(best1pmrn <- glm(m_as ~ age + length + period + area + age:length + period:area + age:period + age:length:area + age:length:period + age:area:period +  length:area:period + age:length:period:area, data = growth_PMRN, family = binomial()))

#Mann-Whitney U test, testing if there is significant differences between m_as of areas and periods
test1 <- wilcox.test(growth_PMRN[growth_PMRN$age == 2 & growth_PMRN$period == "early", 9] ~ growth_PMRN[growth_PMRN$age == 2 & growth_PMRN$period == "early", 10],
                     alternative = "greater")
test2 <- wilcox.test(growth_PMRN[growth_PMRN$age == 3 & growth_PMRN$period == "early", 9] ~ growth_PMRN[growth_PMRN$age == 3 & growth_PMRN$period == "early", 10])
wilcox.test(growth_PMRN[growth_PMRN$age == 4 & growth_PMRN$period == "early", 9] ~ growth_PMRN[growth_PMRN$age == 4 & growth_PMRN$period == "early", 10])
wilcox.test(growth_PMRN[growth_PMRN$age == 5 & growth_PMRN$period == "early", 9] ~ growth_PMRN[growth_PMRN$age == 5 & growth_PMRN$period == "early", 10])
wilcox.test(growth_PMRN[growth_PMRN$age == 2 & growth_PMRN$area == "BT", 9] ~ growth_PMRN[growth_PMRN$age == 2 & growth_PMRN$area == "BT", 11],
            alternative = "less")
wilcox.test(growth_PMRN[growth_PMRN$age == 3 & growth_PMRN$area == "BT", 9] ~ growth_PMRN[growth_PMRN$age == 3 & growth_PMRN$area == "BT", 11],
            alternative = "less")
wilcox.test(growth_PMRN[growth_PMRN$age == 2 & growth_PMRN$period == "late", 9] ~ growth_PMRN[growth_PMRN$age == 2 & growth_PMRN$period == "late", 10],
            alternative = "greater")
qnorm(test$p.value)
test3 <- wilcox.test(growth_PMRN[growth_PMRN$age == 2 & growth_PMRN$period == "late", 9] ~ growth_PMRN[growth_PMRN$age == 2 & growth_PMRN$period == "late", 10],
                     alternative = "greater")
test5 <- wilcox.test(growth_PMRN[growth_PMRN$age == 5 & growth_PMRN$period == "late", 9] ~ growth_PMRN[growth_PMRN$age == 5 & growth_PMRN$period == "late", 10],
                     alternative = "greater")


#generating Lp50 using the best PMRN model
inverse_best1 = function(p, fixed, mod) {
  
  # Set indicator for period
  per = ifelse(fixed[3] == "early", 0, 1)
  are = ifelse(fixed[2] == "BT", 0, 1)
  
  # Calculate length as a function of probability of maturation and fixed auxiliary information
  ret = (logit(p) - 
           coef(mod)["(Intercept)"] - 
           as.numeric(fixed[1])*coef(mod)["age"] - 
           coef(mod)["periodlate"]*per -
           coef(mod)["areaFM"]*are - 
           coef(mod)["periodlate:areaFM"]*are*per -
           as.numeric(fixed[1])*coef(mod)["age:periodlate"]*per -
           as.numeric(fixed[1])*coef(mod)["age:periodlate:areaFM"]*are*per) /
    (coef(mod)["length"] + 
       as.numeric(fixed[1])*coef(mod)["age:length"] + 
       as.numeric(fixed[1])*coef(mod)["age:length:areaFM"]*are +
       as.numeric(fixed[1])*coef(mod)["age:length:periodlate"]*per +
       coef(mod)["length:periodlate:areaFM"]*are*per + 
       as.numeric(fixed[1])*coef(mod)["age:length:periodlate:areaFM"]*are*per)
  
  return(ret)
}
getlength_best1 = function(p, fixed, mod) {
  
  # Calculate length at which maturation probability is p
  len = inverse_best1(p, fixed, mod)
  
  # Get standard error using endpoint transformation
  pred.se = predict(mod, newdata = list(age = as.numeric(fixed[1]), length = len, area = fixed[2], period = fixed[3]), se.fit = T)
  len_up = inverse_best1(expit(pred.se$fit + 1.96*pred.se$se.fit), fixed, mod)
  len_low = inverse_best1(expit(pred.se$fit - 1.96*pred.se$se.fit), fixed, mod)
  
  # Return list
  return(c(L50 = unname(len), CI.low = unname(len_low), CI.up = unname(len_up)))
}
new_dat = data.frame(age = rep(2:5,each=4), area = rep(rep(c("FM","BT"),each=2),4), period = rep(rep(c("early","late"),2),4))
Lp50_newdat_best1 = cbind(new_dat, t(apply(new_dat, 1, getlength_best1, p = 0.5, mod = best1pmrn)))

#dodge overlapping 
pd <- position_dodge(width = 0.2)

myColors <- c("#D9564C", "#4C53AA")
names(myColors) <- levels(growth_PMRN$area)
colScale <- scale_colour_manual(name = "area",values = myColors)

#plotting
ggplot(Lp50_newdat_best1, aes(x = age, y = L50, group = area, col = area)) +
  geom_point(aes(shape = period), size = 3, position = pd) +
  geom_errorbar(aes(x = age, ymin=CI.low, ymax=CI.up ), width = 0.1, position = pd) +
  facet_grid(cols = vars(period)) +
  colScale +
  theme_bw()+
  xlab("Age")+
  ylim(0,958) +
  ylab("Lp50 (mm)") +
  theme(legend.position="none", strip.text.x = element_blank(),
        axis.title = element_text(size = 15), axis.text = element_text(size =13)) +
  labs(title = "")

#separating Lp50_newdat_best1 by area 
Lp50_newdat_best1_BT <- subset(Lp50_newdat_best1, area == "BT")
Lp50_newdat_best1_FM <- subset(Lp50_newdat_best1, area == "FM")

#plotting only the heated area
ggplot(Lp50_newdat_best1_BT, aes(x= age, y=L50, group = period))+
  geom_point(aes(shape = period), size = 2.5, col = "#D9564C", position = pd) +
  geom_errorbar(aes(x = age, ymin=CI.low, ymax=CI.up ), width = 0.1, position = pd, col = "#D9564C") +
  xlab("Age") +
  ylab("Lp50")+
  ylim(0, 958)+
  theme_bw()+
  theme(axis.title = element_text(size=15), axis.text = element_text(size=13))

#model assumption check and validation diagnostic plots for the ogive models and reaction norm models
plot(logit(o_a)~ length, data = subset(growth_BT8084, age < 6), xlab = "Size", ylab = "Ogive")
plot(logit(o_a)~ length, data = subset(growth_FM8084, age < 6), xlab = "Size", ylab = "Ogive")
plot(logit(o_a)~ length, data = subset(growth_BT9196, age < 6), xlab = "Size", ylab = "Ogive")
plot(logit(o_a)~ length, data = subset(growth_FM9196, age < 6), xlab = "Size", ylab = "Ogive")
plot(o.bt.early.full, which = 4, id.n = 3)
plot(ogive.bt.late.full, which = 4, id.n = 3)
plot(o.fm.early.full, which = 4, id.n = 3)
plot(o.fm.late.full, which = 4, id.n = 3)

library(LaplacesDemon)
pred.mix <- predict(best1pmrn)
data_mix <- subset(growth_2areas_2periods, m_as > 0 & age < 6)
plot(pred.mix, logit(data_mix$m_as), xlab = "Prediction", ylab = "Original data")
abline(a = 0, b = 1)
plot(best1pmrn) #save the residual vs. fitted value
plot(gbest)

#investigating sampling time for PMRN
BT8003_2d_a25 <- subset(BT8003_2d, age %in% 2:5 & birth.year %in% c(1980:1984,1991:1996))
BT8003_2d_a25$period <- "early"
BT8003_2d_a25[BT8003_2d_a25$birth.year %in% 1991:1996, "period"] <- "late"
BT_samo <- ggplot() + 
  geom_histogram(BT8003_2d_a25, mapping = aes(month)) + 
  facet_grid(rows = vars(period),switch = "both",
             labeller = labeller(period = period.labs)) +
  scale_x_continuous(breaks = seq(1, 12, 1), lim = c(1,12)) +
  ylim(0,400)+
  labs(x = "Month", y = "Count", title = "Heated") +
  theme_bw() +
  theme(axis.title = element_text(size = 13), axis.text = element_text(size = 13))

FM_month_a25 <- subset(FM8098_2d_f_m789, age %in% 2:5 & birth.year %in% c(1980:1984,1991:1996))
FM_month_a25$period <- "early"
FM_month_a25[FM_month_a25$birth.year %in% 1991:1996, "period"] <- "late"
FM_samo <- ggplot() +
  geom_histogram(FM_month_a25, mapping = aes(month)) +
  facet_grid(rows = vars(period),switch = "both",
             labeller = labeller(period = period.labs)) +
  scale_x_continuous(breaks = seq(1, 12, 1), lim = c(1,12)) +
  ylim(0,400)+
  labs(x = "Month", y = "", title = "Natural") +
  theme_bw() +
  theme(axis.title = element_text(size = 13), axis.text = element_text(size = 13))

grid.arrange(BT_samo, FM_samo, ncol = 2)

#GSI
#gonad data from both areas and periods
g8001 <- read.csv("g8001.csv") #gonad_selectyear from here with length
#density distribution of GSI for week 10-30
g8001_1030 <- subset(g8001, v %in% 10:30) #March to August
plot(density(g8001_1030$GSI, bw = 1.3), xlab = "GSI", ylab = "Density", cex = 2, main = "")

gonad_selectyear <- read.csv("gonadselectyear.csv") #contains only GSI>10 and v10-30
gonad_fin <- subset(gonad_selectyear, age > 2 & age <6 )
#model selection, the full list is attached as excel file
summary(gbest <- glm(GSI ~ age + length + area + period + age:length + age:area + age:period + length:period + 
                       age:length:area + age:length:period + age:area:period + length:area:period , data = gonad_fin))

#GSI prediction
new_dat_GSI_BT_early_3 = data.frame(age = rep(3, 221), length = seq(110,330,1), area = rep("BT",221), period = rep("early",221))
new_dat_GSI_BT_early_4 = data.frame(age = rep(4, 221), length = seq(110,330,1), area = rep("BT",221), period = rep("early",221))
new_dat_GSI_BT_early_5 = data.frame(age = rep(5, 221), length = seq(110,330,1), area = rep("BT",221), period = rep("early",221))

new_dat_GSI_BT_late_3 = data.frame(age = rep(3, 221), length = seq(110, 330, 1), area = rep("BT",221), period = rep("late",221))
new_dat_GSI_BT_late_4 = data.frame(age = rep(4, 221), length = seq(110, 330, 1), area = rep("BT",221), period = rep("late",221))
new_dat_GSI_BT_late_5 = data.frame(age = rep(5, 221), length = seq(110, 330, 1), area = rep("BT",221), period = rep("late",221))

new_dat_GSI_FM_early_3 = data.frame(age = rep(3, 221), length = seq(110,330,1), area = rep("FM",221), period = rep("early",221))
new_dat_GSI_FM_early_4 = data.frame(age = rep(4, 221), length = seq(110,330,1), area = rep("FM",221), period = rep("early",221))
new_dat_GSI_FM_early_5 = data.frame(age = rep(5, 221), length = seq(110,330,1), area = rep("FM",221), period = rep("early",221))

new_dat_GSI_FM_late_3 = data.frame(age = rep(3, 221), length = seq(110,330,1), area = rep("FM",221), period = rep("late",221))
new_dat_GSI_FM_late_4 = data.frame(age = rep(4, 221), length = seq(110,330,1), area = rep("FM",221), period = rep("late",221))
new_dat_GSI_FM_late_5 = data.frame(age = rep(5, 221), length = seq(110,330,1), area = rep("FM",221), period = rep("late",221))

#make predictions of GSI using the best model for each area for each period 
BT_early_3 = predict(gbest, newdata = new_dat_GSI_BT_early_3, se.fit = T)
BT_early_4 = predict(gbest, newdata = new_dat_GSI_BT_early_4, se.fit = T)
BT_early_5 = predict(gbest, newdata = new_dat_GSI_BT_early_5, se.fit = T)

FM_early_3 = predict(gbest, newdata = new_dat_GSI_FM_early_3, se.fit = T)
FM_early_4 = predict(gbest, newdata = new_dat_GSI_FM_early_4, se.fit = T)
FM_early_5 = predict(gbest, newdata = new_dat_GSI_FM_early_5, se.fit = T)

BT_late_3 = predict(gbest, newdata = new_dat_GSI_BT_late_3, se.fit = T)
BT_late_4 = predict(gbest, newdata = new_dat_GSI_BT_late_4, se.fit = T)
BT_late_5 = predict(gbest, newdata = new_dat_GSI_BT_late_5, se.fit = T)

FM_late_3 = predict(gbest, newdata = new_dat_GSI_FM_late_3, se.fit = T)
FM_late_4 = predict(gbest, newdata = new_dat_GSI_FM_late_4, se.fit = T)
FM_late_5 = predict(gbest, newdata = new_dat_GSI_FM_late_5, se.fit = T)

#combining the predictions of each age from both areas 
df_late_3 <- data.frame(BT_p = BT_late_3$fit, FM_p = FM_late_3$fit, length = length) 
df_late_4 <- data.frame(BT_p = BT_late_4$fit, FM_p = FM_late_4$fit, length = length) 
df_late_5 <- data.frame(BT_p = BT_late_5$fit, FM_p = FM_late_5$fit, length = length) 

df_early_3 <- data.frame(BT_p = BT_early_3$fit, FM_p = FM_early_3$fit, length = length) 
df_early_4 <- data.frame(BT_p = BT_early_4$fit, FM_p = FM_early_4$fit, length = length) 
df_early_5 <- data.frame(BT_p = BT_early_5$fit, FM_p = FM_early_5$fit, length = length)

#adding empirical data points of GSI
BT_early_3_real <- subset(gonad_selectyear, area == "BT"& period == "early"&age == 3) #47 obs
BT_early_4_real <- subset(gonad_selectyear, area == "BT"& period == "early"&age == 4) #52 obs
BT_early_5_real <- subset(gonad_selectyear, area == "BT"& period == "early"&age == 5) #25 obs

FM_early_3_real <- subset(gonad_selectyear, area == "FM"& period == "early"&age == 3) #1 obs
FM_early_4_real <- subset(gonad_selectyear, area == "FM"& period == "early"&age == 4) #18 obs
FM_early_5_real <- subset(gonad_selectyear, area == "FM"& period == "early"&age == 5) #13 obs

BT_late_3_real <- subset(gonad_selectyear, area == "BT"& period == "late"&age == 3) #39 obs
BT_late_4_real <- subset(gonad_selectyear, area == "BT"& period == "late"&age == 4) #48 obs
BT_late_5_real <- subset(gonad_selectyear, area == "BT"& period == "late"&age == 5) #46 obs

FM_late_3_real <- subset(gonad_selectyear, area == "FM"& period == "late"&age == 3) #27 obs
FM_late_4_real <- subset(gonad_selectyear, area == "FM"& period == "late"&age == 4) #66 obs
FM_late_5_real <- subset(gonad_selectyear, area == "FM"& period == "late"&age == 5) #68 obs

#plotting
#period early
#age 3
#colour: red "#D9564C", blue "#4C53AA"
plot_e_3 <- ggplot() + 
  geom_line(mapping = aes(y = df_early_3$BT_p, x = length), col = "#D9564C", linetype = 2) +
  geom_line(subset(df_early_3, length %in% c(122:205)), mapping = aes(y = BT_p, x = length), col = "#D9564C", linetype = 1, size = 1.5) +
  geom_line(mapping = aes(y = df_early_3$FM_p, x = length), col =  "#4C53AA", linetype = 2) +
  geom_ribbon(aes(x = length, ymin=BT_early_3$fit - 1.96* BT_early_3$se.fit, ymax=BT_early_3$fit + 1.96* BT_early_3$se.fit), alpha=0.1, fill = "#D9564C", 
              color = "white", linetype = "dotted") +
  geom_ribbon(aes(x = length, ymin=FM_early_3$fit - 1.96* FM_early_3$se.fit, ymax=FM_early_3$fit + 1.96* FM_early_3$se.fit), alpha=0.1, fill = "#4C53AA", 
              color = "white", linetype = "dotted") +
  geom_point(BT_early_3_real, mapping = aes(y = GSI, x = length), col = "#D9564C") +
  geom_point(FM_early_3_real, mapping = aes(y = GSI, x = length), col =  "#4C53AA") +
  xlim(110,330) +
  labs(title = "Age 3")+
  ylab("early")+
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5))
#age 4
plot_e_4 <-  ggplot() + geom_line(mapping = aes(y = df_early_4$BT_p, x = length), col = "#D9564C", linetype = 2) +
  geom_line(subset(df_early_4, length %in% c(150:275)), mapping = aes(y = BT_p, x = length), col = "#D9564C", linetype = 1, size = 1.5) +
  geom_line(mapping = aes(y = df_early_4$FM_p, x = length), col =  "#4C53AA", linetype = 2) +
  geom_line(subset(df_early_4, length %in% c(162:219)), mapping = aes(y = FM_p, x = length), col = "#4C53AA", linetype = 1, size = 1.5) +
  geom_ribbon(aes(x = length, ymin=BT_early_4$fit - 1.96* BT_early_4$se.fit, ymax=BT_early_4$fit + 1.96* BT_early_4$se.fit), alpha=0.1, fill ="#D9564C", 
              color = "white", linetype = "dotted") +
  geom_ribbon(aes(x = length, ymin=FM_early_4$fit - 1.96* FM_early_4$se.fit, ymax=FM_early_4$fit + 1.96* FM_early_4$se.fit), alpha=0.1, fill =  "#4C53AA", 
              color = "white", linetype = "dotted") +
  geom_point(BT_early_4_real, mapping = aes(y = GSI, x = length), col = "#D9564C") +
  geom_point(FM_early_4_real, mapping = aes(y = GSI, x = length), col =  "#4C53AA") +
  xlim(140,324) +
  ylim(-1.6, 35.7) +
  labs(title = "Age 4", tag = "a")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 13), 
        axis.text = element_text(size=13), axis.title= element_blank())

#age 5 
plot_e_5 <-  ggplot() + geom_line(mapping = aes(y = df_early_5$BT_p, x = length), col = "#D9564C", linetype = 2) +
  geom_line(subset(df_early_5, length %in% c(185:282)), mapping = aes(y = BT_p, x = length), col ="#D9564C", linetype = 1, size = 1.5) +
  geom_line(mapping = aes(y = df_early_5$FM_p, x = length), col =  "#4C53AA", linetype = 2) +
  geom_line(subset(df_early_5, length %in% c(189:221)), mapping = aes(y = FM_p, x = length), col = "#4C53AA", linetype = 1, size = 1.5)+
  geom_ribbon(aes(x = length, ymin=BT_early_5$fit - 1.96* BT_early_5$se.fit, ymax=BT_early_5$fit + 1.96* BT_early_5$se.fit), alpha=0.1, fill ="#D9564C", 
              color = "white", linetype = "dotted") +
  geom_ribbon(aes(x = length, ymin=FM_early_5$fit - 1.96* FM_early_5$se.fit, ymax=FM_early_5$fit + 1.96* FM_early_5$se.fit), alpha=0.1, fill =  "#4C53AA", 
              color = "white", linetype = "dotted") +
  geom_point(BT_early_5_real, mapping = aes(y = GSI, x = length), col = "#D9564C") +
  geom_point(FM_early_5_real, mapping = aes(y = GSI, x = length), col =  "#4C53AA") +
  xlim(140,324) +
  ylim(-1.6, 35.7) +
  labs(title = "Age 5", tag = "b")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 13), 
        axis.text = element_text(size=13), axis.title = element_blank())

#period late
#age 3
plot_l_3 <-   ggplot() + 
  geom_line(mapping = aes(y = df_late_3$BT_p, x = length), col = "red", linetype = 2) +
  geom_line(subset(df_late_3, length %in% c(178:313)), mapping = aes(y = BT_p, x = length), col = "red", linetype = 1, size = 1.5) +
  geom_line(mapping = aes(y = df_late_3$FM_p, x = length), col = "blue", linetype = 2) +
  geom_line(subset(df_late_3, length %in% c(150:185)), mapping = aes(y = FM_p, x = length), col = "blue", linetype = 1, size = 1.5) +
  geom_ribbon(aes(x = length, ymin=BT_late_3$fit - 1.96* BT_late_3$se.fit, ymax=BT_late_3$fit + 1.96* BT_late_3$se.fit), alpha=0.1, fill = "red", 
              color = "white", linetype = "dotted") +
  geom_ribbon(aes(x = length, ymin=FM_late_3$fit - 1.96* FM_late_3$se.fit, ymax=FM_late_3$fit + 1.96* FM_late_3$se.fit), alpha=0.1, fill = "blue", 
              color = "white", linetype = "dotted") +
  geom_point(BT_late_3_real, mapping = aes(y = GSI, x = length), col = "red") +
  geom_point(FM_late_3_real, mapping = aes(y = GSI, x = length), col = "blue") +
  xlim(110,330)  +
  labs(title = "age 3")+
  ylab("late")+
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5))

#age 4
plot_l_4 <-   ggplot() + geom_line(mapping = aes(y = df_late_4$BT_p, x = length), col ="#D9564C", linetype = 2) +
  geom_line(subset(df_late_4, length %in% c(169:315)), mapping = aes(y = BT_p, x = length), col = "#D9564C", linetype = 1, size = 1.5) +
  geom_line(mapping = aes(y = df_late_4$FM_p, x = length), col =  "#4C53AA", linetype = 2) +
  geom_line(subset(df_late_4, length %in% c(154:245)), mapping = aes(y = FM_p, x = length), col =  "#4C53AA", linetype = 1, size = 1.5) +
  geom_ribbon(aes(x = length, ymin=BT_late_4$fit - 1.96* BT_late_4$se.fit, ymax=BT_late_4$fit + 1.96* BT_late_4$se.fit), alpha=0.1, fill = "#D9564C", 
              color = "white", linetype = "dotted") +
  geom_ribbon(aes(x = length, ymin=FM_late_4$fit - 1.96* FM_late_4$se.fit, ymax=FM_late_4$fit + 1.96* FM_late_4$se.fit), alpha=0.1, fill =  "#4C53AA", 
              color = "white", linetype = "dotted") +
  geom_point(BT_late_4_real, mapping = aes(y = GSI, x = length), col = "#D9564C") +
  geom_point(FM_late_4_real, mapping = aes(y = GSI, x = length), col =  "#4C53AA") +
  xlim(140,324) +
  ylim(-1.6, 35.7) +
  labs(title = "Age 4", tag = "c")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 13), 
        axis.text = element_text(size=13), axis.title = element_blank())

#age 5 
plot_l_5 <-   
  ggplot() + geom_line(mapping = aes(y = df_late_5$BT_p, x = length), col = "#D9564C", linetype = 2) +
  geom_line(subset(df_late_5, length %in% c(174:323)), mapping = aes(y = BT_p, x = length), col = "#D9564C", linetype = 1, size = 1.5) +
  geom_line(mapping = aes(y = df_late_5$FM_p, x = length), col = "#4C53AA", linetype = 2) +
  geom_line(subset(df_late_5, length %in% c(168:263)), mapping = aes(y = FM_p, x = length), col =  "#4C53AA", linetype = 1, size = 1.5)+
  geom_ribbon(aes(x = length, ymin=BT_late_5$fit - 1.96* BT_late_5$se.fit, ymax=BT_late_5$fit + 1.96* BT_late_5$se.fit), alpha=0.1, fill = "#D9564C", 
              color = "white", linetype = "dotted") +
  geom_ribbon(aes(x = length, ymin=FM_late_5$fit - 1.96* FM_late_5$se.fit, ymax=FM_late_5$fit + 1.96* FM_late_5$se.fit), alpha=0.1, fill = "#4C53AA", 
              color = "white", linetype = "dotted") +
  geom_point(BT_late_5_real, mapping = aes(y = GSI, x = length), col = "#D9564C") +
  geom_point(FM_late_5_real, mapping = aes(y = GSI, x = length), col =  "#4C53AA") +
  xlim(140,324) +
  ylim(-1.6, 35.7) +
  labs(title = "Age 5", tag = "d")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 13), 
        axis.text = element_text(size=13), axis.title = element_blank())

grid.arrange(plot_e_4, plot_l_4, plot_e_5, plot_l_5, ncol = 2,  bottom = textGrob("Body size (mm)", gp = gpar(fontface = "bold", cex = 1.5)),
             left = textGrob("Gonado-somatic index (%)", rot = 90, vjust = 1, gp = gpar(fontface = "bold", cex = 1.5)))


#Student T test
library(rstatix)
G4early <- gonad_selectyear[gonad_selectyear$age == 4 & gonad_selectyear$area == "BT", ]
testG <- t.test(gonad_selectyear[gonad_selectyear$age == 4 & gonad_selectyear$area == "BT", 7] ~ gonad_selectyear[gonad_selectyear$age == 4 & gonad_selectyear$area == "BT", 9],
                alternative = "greater")
G4early %>% cohens_d(GSI ~ period, var.equal = TRUE)
d = (20.16- 16.91)/sqrt(sd)
sd = (3.45^2 + 1.56^2 )/(51+17)