library(SmartEDA)
library(dplyr)
library(MASS)
library(readxl)
library(HistData)
library(car)
library(lmtest)
library(wooldridge)
library(ggplot2)
library(gridExtra)

## 0. Data Load 

ds <- read.csv('C:/Users/dain/Downloads/R/oes2002_2006.csv')
summary(ds)


## 1. EDA

ExpData(ds, type = 1)
ExpData(ds, type=2)
ExpNumStat(ds)
ExpNumStat(ds, by='GA', gp='size')

plot <- ds |> ExpNumViz(Page=c(3,3), target = 'size')
plot

plot <- ds |> ExpNumViz(Page=c(3,3), target = 'male')
plot

plot <- ds |> ExpNumViz(Page=c(3,3), target = 'mar')
plot

plot <- ds |> ExpNumViz(Page=c(3,3), target = 'metro')
plot

plot <- ds |> ExpNumViz(Page=c(3,3), target = 'non_regular')
plot

ExpCTable(ds)

plot2 <- ExpCatViz(ds, Page=c(2,2))
plot2


## 2. Estimation of Systematic Differnce between Size=8 and 9

ds$Q <- as.numeric(ds$size==9)

r1 = lm(male~Q, data=ds)
r2 = lm(edu~Q, data=ds)
r3 = lm(mar~Q, data=ds)
r4 = lm(tenure~Q, data=ds)
r5 = lm(metro~Q, data=ds)
r6 = lm(non_regular~Q, data=ds)
r7 = lm(weektime~Q, data=ds)
r8 = lm(hourlywage~Q, data=ds)
r9 = lm(hourly_wage_r~Q, data=ds)

summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)
summary(r7)
summary(r8)
summary(r9)


## Differnce in Differnce
## 3. Introduction to DD

ds$y1 = ds$weektime
ds$y2 = ds$hourly_wage_r
ds$y3 = ds$y1*ds$y2

ds$t02 = as.numeric(ds$year==2002)
ds$t03 = as.numeric(ds$year==2003)
ds$t04 = as.numeric(ds$year==2004)
ds$t05 = as.numeric(ds$year==2005)
ds$t06 = as.numeric(ds$year==2006)


minfo_y1 = aggregate(ds$y1, list(ds$year,ds$Q == 1), mean)
names(minfo_y1) = c("YR","Treatment","y1")

minfo_y1$Group[1:5] = "y1, Size==8"
minfo_y1$Group[6:10] = "y1, Size==9"
minfo_y1

qplot(YR, y1, data=minfo_y1, geom=c("point","line"), colour=Group,
      xlab="Year", ylab="Y1",
      ylim=c(45, 55)) + geom_vline(xintercept = 2004)


minfo_y2 = aggregate(ds$y2, list(ds$year,ds$Q == 1), mean)
names(minfo_y2) = c("YR","Treatment","y2")

minfo_y2$Group[1:5] = "y2, Size==8"
minfo_y2$Group[6:10] = "y2, Size==9"
minfo_y2

qplot(YR, y2, data=minfo_y2, geom=c("point","line"), colour=Group,
      xlab="Year", ylab="Y2",
      ylim=c(0, 2)) + geom_vline(xintercept = 2004)


minfo_y3 = aggregate(ds$y3, list(ds$year,ds$Q == 1), mean)
names(minfo_y3) = c("YR","Treatment","y3")

minfo_y3$Group[1:5] = "y3, Size==8"
minfo_y3$Group[6:10] = "y3, Size==9"
minfo_y3

qplot(YR, y3, data=minfo_y3, geom=c("point","line"), colour=Group,
      xlab="Year", ylab="Y3",
      ylim=c(45, 90)) + geom_vline(xintercept = 2004)


## 4. Control for covariates

ds$age2 = ds$age^2
ds$Q4= as.numeric((ds$t03+ds$t04)==1)

reg1 = lm(y1 ~Q*t04+male+age+age2+edu2+edu3+edu4+mar+tenure+metro+oc2+oc3+oc4+ind2+ind3+ind4+ind5+ind6+ind7+ind8+non_regular, data = ds, Q4==1)
summary(reg1)
reg2 = lm(y2 ~Q*t04+male+age+age2+edu2+edu3+edu4+mar+tenure+metro+oc2+oc3+oc4+ind2+ind3+ind4+ind5+ind6+ind7+ind8+non_regular, data = ds, Q4==1)
summary(reg2)
reg3 = lm(y3 ~Q*t04+male+age+age2+edu2+edu3+edu4+mar+tenure+metro+oc2+oc3+oc4+ind2+ind3+ind4+ind5+ind6+ind7+ind8+non_regular, data = ds, Q4==1)
summary(reg3)


## 5. Additional Control for covariates

ds$tenure2 = ds$tenure^2
ds$fem = as.numeric(ds$male==0)
ds$marfem = ds$mar * ds$fem

reg4 = lm(y1 ~Q*t04+male+age+age2+edu2+edu3+edu4+mar+tenure+tenure2+metro+oc2+oc3+oc4+ind2+ind3+ind4+ind5+ind6+ind7+ind8+non_regular, data = ds, Q4==1)
summary(reg4)
reg5 = lm(y2 ~Q*t04+male+age+age2+edu2+edu3+edu4+mar+tenure+tenure2+metro+oc2+oc3+oc4+ind2+ind3+ind4+ind5+ind6+ind7+ind8+non_regular, data = ds, Q4==1)
summary(reg5)
reg6 = lm(y3 ~Q*t04+male+age+age2+edu2+edu3+edu4+mar+tenure+tenure2+metro+oc2+oc3+oc4+ind2+ind3+ind4+ind5+ind6+ind7+ind8+non_regular, data = ds, Q4==1)
summary(reg6)

reg7 = lm(y1 ~Q*t04+age+age2+edu2+edu3+edu4+mar*fem+tenure+metro+oc2+oc3+oc4+ind2+ind3+ind4+ind5+ind6+ind7+ind8+non_regular, data = ds, Q4==1)
summary(reg7)


## 6. DD in Size 8

ds$Qr= as.numeric(ds$size==8)
ds$Q6= as.numeric((ds$t04+ds$t05)==1)

reg8 = lm(y1 ~Qr*t05+male+age+age2+edu2+edu3+edu4+mar+tenure+tenure2+metro+oc2+oc3+oc4+ind2+ind3+ind4+ind5+ind6+ind7+ind8+non_regular, data = ds, Q6==1)
summary(reg8)
reg9 = lm(y2 ~Qr*t05+male+age+age2+edu2+edu3+edu4+mar+tenure+tenure2+metro+oc2+oc3+oc4+ind2+ind3+ind4+ind5+ind6+ind7+ind8+non_regular, data = ds, Q6==1)
summary(reg9)
reg10 = lm(y3 ~Qr*t05+male+age+age2+edu2+edu3+edu4+mar+tenure+tenure2+metro+oc2+oc3+oc4+ind2+ind3+ind4+ind5+ind6+ind7+ind8+non_regular, data = ds, Q6==1)
summary(reg10)


## 7. Dynamics of the treatment effect over time

reg11 = lm(y1 ~Q*t03+Q*t04+Q*t05+Q*t06+male+age+age2+edu2+edu3+edu4+mar+tenure+tenure2+metro+oc2+oc3+oc4+ind2+ind3+ind4+ind5+ind6+ind7+ind8+non_regular, data = ds)
summary(reg11)
reg12 = lm(y2 ~Q*t03+Q*t04+Q*t05+Q*t06+male+age+age2+edu2+edu3+edu4+mar+tenure+tenure2+metro+oc2+oc3+oc4+ind2+ind3+ind4+ind5+ind6+ind7+ind8+non_regular, data = ds)
summary(reg12)
reg13 = lm(y3 ~Q*t03+Q*t04+Q*t05+Q*t06+male+age+age2+edu2+edu3+edu4+mar+tenure+tenure2+metro+oc2+oc3+oc4+ind2+ind3+ind4+ind5+ind6+ind7+ind8+non_regular, data = ds)
summary(reg13)

p_values_y1 <- summary(reg11)$coefficients[, "Pr(>|t|)"]
p1 <- (tail(p_values_y1,4))

p_values_y2 <- summary(reg12)$coefficients[, "Pr(>|t|)"]
p2 <- (tail(p_values_y2,4))

p_values_y3 <- summary(reg13)$coefficients[, "Pr(>|t|)"]
p3 <- (tail(p_values_y3,4))

p_values <- list()
p_values <- rbind(p1, p2, p3)
p_values