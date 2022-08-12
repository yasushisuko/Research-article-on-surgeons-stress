# Author of this program: Pasi Kytölä
setwd("")

#########################
# 1 SCL
#########################

library("nlme")
SukoSCL<-read.csv("1SCL.dat", header = TRUE, sep = ";", skip = 28)
SukoSCL<-na.omit(SukoSCL)
head(SukoSCL[,c(1:5)])
SukoSCL <- SukoSCL[-c(2),] #Remove Oct 9
SukoSCL$NS <- as.factor(SukoSCL$NS)
SukoSCL$SC <- as.factor(SukoSCL$SC)
SukoSCL<-reshape(SukoSCL, direction = "long", varying = list(12:131), idvar = "Date")
names(SukoSCL)[13]<-"SCL"
fit <- lme(SCL ~ NS+SC+time+NS:time+SC:time+NS:SC:time, data = SukoSCL, random = ~ 1 | Surgeon/Id)
summary(fit)
intervals(fit)

#The following script is to draw Figure 3 (prediction of the surgeons' SCL)

##### Script for Figure 3 starts here #####

#Expert surgeons with natural sound
(SC1NS1 <- data.frame(time = 1:120, 
                      SC = rep(1, 120),
                      NS = rep(1, 120)))
(SC1NS1 <- predict(fit, SC1NS1, level = 0, asList = TRUE))

#Expert surgeons without natural sound
(SC1NS0 <- data.frame(time = 1:120, 
                      SC = rep(1, 120),
                      NS = rep(0, 120)))
(SC1NS0 <- predict(fit, SC1NS0, level = 0, asList = TRUE))

#Resident surgeons with natural sound
(SC0NS1 <- data.frame(time = 1:120, 
                      SC = rep(0, 120),
                      NS = rep(1, 120)))
(SC0NS1 <- predict(fit, SC0NS1, level = 0, asList = TRUE))

#Resident surgeons without natural sounds
(SC0NS0 <- data.frame(time = 1:120, 
                      SC = rep(0, 120),
                      NS = rep(0, 120)))
(SC0NS0 <- predict(fit, SC0NS0, level = 0, asList = TRUE))

#Data frame
prediction<-as.data.frame(cbind(1:120,SC1NS1[1:120],SC1NS0[1:120],SC0NS1[1:120],SC0NS0[1:120]))
names(prediction)[c(1,2,3,4,5)]<-c("time","SC1NS1","SC1NS0","SC0NS1","SC0NS0")

#Plot
setEPS()
postscript("Figure3_prediction_of_SCL.eps", height = 3.5, width = 6, pointsize = 9)
par_vanha <- par(mar=c(5,5,3,3), xaxs = "r", yaxs = "r")
plot(prediction$time, prediction$SC1NS1, 
     type = "l",
     col = 1,
     xaxt = "n",
     xlab = "Time (min)",
     ylab = "SCL (microsiemens)", 
     ylim = c(3, 8))
axis(side = 1, at=c(0, 24, 48, 72, 96, 120), labels=c("0","2","4","6","8","10"))
lines(prediction$time, prediction$SC1NS0, type = "l", lty=4, col = 1)
lines(prediction$time, prediction$SC0NS1, type = "l", lty=5, col = 1)
lines(prediction$time, prediction$SC0NS0, type = "l", lty=3, col = 1)
legend(x = 90, y = 8.1, legend=c("Expert with NS", "Expert without NS", "Resident with NS", "Resident without NS"), lty=c(1,4,5,3), cex=0.8)
par(par_vanha)
dev.off()

##### Script for Figure 3 ends here #####

#########################
# 2 ROS-J
#########################

library("nlme")
SukoROS.J<-read.csv("2ROSJ.dat", header = TRUE, sep = ";", skip = 33)
SukoROS.J$NS <- as.factor(SukoROS.J$NS)
SukoROS.J$SC <- as.factor(SukoROS.J$SC)
SukoROS.J$Stage <- as.factor(SukoROS.J$Stage)

contrasts(SukoROS.J$Stage) <- contr.treatment(3, base = 2)
contrasts(SukoROS.J$Stage)

#Model 1
f1 <- lme(fixed = ROS.J.Total ~ Stage*NS,
          data = SukoROS.J,
          random = list(Surgeon = pdIdent(~Stage-1)))
summary(f1)
intervals(f1)
ranef(f1)
VarCorr(f1)

#Model 2
f2 <- lme(fixed = ROS.J.Total ~ Stage*NS*SC,
          data = SukoROS.J,
          weights = varIdent(form = ~ 1 | Surgeon),
          random = list(Surgeon = pdIdent(~Stage-1)))
summary(f2)
intervals(f2)
ranef(f2)
VarCorr(f2)

#########################
# 3 POMS2-A Short
#########################

library("nlme")
SukoPOMS<-read.csv("3POMS2AShort.dat", header = TRUE, sep = ";", skip = 33)
SukoPOMS$NS <- as.factor(SukoPOMS$NS)
SukoPOMS$SC <- as.factor(SukoPOMS$SC)
SukoPOMS$Stage <- as.factor(SukoPOMS$Stage)

# Model 1
f1_POMS2_TMD <- lme(fixed = POMS2_TMD ~ Stage*NS, data = SukoPOMS, correlation = corAR1(), random = ~ 1 | Surgeon)
summary(f1_POMS2_TMD)
intervals(f1_POMS2_TMD)
ranef(f1_POMS2_TMD)
VarCorr(f1_POMS2_TMD)

# Model 2
f2_POMS2_TMD <- lme(fixed = POMS2_TMD ~ Stage*NS*SC, data = SukoPOMS, correlation = corAR1(), random = ~ 1 | Surgeon)
summary(f2_POMS2_TMD)
intervals(f2_POMS2_TMD)
ranef(f2_POMS2_TMD)
VarCorr(f2_POMS2_TMD)