# Author of this program: Pasi Kytölä
setwd("")

#########################
# 2 ROS-J sub-scales
#########################

library("nlme")
SukoROS.J<-read.csv("2ROSJ.dat", header = TRUE, sep = ";", skip = 33)
SukoROS.J$NS <- as.factor(SukoROS.J$NS)
SukoROS.J$SC <- as.factor(SukoROS.J$SC)
SukoROS.J$Stage <- as.factor(SukoROS.J$Stage)

contrasts(SukoROS.J$Stage) <- contr.treatment(3, base = 2)
contrasts(SukoROS.J$Stage)

# Model 1
f1_ROSJ_Q1 <- lme(fixed = ROS.J.Q1 ~ Stage*NS, data = SukoROS.J, random = list(Surgeon = pdIdent(~Stage-1)))
f1_ROSJ_Q2 <- lme(fixed = ROS.J.Q2 ~ Stage*NS, data = SukoROS.J, random = list(Surgeon = pdIdent(~Stage-1)))
f1_ROSJ_Q3 <- lme(fixed = ROS.J.Q3 ~ Stage*NS, data = SukoROS.J, random = list(Surgeon = pdIdent(~Stage-1)))
f1_ROSJ_Q4 <- lme(fixed = ROS.J.Q4 ~ Stage*NS, data = SukoROS.J, random = list(Surgeon = pdIdent(~Stage-1)))
f1_ROSJ_Q5 <- lme(fixed = ROS.J.Q5 ~ Stage*NS, data = SukoROS.J, random = list(Surgeon = pdIdent(~Stage-1)))
f1_ROSJ_Q6 <- lme(fixed = ROS.J.Q5 ~ Stage*NS, data = SukoROS.J, random = list(Surgeon = pdIdent(~Stage-1)))


summary(f1_ROSJ_Q1)
intervals(f1_ROSJ_Q1)

summary(f1_ROSJ_Q2)
intervals(f1_ROSJ_Q2)

summary(f1_ROSJ_Q3)
intervals(f1_ROSJ_Q3)

summary(f1_ROSJ_Q4)
intervals(f1_ROSJ_Q4)

summary(f1_ROSJ_Q5)
intervals(f1_ROSJ_Q5)

summary(f1_ROSJ_Q6)
intervals(f1_ROSJ_Q6)

# Model 2
# Note: The code for Q2, Q4, and Q6 differs from the code for Q1, Q3, and Q5.
f2_ROSJ_Q1 <- lme(fixed = ROS.J.Q1 ~ Stage*NS*SC,
                  data = SukoROS.J,
                  weights = varIdent(form = ~ 1 | Surgeon),
                  random = list(Surgeon = pdIdent(~Stage-1)))
f2_ROSJ_Q3 <- lme(fixed = ROS.J.Q3 ~ Stage*NS*SC,
                  data = SukoROS.J,
                  weights = varIdent(form = ~ 1 | Surgeon),
                  random = list(Surgeon = pdIdent(~Stage-1)))
f2_ROSJ_Q5 <- lme(fixed = ROS.J.Q5 ~ Stage*NS*SC,
                  data = SukoROS.J,
                  weights = varIdent(form = ~ 1 | Surgeon),
                  random = list(Surgeon = pdIdent(~Stage-1)))

f2_ROSJ_Q2 <- lme(fixed = ROS.J.Q2 ~ Stage*NS*SC,
                  data = SukoROS.J,
                  correlation = corAR1(),
                  random = list(Surgeon = pdIdent(~Stage-1)))
f2_ROSJ_Q4 <- lme(fixed = ROS.J.Q4 ~ Stage*NS*SC,
                  data = SukoROS.J,
                  correlation = corAR1(),
                  random = list(Surgeon = pdIdent(~Stage-1)))
f2_ROSJ_Q6 <- lme(fixed = ROS.J.Q6 ~ Stage*NS*SC,
                  data = SukoROS.J,
                  correlation = corAR1(),
                  random = list(Surgeon = pdIdent(~Stage-1)))


summary(f2_ROSJ_Q1)
intervals(f2_ROSJ_Q1)

summary(f2_ROSJ_Q2)
intervals(f2_ROSJ_Q2)

summary(f2_ROSJ_Q3)
intervals(f2_ROSJ_Q3)

summary(f2_ROSJ_Q4)
intervals(f2_ROSJ_Q4)

summary(f2_ROSJ_Q5)
intervals(f2_ROSJ_Q5)

summary(f2_ROSJ_Q6)
intervals(f2_ROSJ_Q6)

#########################
# 3 POMS2-A Short sub-scales
#########################
library("nlme")
SukoPOMS<-read.csv("3POMS2AShort.dat", header = TRUE, sep = ";", skip = 33)
SukoPOMS$NS <- as.factor(SukoPOMS$NS)
SukoPOMS$SC <- as.factor(SukoPOMS$SC)
SukoPOMS$Stage <- as.factor(SukoPOMS$Stage)

# Model 1
f1_POMS2_AH <- lme(fixed = POMS2_AH ~ Stage*NS, data = SukoPOMS, weights = varIdent(form = ~ 1 | Surgeon), random = ~ 1 | Surgeon)
f1_POMS2_CB <- lme(fixed = POMS2_CB ~ Stage*NS, data = SukoPOMS, weights = varIdent(form = ~ 1 | Surgeon), random = ~ 1 | Surgeon)
f1_POMS2_DD <- lme(fixed = POMS2_DD ~ Stage*NS, data = SukoPOMS, weights = varIdent(form = ~ 1 | Surgeon), random = ~ 1 | Surgeon)
f1_POMS2_FI <- lme(fixed = POMS2_FI ~ Stage*NS, data = SukoPOMS, correlation = corAR1(), random = list(Surgeon = pdIdent(~Stage-1)))
f1_POMS2_TA <- lme(fixed = POMS2_TA ~ Stage*NS, data = SukoPOMS, weights = varIdent(form = ~ 1 | Surgeon), random = ~ 1 | Surgeon)
f1_POMS2_VA <- lme(fixed = POMS2_VA ~ Stage*NS, data = SukoPOMS, correlation = corAR1(), random = list(Surgeon = pdIdent(~Stage-1)))
f1_POMS2_F <- lme(fixed = POMS2_F ~ Stage*NS, data = SukoPOMS, weights = varIdent(form = ~ 1 | Surgeon), random = list(Surgeon = pdIdent(~Stage-1)))

summary(f1_POMS2_AH)
intervals(f1_POMS2_AH)

summary(f1_POMS2_CB)
intervals(f1_POMS2_CB)

summary(f1_POMS2_DD)
intervals(f1_POMS2_DD)

summary(f1_POMS2_FI)
intervals(f1_POMS2_FI)

summary(f1_POMS2_TA)
intervals(f1_POMS2_TA)

summary(f1_POMS2_VA)
intervals(f1_POMS2_VA)

summary(f1_POMS2_F)
intervals(f1_POMS2_F)

# Model 2
f2_POMS2_AH <- lme(fixed = POMS2_AH ~ Stage*NS*SC, data = SukoPOMS, weights = varIdent(form = ~ 1 | Surgeon), random = ~ 1 | Surgeon)
f2_POMS2_CB <- lme(fixed = POMS2_CB ~ Stage*NS*SC, data = SukoPOMS, weights = varIdent(form = ~ 1 | Surgeon), random = ~ 1 | Surgeon)
f2_POMS2_DD <- lme(fixed = POMS2_DD ~ Stage*NS*SC, data = SukoPOMS, weights = varIdent(form = ~ 1 | Surgeon), random = ~ 1 | Surgeon)
f2_POMS2_FI <- lme(fixed = POMS2_FI ~ Stage*NS*SC, data = SukoPOMS, correlation = corAR1(), random = list(Surgeon = pdIdent(~Stage-1)))
f2_POMS2_TA <- lme(fixed = POMS2_TA ~ Stage*NS*SC, data = SukoPOMS, weights = varIdent(form = ~ 1 | Surgeon), random = ~ 1 | Surgeon)
f2_POMS2_VA <- lme(fixed = POMS2_VA ~ Stage*NS*SC, data = SukoPOMS, correlation = corAR1(), random = list(Surgeon = pdIdent(~Stage-1)))
f2_POMS2_F <- lme(fixed = POMS2_F ~ Stage*NS*SC, data = SukoPOMS, weights = varIdent(form = ~ 1 | Surgeon), random = list(Surgeon = pdIdent(~Stage-1)))

summary(f2_POMS2_AH)
intervals(f2_POMS2_AH)

summary(f2_POMS2_CB)
intervals(f2_POMS2_CB)

summary(f2_POMS2_DD)
intervals(f2_POMS2_DD)

summary(f2_POMS2_FI)
intervals(f2_POMS2_FI)

summary(f2_POMS2_TA)
intervals(f2_POMS2_TA)

summary(f2_POMS2_VA)
intervals(f2_POMS2_VA)

summary(f2_POMS2_F)
intervals(f2_POMS2_F)