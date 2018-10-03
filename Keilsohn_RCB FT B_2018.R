#### William Keilsohn
### RCB, Friedman's Test, and Blocking

### Load Packages
library(dplyr)
library(xlsx)
library(agricolae)

### Start Running the Tests

# Individuals on differeing sleep scheduals are compared
# The regiments are provided
# All individuals undergo all regiments
# Blood glucose levels are measured
RCB.out1<-aov(Yld~Fert+Block, data = MD)
anova(RCB.out1) # This line and the following do the same thing. This just gives you one more decemil
summary(RCB.out1)
A1<-aov(Yld~Fert, data = MD)
shapiro.test(residuals(A1))# Ignore blocks for assumptions
shapiro.test(residuals(RCB.out1))
bartlett.test(MD$Yld,MD$Fert) # Ignore blocks for assumptions

MD<-mutate(MD, HolyShit = log(Yld))
A4<-aov(HolyShit~Fert, data = MD)
shapiro.test(residuals(A4))
F1<- friedman.test(MD$Yld,MD$Fert,MD$Block)

# Algae media is compared for growth
# Three types of media are used
# All recive the same strain of algae
# Lipid production is measured as a proxy for growth
D1$Student<-as.factor(D1$Student)
D1$Sleep<-as.factor(D1$Sleep)

RCB.out2<-aov(Blood~Sleep+Student, data = D1)
shapiro.test(residuals(RCB.out2))
A2<-aov(Blood~Sleep, data = D1)
shapiro.test(residuals(A2))
bartlett.test(D1$Blood, D1$Sleep)
out1<-HSD.test(RCB.out2, "Sleep")


# Question 3:
D2$Media<-as.factor(D2$Media)
D2$Algae<-as.factor(D2$Algae)
D2<-D2[,1:3]
D2<-na.omit(D2) ### Cleaning data b/c I have nothing else better to do with my short sad life

RCB.out3<-aov(Growth~Media+Algae, data = D2)
shapiro.test(residuals(RCB.out3))
A3<-aov(Growth~Media, data = D2)
shapiro.test(residuals(A3))
bartlett.test(D2$Growth, D2$Media)
summary(RCB.out3)
out2<-HSD.test(RCB.out3, "Media")
