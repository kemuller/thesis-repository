load(url("http://dl.dropbox.com/s/e8xg6k2cqvalnhj/cg12004-2012.RData"))


ss <- dt[dt$expNm==1997 & dt$Status=="basal",] #only non-flowering plants
plot(sort(ss$basalLfCt))
#make a category for basal leaf count
ss[ss$basalLfCt>=1 & ss$basalLfCt <= 3,"lfClass"] <- "1-3"
ss[ss$basalLfCt>=4 & ss$basalLfCt <= 6,"lfClass"] <- "4-6"
ss[ss$basalLfCt>=7,"lfClass"] <- ">6"
ss$lfClass <- factor(ss$lfClass,levels=c("1-3","4-6",">6"))
table(ss$lfClass)

ss$measureYr <- factor(ss$measureYr)
#make measure date a number, giving the days since Jan 1st
ss[ss$measureYr==2004,"dateNum"] <- ss[ss$measureYr==2004,"measureDt2"]- as.Date("2004-01-01",format=("%Y-%m-%d"))
ss[ss$measureYr==2005,"dateNum"] <- ss[ss$measureYr==2005,"measureDt2"]- as.Date("2005-01-01",format=("%Y-%m-%d"))
ss[ss$measureYr==2006,"dateNum"] <- ss[ss$measureYr==2006,"measureDt2"]- as.Date("2006-01-01",format=("%Y-%m-%d"))
ss[ss$measureYr==2007,"dateNum"] <- ss[ss$measureYr==2007,"measureDt2"]- as.Date("2007-01-01",format=("%Y-%m-%d"))
ss[ss$measureYr==2008,"dateNum"] <- ss[ss$measureYr==2008,"measureDt2"]- as.Date("2008-01-01",format=("%Y-%m-%d"))

#ss$cgPlaId <- factor(ss$cgPlaId)
ss$row <- factor(ss$row)
ss$pos <- factor(ss$pos)

#Exclude dates with only a few measurements:
table(ss$measureDt2,ss$measureYr)
ss <- ss[!ss$measureDt2 %in% c("2005-08-29","2006-08-03","2006-08-05","2007-08-08","2008-07-23"),]
ss$measureDt2 <- factor(ss$measureDt2,levels=unique(ss$measureDt2))


#visualize change in chewhole by year (split up by cgPlaID for visualization)
library(lattice)

xyplot(chewhole~measureYr |cgPlaId, data=ss[ss$cgPlaId %in% c(649:745),] , 
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.lmline(x,y)
       }, ylim=c(-2,2),as.table=T)
xyplot(chewhole~measureYr |cgPlaId, data=ss[ss$cgPlaId %in% c(746:832),] , 
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.lmline(x,y)
       }, ylim=c(-2,2),as.table=T)
xyplot(chewhole~measureYr |cgPlaId, data=ss[ss$cgPlaId %in% c(833:934),] , 
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.lmline(x,y)
       }, ylim=c(-2,2),as.table=T)
xyplot(chewhole~measureYr |cgPlaId, data=ss[ss$cgPlaId %in% c(935:1035),] , 
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.lmline(x,y)
       }, ylim=c(-2,2),as.table=T)
xyplot(chewhole~measureYr |cgPlaId, data=ss[ss$cgPlaId %in% c(1036:1135),] , 
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.lmline(x,y)
       }, ylim=c(-2,2),as.table=T)

xyplot(chewhole~measureYr |cgPlaId, data=ss[ss$cgPlaId %in% c(1136:9182),] , 
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.lmline(x,y)
       }, ylim=c(-2,2),as.table=T)

###model: GLMM

library(lme4)

#backwards model selection
a9 <- glmer(chewhole ~  lfClass*aphids2 + measureYr*dateNum +row + pos + (1|cgPlaId), family=binomial, data=ss)
a8.1 <- glmer(chewhole ~  lfClass + aphids2 + measureYr*dateNum +row + pos + (1|cgPlaId), family=binomial, data=ss)
a8.2 <- glmer(chewhole ~  lfClass*aphids2 + measureYr + dateNum +row + pos + (1|cgPlaId), family=binomial, data=ss)
anova(a8.1,a9)
anova(a8.2,a9)
a8 <- glmer(chewhole ~  lfClass*aphids2 + measureYr + dateNum +row + pos + (1|cgPlaId), family=binomial, data=ss)
a7 <- glmer(chewhole ~  lfClass + aphids2 + measureYr + dateNum +row + pos + (1|cgPlaId), family=binomial, data=ss)
anova(a7,a8)
a6.1 <- glmer(chewhole ~            aphids2 + measureYr + dateNum +row + pos + (1|cgPlaId), family=binomial, data=ss)
a6.2 <- glmer(chewhole ~  lfClass           + measureYr + dateNum +row + pos + (1|cgPlaId), family=binomial, data=ss)
a6.3 <- glmer(chewhole ~  lfClass + aphids2             + dateNum +row + pos + (1|cgPlaId), family=binomial, data=ss)
a6.4 <- glmer(chewhole ~  lfClass + aphids2 + measureYr           +row + pos + (1|cgPlaId), family=binomial, data=ss)
a6.5 <- glmer(chewhole ~  lfClass + aphids2 + measureYr + dateNum      + pos + (1|cgPlaId), family=binomial, data=ss)
a6.6 <- glmer(chewhole ~  lfClass + aphids2 + measureYr + dateNum +row       + (1|cgPlaId), family=binomial, data=ss)
anova(a6.1,a7)
anova(a6.2,a7)
anova(a6.3,a7)
anova(a6.4,a7)
anova(a6.5,a7)
anova(a6.6,a7)

a6 <- glmer(chewhole ~  lfClass + aphids2 + measureYr+row + pos + (1|cgPlaId), family=binomial, data=ss)
a5 <- glmer(chewhole ~  lfClass + aphids2 + measureYr+row + (1|cgPlaId), family=binomial, data=ss)
anova(a5,a6)
a4.1 <- glmer(chewhole ~            aphids2 + measureYr+row + (1|cgPlaId), family=binomial, data=ss)
a4.2 <- glmer(chewhole ~  lfClass           + measureYr+row + (1|cgPlaId), family=binomial, data=ss)
a4.3 <- glmer(chewhole ~  lfClass + aphids2            +row + (1|cgPlaId), family=binomial, data=ss)
a4.4 <- glmer(chewhole ~  lfClass + aphids2 + measureYr     + (1|cgPlaId), family=binomial, data=ss)
anova(a4.1,a5)
anova(a4.2,a5)
anova(a4.3,a5)
anova(a4.4,a5)
a3 <- glmer(chewhole ~  lfClass + aphids2 + measureYr+ (1|cgPlaId), family=binomial, data=ss)
a2.1 <- glmer(chewhole ~          aphids2 + measureYr+ (1|cgPlaId), family=binomial, data=ss)
a2.2 <- glmer(chewhole ~  lfClass         + measureYr+ (1|cgPlaId), family=binomial, data=ss)
a2.3 <- glmer(chewhole ~  lfClass + aphids2          + (1|cgPlaId), family=binomial, data=ss)
anova(a2.1,a3)
anova(a2.2,a3)
anova(a2.3,a3)

#best model:
best <- glmer(chewhole ~ aphids2 + lfClass + measureYr + row + (1|cgPlaId),family=binomial,data=ss)

#plot random effects against the predicted values from the fixed effect component of the model 
# and check for no trend
m <- model.matrix(best)
ss$ft.fix <- as.numeric(m %*% fixef(best))
rr <- ranef(best,drop=TRUE)$cgPlaId
rrr <- data.frame(cgPlaId = attr(rr,"names"),ranef=rr)
ss <- merge(ss,rrr,all.x=TRUE)
str(ss)

head(m)
fixef(best)
head(rrr)
head(ss)


#the main diagnostic plot should be estimated random effects vs. fitted values from GLMM fit.

####visualize for every factor:
#aphid abundance
par(mfrow=c(2,2))
with(ss[ss$aphids2=="0",],plot(ft.fix,ranef,pch=19,las=1,cex=1))
abline(0,0,lwd=1.5)
with(ss[ss$aphids2=="1-10",],plot(ft.fix,ranef,pch=19,las=1,cex=1))
abline(0,0,lwd=1.5)
with(ss[ss$aphids2=="11-80",],plot(ft.fix,ranef,pch=19,las=1,cex=1))
abline(0,0,lwd=1.5)
with(ss[ss$aphids2==">80",],plot(ft.fix,ranef,pch=19,las=1,cex=1))
abline(0,0,lwd=1.5)
par(mfrow=c(1,1))

#measure year
par(mfrow=c(2,3))
with(ss[ss$measureYr=="2004",],plot(ft.fix,ranef,pch=19,las=1,cex=1))
abline(0,0,lwd=1.5)
with(ss[ss$measureYr=="2005",],plot(ft.fix,ranef,pch=19,las=1,cex=1))
abline(0,0,lwd=1.5)
with(ss[ss$measureYr=="2006",],plot(ft.fix,ranef,pch=19,las=1,cex=1))
abline(0,0,lwd=1.5)
with(ss[ss$measureYr=="2007",],plot(ft.fix,ranef,pch=19,las=1,cex=1))
abline(0,0,lwd=1.5)
with(ss[ss$measureYr=="2008",],plot(ft.fix,ranef,pch=19,las=1,cex=1))
abline(0,0,lwd=1.5)

#row
par(mfrow=c(2,2))
plot(ft.fix ~ aphids2, data=ss)
plot(ft.fix ~ measureYr, data=ss)
plot(ft.fix ~ lfClass, data=ss)
plot(ft.fix ~ row, data=ss)
par(mfrow=c(1,1))

ss[ss$ft.fix < -3,]


ss$fit <- fitted(best)

plot(fit~aphids2,data=ss)

##check for normality of random effects
qqnorm(rr, pch = 19, las = 1, cex = 1)
abline(0,1,lwd = 1.5) #the slope is much less than 1--not sure what that means for the model


head(sort(rr))

inv.logit <- function(x) exp(x)/(1+exp(x))

plot(inv.logit(ft.fix) ~ aphids2, data=ss)


#question: what's the best way to present the model results?
#how do I present the fitted values as barplots, say, for a typical individual in a particular year?

#qualities of typical individual?
newd <- data.frame(aphids2 = rep(c("0","1-10","11-80",">80"),each=2),row=median(as.numeric(as.character(ss$row))),measureYr="2005",lfClass="4-6",chewhole=rep(c(0,1),4))
