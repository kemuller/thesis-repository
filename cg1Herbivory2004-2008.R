load(url("http://dl.dropbox.com/s/e8xg6k2cqvalnhj/cg12004-2012.RData"))
load("C:\\Users\\Katherine\\Dropbox\\KMullerdatasets\\cg12004-2012.RData") #for use on my computer, when I'm disconnected from the internet

#there are 5 possible analysis for herbivory (i.e. data collected within 10 days)
#     expNm     date       dayspan      range
#    1997      2004         5    "2004-08-07" "2004-08-12"
#    1999      2004         8    "2004-08-03" "2004-08-11"
#    1996      2005         8    "2005-08-11" "2005-08-19"
#    1999      2005         5    "2005-07-28" "2005-08-02"
#    1996      2007         6    "2007-07-19" "2007-07-25"

#for 2004 I could combine the 1997 and 1999 experiments into a single analysis


#visualize distribution of chewhole damage among years and experiments:
#setwd("C:\\Users\\Katherine\\Dropbox\\thesisResults\\summaryGraphs")
#png(file="chewHoleDamageBySourcePopCG1.png",width=800,height=800)
propChewhole <- round(tapply(dt$chewhole,list(dt$expNm,dt$measureYr),mean),digits=2)
cols <- c(gray(seq(0,1,by=1/3)))
bp <- barplot(propChewhole,beside=T,ylim=c(0,1),col=rep(cols,5),ylab="prop. of plants with fol. damage")
legend("topleft",legend=unique(dt$expNm),fill=cols,bty="n")
#dev.off()

d96 <- dt[dt$expNm==1996,]
d97 <- dt[dt$expNm==1997,]
d98 <- dt[dt$expNm==1998,]
d99 <- dt[dt$expNm==1999,]

table(d98$siteOfOriginPedigree) # all plants in the 1998 exp had unknown pedigree

(chewholeBySite96 <- na.omit(round(tapply(d96$chewhole,list(d96$siteOfOriginPedigree,d96$measureYr),mean),digits=2)))
(chewholeBySite97 <- na.omit(round(tapply(d97$chewhole,list(d97$siteOfOriginPedigree,d97$measureYr),mean),digits=2)))
(chewholeBySite98 <- na.omit(round(tapply(d98$chewhole,list(d98$siteOfOriginPedigree,d98$measureYr),mean),digits=2)))
(chewholeBySite99 <- na.omit(round(tapply(d99$chewhole,list(d99$siteOfOriginPedigree,d99$measureYr),mean),digits=2)))

#it looks like there may be differences in chewhole damage among sites--but I won't include this in analysis

par(mfcol=c(2,2))
barplot(chewholeBySite96,beside=T)
barplot(chewholeBySite97,beside=T)
barplot(chewholeBySite98,beside=T)
barplot(chewholeBySite99,beside=T)
par(mfcol=c(1,1))
#the proportion of plants with herbivore damage differs among experiments in each year:
# the 1996 garden is highest in all years except 2007, when 1998 is highest.
# the 1999 garden is lowest in 2004-2006

#perhaps basalLfCt should be a category (1-3, 4-5, >5)--same as other analyses
dt[dt$basalLfCt >= 1 & dt$basalLfCt <= 3,"lfClass" ] <- "1-3"
dt[dt$basalLfCt >= 4 & dt$basalLfCt <= 5,"lfClass" ] <- "4-5"
dt[dt$basalLfCt >= 6,"lfClass" ] <- ">5"
dt[dt$Status == "flowering","lfClass" ] <- "flowering"

dt$lfClass <- factor(dt$lfClass, levels=c("1-3","4-5",">5","flowering"))
table(dt$lfClass) #not super balanced

#remove can'tFind's
dt <- dt[dt$Status !="can'tFind",]
dt$Status <- factor(dt$Status,levels=c("basal","flowering"))
#extract years and experiments in which the data were collected within a week (keep experiments separate)
#that makes 5 dataframes:
d1 <- dt[dt$expNm %in% c(1997) & dt$measureYr==2004,]
d2 <- dt[dt$expNm %in% c(1999) & dt$measureYr==2004 ,]
d3 <- dt[dt$expNm %in% 1996 & dt$measureYr==2005 ,]
d4 <- dt[dt$expNm %in% 1999 & dt$measureYr==2005 ,]
d5 <- dt[dt$expNm %in% 1996 & dt$measureYr==2007,]

#summary statistics:
#plant size/status in each experiment/year:

table(d1$lfClass)
round(tapply(d1$chewhole,d1$lfClass,mean),digits=2)
table(d2$lfClass)
round(tapply(d2$chewhole,d2$lfClass,mean),digits=2)
table(d3$lfClass)
round(tapply(d3$chewhole,d3$lfClass,mean),digits=2)
table(d4$lfClass)
round(tapply(d4$chewhole,d4$lfClass,mean),digits=2)
table(d5$lfClass)
round(tapply(d5$chewhole,d5$lfClass,mean),digits=2)

table(d1$Status)
round(tapply(d1$chewhole,d1$Status,mean),digits=2)
table(d2$Status)
round(tapply(d2$chewhole,d2$Status,mean),digits=2)
table(d3$Status)
round(tapply(d3$chewhole,d3$Status,mean),digits=2)
table(d4$Status)
round(tapply(d4$chewhole,d4$Status,mean),digits=2)
table(d5$Status)
round(tapply(d5$chewhole,d5$Status,mean),digits=2)


#chewhole damage by aphid abundance:
list(c(round(tapply(d1$chewhole,d1$aphids2,mean),digits=3)),c(tapply(d1$chewhole,d1$aphids2,length)))
list(c(round(tapply(d2$chewhole,d2$aphids2,mean),digits=3)),c(tapply(d2$chewhole,d2$aphids2,length)))
list(c(round(tapply(d3$chewhole,d3$aphids2,mean),digits=3)),c(tapply(d3$chewhole,d3$aphids2,length)))
list(c(round(tapply(d4$chewhole,d4$aphids2,mean),digits=3)),c(tapply(d4$chewhole,d4$aphids2,length)))
list(c(round(tapply(d5$chewhole,d5$aphids2,mean),digits=3)),c(tapply(d5$chewhole,d5$aphids2,length)))
#chewhole damage is greater with increasing aphid abundance in most years--opposite of addition/exclusion experiment
table(d1$aphids2)
barplot(tapply(d1$chewhole,d1$aphids2,mean))

#chewhole damage by plant status:

tapply(d1$chewhole,d1$Status,mean)
tapply(d2$chewhole,d2$Status,mean)
tapply(d3$chewhole,d3$Status,mean)
tapply(d4$chewhole,d4$Status,mean)
tapply(d5$chewhole,d5$Status,mean)


#chewhole damage by site origin:
#adjust factor levels
d1$siteOfOriginPedigree <- factor(d1$siteOfOriginPedigree,levels= unique(d1$siteOfOriginPedigree))                                     
d1$siteOfOriginPedigree <- factor(d1$siteOfOriginPedigree,levels= dimnames(sort(table(d1$siteOfOriginPedigree)))[[1]])                                     
d2$siteOfOriginPedigree <- factor( d2$siteOfOriginPedigree,levels= unique( d2$siteOfOriginPedigree))                                     
d2$siteOfOriginPedigree <- factor(d2$siteOfOriginPedigree,levels=dimnames(sort(table(d2$siteOfOriginPedigree)))[[1]])
d3$siteOfOriginPedigree <- factor(d3$siteOfOriginPedigree,levels= unique(d3$siteOfOriginPedigree))                                     
d3$siteOfOriginPedigree <- factor(d3$siteOfOriginPedigree,levels=dimnames(sort(table(d3$siteOfOriginPedigree)))[[1]])
d4$siteOfOriginPedigree <- factor(d4$siteOfOriginPedigree,levels= unique(d4$siteOfOriginPedigree))                                     
d4$siteOfOriginPedigree <- factor(d4$siteOfOriginPedigree,levels=dimnames(sort(table(d4$siteOfOriginPedigree)))[[1]])
d5$siteOfOriginPedigree <- factor(d5$siteOfOriginPedigree,levels= unique(d5$siteOfOriginPedigree))                                     
d5$siteOfOriginPedigree <- factor(d5$siteOfOriginPedigree,levels=dimnames(sort(table(d5$siteOfOriginPedigree)))[[1]])

lookAtChews <- function(x){
ens <- as.vector(table(x$siteOfOriginPedigree))
bp <- barplot(tapply(x$chewhole,x$siteOfOriginPedigree,mean),ylim=c(0,1),cex.names=0.9)
legend("topleft",legend=c(paste(unique(x$expNm),"exp",sep=""),unique(x$measureYr)),bty="n")
text(bp,0.05,labels=ens)
}
windows()
lookAtChews(d1)
lookAtChews(d2)
lookAtChews(d3)
lookAtChews(d4)
lookAtChews(d5)
par(mfcol=c(1,1))
ens <- as.vector(table(d2$siteOfOriginPedigree))
bp <- barplot(tapply(d2$chewhole,d2$siteOfOriginPedigree,mean))
legend("topleft",legend=c(paste(unique(d2$expNm),"exp",sep=""),unique(d2$measureYr)),bty="n")
text(bp,0.05,labels=ens)

#the proportion of plants with chewhole damage in basal vs. flowering plants differs in some years, but not in others.
# it wouldn't hurt to separate out basal plants-to be consistent with other analysis.
d1b <- d1[d1$Status=="basal",]
d2b <- d2[d2$Status=="basal",]
d3b <- d3[d3$Status=="basal",]
d4b <- d4[d4$Status=="basal",]
d5b <- d5[d5$Status=="basal",]

d1f <- d1[d1$Status=="flowering",]
d2f <- d2[d2$Status=="flowering",]
d3f <- d3[d3$Status=="flowering",]
d4f <- d4[d4$Status=="flowering",]
d5f <- d5[d5$Status=="flowering",]


#chewhole damage by aphid abundance (basal plants):
tapply(d1b$chewhole,d1b$aphids2,mean)
tapply(d2b$chewhole,d2b$aphids2,mean)
tapply(d3b$chewhole,d3b$aphids2,mean)
tapply(d4b$chewhole,d4b$aphids2,mean)
tapply(d5b$chewhole,d5b$aphids2,mean)
tapply(d5b$chewhole,list(d5b$aphids2,d5b$lfClass),mean)

tapply(d1f$chewhole,d1f$aphids2,mean)
tapply(d2f$chewhole,d2f$aphids2,mean)
tapply(d3f$chewhole,d3f$aphids2,mean)
tapply(d4f$chewhole,d4f$aphids2,mean)
tapply(d5f$chewhole,d5f$aphids2,mean)

#basal plants:
#chewhole damage is greater with higher aphid abundance in all years/exps except for d3 (1996 garden, 2005)

#flowering plants: the pattern is similar, but less strong--most pronounced in d3 (1996 garden, 2005)
####chewhole damage in 1997 exp. in 2004:-basal plants only

##which interaction terms to include?
table(d1b$basalLfCt,d1b$aphids2)
table(d1b$basalLfCt,d1b$row)
table(d1b$basalLfCt,d1b$pos)
table(d1b$aphids2,d1b$row)
table(d1b$aphids2,d1b$pos)


#start with basal leaf count:
#predictors: location (row,pos), plant size (basalLfCt), aphid abundance 
a11 <-  glm(chewhole ~ row*lfClass*aphids2 + pos*lfClass*aphids2,data=d1b,family=quasibinomial)
summary(a11)
a11.1 <- update(a11, ~.-row:lfClass:aphids2 )
a11.2 <- update(a11, ~.-pos:lfClass:aphids2 )
anova(a11.1,a11,test="F")
anova(a11.2,a11,test="F")

a10 <- a11.1

a9 <-  glm(chewhole ~ row*lfClass + row*aphids2 + pos*lfClass + pos*aphids2 + lfClass:aphids2,data=d1b,family=quasibinomial)
a9.1 <- update(a9,~.-row:lfClass)
a9.2 <- update(a9,~.-row:aphids2)
a9.3 <- update(a9,~.-pos:lfClass)
a9.4 <- update(a9,~.-pos:aphids2)
a9.5 <- update(a9,~.-lfClass:aphids2)
anova(a9.1,a9,test="F")
anova(a9.2,a9,test="F")
anova(a9.3,a9,test="F")
anova(a9.4,a9,test="F")
anova(a9.5,a9,test="F")

a8 <- a9.3
a9.1 <- update(a8,~.-row:lfClass)
a9.2 <- update(a8,~.-row:aphids2)
#a9.3 <- update(a8,~.-pos:lfClass)
a9.4 <- update(a8,~.-pos:aphids2)
a9.5 <- update(a8,~.-lfClass:aphids2)
anova(a9.1,a8,test="F")
anova(a9.2,a8,test="F")
#anova(a9.3,a8,test="F")
anova(a9.4,a8,test="F")
anova(a9.5,a8,test="F")

a7 <- a9.4
a9.1 <- update(a7,~.-row:lfClass)
a9.2 <- update(a7,~.-row:aphids2)
#a9.3 <- update(a7,~.-pos:lfClass)
#a9.4 <- update(a7,~.-pos:aphids2)
a9.5 <- update(a7,~.-lfClass:aphids2)
anova(a9.1,a7,test="F")
anova(a9.2,a7,test="F")
#anova(a9.3,a7,test="F")
#anova(a9.4,a7,test="F")
anova(a9.5,a7,test="F")

a6 <- a9.1
#a9.1 <- update(a6,~.-row:lfClass)
a9.2 <- update(a6,~.-row:aphids2)
#a9.3 <- update(a6,~.-pos:lfClass)
#a9.4 <- update(a6,~.-pos:aphids2)
a9.5 <- update(a6,~.-lfClass:aphids2)
#anova(a9.1,a6,test="F")
anova(a9.2,a6,test="F")
#anova(a9.3,a6,test="F")
#anova(a9.4,a6,test="F")
anova(a9.5,a6,test="F")

a5 <- a9.2
a4 <- glm(chewhole ~ row + pos + lfClass + aphids2,data=d1b,family=quasibinomial)
a4.1 <- update(a4,~.-row)
a4.2 <- update(a4,~.-pos)
a4.3 <- update(a4,~.-lfClass)
a4.4 <- update(a4,~.-aphids2)
anova(a4.1,a4,test="F")
anova(a4.2,a4,test="F")
anova(a4.3,a4,test="F")
anova(a4.4,a4,test="F")

a3 <- a4.3
a4.1 <- update(a3,~.-row)
a4.2 <- update(a3,~.-pos)
#a4.3 <- update(a3,~.-lfClass)
a4.4 <- update(a3,~.-aphids2)
anova(a4.1,a3,test="F")
anova(a4.2,a3,test="F")
#anova(a4.3,a3,test="F")
anova(a4.4,a3,test="F")

a2 <- a4.2
a4.1 <- update(a2,~.-row)
#a4.2 <- update(a2,~.-pos)
#a4.3 <- update(a2,~.-lfClass)
a4.4 <- update(a2,~.-aphids2)
anova(a4.1,a2,test="F")
#anova(a4.2,a2,test="F")
#anova(a4.3,a2,test="F")
anova(a4.4,a2,test="F")

a1 <- a4.1

null <-  glm(chewhole ~ 1,data=d1b,family=quasibinomial)
anova(null,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,test="F")
anova(null,a1,a2,a3,a4,a5,a6,a7,a8,a9,test="F")


#check model:
###best model:##1997 Garden, 2004
opt <- glm(chewhole ~ row + pos + aphids2,data=d1b,family=quasibinomial)


#visualize:
par(mfcol=c(2,2))
plot(opt)
par(mfcol=c(1,1))

median(d1b$row)
median(d1b$pos)
stem(d1b$basalLfCt)
    #fitted values for mode leaf count (4) in middle (row 25)
newd <- data.frame(aphids2=c("0","1-10","11-80",">80"),row=rep(25,4),pos=rep(974,4),lfClass=rep("4-5",4))
pred <- predict(opt,type="response", se.fit=T ,newdata=newd)
newd$fit <- pred$fit
newd$se <- pred$se.fit
newd
bp <- barplot(newd$fit,ylim=c(0,0.8),ylab="prop. of plants with fol. Damage")
axis(1,at=bp,labels=c("0","1-10","11-80",">80"))
segments(bp,newd$fit+newd$se,bp,newd$fit-newd$se)

##########
# 1999 exp, 2004
a11 <-  glm(chewhole ~ row*lfClass*aphids2 + pos*lfClass*aphids2,data=d2b,family=quasibinomial)
summary(a11)
a11.1 <- update(a11, ~.-row:lfClass:aphids2 )
a11.2 <- update(a11, ~.-pos:lfClass:aphids2 )
anova(a11.1,a11,test="F")
anova(a11.2,a11,test="F")

a10 <- a11.1

a9 <-  glm(chewhole ~ row*lfClass + row*aphids2 + pos*lfClass + pos*aphids2 + lfClass:aphids2,data=d2b,family=quasibinomial)
a9.1 <- update(a9,~.-row:lfClass)
a9.2 <- update(a9,~.-row:aphids2)
a9.3 <- update(a9,~.-pos:lfClass)
a9.4 <- update(a9,~.-pos:aphids2)
a9.5 <- update(a9,~.-lfClass:aphids2)
anova(a9.1,a9,test="F")
anova(a9.2,a9,test="F")
anova(a9.3,a9,test="F")
anova(a9.4,a9,test="F")
anova(a9.5,a9,test="F")

a8 <- a9.5
a9.1 <- update(a8,~.-row:lfClass)
a9.2 <- update(a8,~.-row:aphids2)
a9.3 <- update(a8,~.-pos:lfClass)
a9.4 <- update(a8,~.-pos:aphids2)
#a9.5 <- update(a8,~.-lfClass:aphids2)
anova(a9.1,a8,test="F")
anova(a9.2,a8,test="F")
anova(a9.3,a8,test="F")
anova(a9.4,a8,test="F")
#anova(a9.5,a8,test="F")

a7 <- a9.3
a9.1 <- update(a7,~.-row:lfClass)
a9.2 <- update(a7,~.-row:aphids2)
#a9.3 <- update(a7,~.-pos:lfClass)
a9.4 <- update(a7,~.-pos:aphids2)
#a9.5 <- update(a7,~.-lfClass:aphids2)
anova(a9.1,a7,test="F")
anova(a9.2,a7,test="F")
#anova(a9.3,a7,test="F")
anova(a9.4,a7,test="F")
#anova(a9.5,a7,test="F")

a6 <- a9.1
#a9.1 <- update(a6,~.-row:lfClass)
a9.2 <- update(a6,~.-row:aphids2)
#a9.3 <- update(a6,~.-pos:lfClass)
a9.4 <- update(a6,~.-pos:aphids2)
#a9.5 <- update(a6,~.-lfClass:aphids2)
#anova(a9.1,a6,test="F")
anova(a9.2,a6,test="F")
#anova(a9.3,a6,test="F")
anova(a9.4,a6,test="F")
#anova(a9.5,a6,test="F")

a5 <- a9.4
a4 <- glm(chewhole ~ row + pos + lfClass + aphids2,data=d2b,family=quasibinomial)
a4.1 <- update(a4,~.-row)
a4.2 <- update(a4,~.-pos)
a4.3 <- update(a4,~.-lfClass)
a4.4 <- update(a4,~.-aphids2)
anova(a4.1,a4,test="F")
anova(a4.2,a4,test="F")
anova(a4.3,a4,test="F")
anova(a4.4,a4,test="F")

a3 <- a4.2
a4.1 <- update(a3,~.-row)
#a4.2 <- update(a3,~.-pos)
a4.3 <- update(a3,~.-lfClass)
a4.4 <- update(a3,~.-aphids2)
anova(a4.1,a3,test="F")
#anova(a4.2,a3,test="F")
anova(a4.3,a3,test="F")
anova(a4.4,a3,test="F")

a2 <- a4.3
a4.1 <- update(a2,~.-row)
#a4.2 <- update(a2,~.-pos)
#a4.3 <- update(a2,~.-lfClass)
a4.4 <- update(a2,~.-aphids2)
anova(a4.1,a2,test="F")
#anova(a4.2,a2,test="F")
#anova(a4.3,a2,test="F")
anova(a4.4,a2,test="F")

a1 <- a4.1

null <-  glm(chewhole ~ 1,data=d2b,family=quasibinomial)
anova(null,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,test="F")

#best model:
#opt <- glm(chewhole ~ row + basalLfCt + aphids2 + row:aphids2,data=d2b,family=quasibinomial)
opt <- glm(chewhole ~ row + lfClass + aphids2 + row:aphids2,data=d2b,family=quasibinomial)
anova(opt,test="F")
#visualize:
par(mfcol=c(2,2))
plot(opt)
par(mfcol=c(1,1))

median(d2b$row)
median(d2b$pos)

stem(d2b$basalLfCt)
table(d2b$lfClass)
#fitted values for mode leaf count (4) in middle (row 25)
newd <- data.frame(aphids2=c("0","1-10","11-80",">80"),row=rep(46,4),lfClass=rep(">5",4))
pred <- predict(opt,type="response", se.fit=T ,newdata=newd)
newd$fit <- pred$fit
newd$se <- pred$se.fit
newd
bp <- barplot(newd$fit,ylim=c(0,0.8),ylab="prop. of plants with fol. Damage")
axis(1,at=bp,labels=c("0","1-10","11-80",">80"))
segments(bp,newd$fit+newd$se,bp,newd$fit-newd$se)

###########
#1996 garden 2005

a11 <-  glm(chewhole ~ row*lfClass*aphids2 + pos*lfClass*aphids2,data=d3b,family=quasibinomial)
summary(a11)
a11.1 <- update(a11, ~.-row:lfClass:aphids2 )
a11.2 <- update(a11, ~.-pos:lfClass:aphids2 )
anova(a11.1,a11,test="F")
anova(a11.2,a11,test="F")

a10 <- a11.2

a9 <-  glm(chewhole ~ row*lfClass + row*aphids2 + pos*lfClass + pos*aphids2 + lfClass:aphids2,data=d3b,family=quasibinomial)
a9.1 <- update(a9,~.-row:lfClass)
a9.2 <- update(a9,~.-row:aphids2)
a9.3 <- update(a9,~.-pos:lfClass)
a9.4 <- update(a9,~.-pos:aphids2)
a9.5 <- update(a9,~.-lfClass:aphids2)
anova(a9.1,a9,test="F")
anova(a9.2,a9,test="F")
anova(a9.3,a9,test="F")
anova(a9.4,a9,test="F")
anova(a9.5,a9,test="F")

a8 <- a9.1
#a9.1 <- update(a8,~.-row:lfClass)
a9.2 <- update(a8,~.-row:aphids2)
a9.3 <- update(a8,~.-pos:lfClass)
a9.4 <- update(a8,~.-pos:aphids2)
a9.5 <- update(a8,~.-lfClass:aphids2)
#anova(a9.1,a8,test="F")
anova(a9.2,a8,test="F")
anova(a9.3,a8,test="F")
anova(a9.4,a8,test="F")
anova(a9.5,a8,test="F")

a7 <- a9.2
#a9.1 <- update(a7,~.-row:lfClass)
#a9.2 <- update(a7,~.-row:aphids2)
a9.3 <- update(a7,~.-pos:lfClass)
a9.4 <- update(a7,~.-pos:aphids2)
a9.5 <- update(a7,~.-lfClass:aphids2)
#anova(a9.1,a7,test="F")
#anova(a9.2,a7,test="F")
anova(a9.3,a7,test="F")
anova(a9.4,a7,test="F")
anova(a9.5,a7,test="F")

a6 <- a9.3
#a9.1 <- update(a6,~.-row:lfClass)
#a9.2 <- update(a6,~.-row:aphids2)
#a9.3 <- update(a6,~.-pos:lfClass)
a9.4 <- update(a6,~.-pos:aphids2)
a9.5 <- update(a6,~.-lfClass:aphids2)
#anova(a9.1,a6,test="F")
#anova(a9.2,a6,test="F")
#anova(a9.3,a6,test="F")
anova(a9.4,a6,test="F")
anova(a9.5,a6,test="F")

a5 <- a9.5
a4 <- glm(chewhole ~ row + pos + lfClass + aphids2,data=d3b,family=quasibinomial)
a4.1 <- update(a4,~.-row)
a4.2 <- update(a4,~.-pos)
a4.3 <- update(a4,~.-lfClass)
a4.4 <- update(a4,~.-aphids2)
anova(a4.1,a4,test="F")
anova(a4.2,a4,test="F")
anova(a4.3,a4,test="F")
anova(a4.4,a4,test="F")

a3 <- a4.2
a4.1 <- update(a3,~.-row)
#a4.2 <- update(a3,~.-pos)
a4.3 <- update(a3,~.-lfClass)
a4.4 <- update(a3,~.-aphids2)
anova(a4.1,a3,test="F")
#anova(a4.2,a3,test="F")
anova(a4.3,a3,test="F")
anova(a4.4,a3,test="F")

a2 <- a4.4
a4.1 <- update(a2,~.-row)
#a4.2 <- update(a2,~.-pos)
a4.3 <- update(a2,~.-lfClass)
#a4.4 <- update(a2,~.-aphids2)
anova(a4.1,a2,test="F")
#anova(a4.2,a2,test="F")
anova(a4.3,a2,test="F")
#anova(a4.4,a2,test="F")

a1 <- a4.1

null <-  glm(chewhole ~ 1,data=d3b,family=quasibinomial)
anova(null,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,test="F")

#best model:

#opt <- glm(chewhole ~ row + basalLfCt + aphids2 + pos + aphids2:pos, data=d3b,family=quasibinomial)
opt <- glm(chewhole ~ row + lfClass + aphids2 + pos + aphids2:pos, data=d3b,family=quasibinomial)
#visualize:
par(mfcol=c(2,2))
plot(opt)
par(mfcol=c(1,1))

median(d3b$row)
median(d3b$pos)
range(d4b$pos)
table(d3b$lfClass)
#fitted values for mode leaf count (4) in middle (row 25)
#newd <- data.frame(aphids2=c("0","1-10","11-80",">80"),pos=rep(959,4),row=rep(28,4),basalLfCt=rep(3,4))
newd <- data.frame(aphids2=c("0","1-10","11-80",">80"),pos=rep(959,4),row=rep(28,4),lfClass=rep(">5",4))

pred <- predict(opt,type="response", se.fit=T ,newdata=newd)
newd$fit <- pred$fit
newd$se <- pred$se.fit
newd

###########
#1999 garden, 2005
a11 <-  glm(chewhole ~ row*lfClass*aphids2 + pos*lfClass*aphids2,data=d4b,family=quasibinomial)
summary(a11)
a11.1 <- update(a11, ~.-row:lfClass:aphids2 )
a11.2 <- update(a11, ~.-pos:lfClass:aphids2 )
anova(a11.1,a11,test="F")
anova(a11.2,a11,test="F")

a10 <- a11.2

a9 <-  glm(chewhole ~ row*lfClass + row*aphids2 + pos*lfClass + pos*aphids2 + lfClass:aphids2,data=d4b,family=quasibinomial)
a9.1 <- update(a9,~.-row:lfClass)
a9.2 <- update(a9,~.-row:aphids2)
a9.3 <- update(a9,~.-pos:lfClass)
a9.4 <- update(a9,~.-pos:aphids2)
a9.5 <- update(a9,~.-lfClass:aphids2)
anova(a9.1,a9,test="F")
anova(a9.2,a9,test="F")
anova(a9.3,a9,test="F")
anova(a9.4,a9,test="F")
anova(a9.5,a9,test="F")

a8 <- a9.2
a9.1 <- update(a8,~.-row:lfClass)
#a9.2 <- update(a8,~.-row:aphids2)
a9.3 <- update(a8,~.-pos:lfClass)
a9.4 <- update(a8,~.-pos:aphids2)
a9.5 <- update(a8,~.-lfClass:aphids2)
anova(a9.1,a8,test="F")
#anova(a9.2,a8,test="F")
anova(a9.3,a8,test="F")
anova(a9.4,a8,test="F")
anova(a9.5,a8,test="F")

a7 <- a9.1
#a9.1 <- update(a7,~.-row:lfClass)
#a9.2 <- update(a7,~.-row:aphids2)
a9.3 <- update(a7,~.-pos:lfClass)
a9.4 <- update(a7,~.-pos:aphids2)
a9.5 <- update(a7,~.-lfClass:aphids2)
#anova(a9.1,a7,test="F")
#anova(a9.2,a7,test="F")
anova(a9.3,a7,test="F")
anova(a9.4,a7,test="F")
anova(a9.5,a7,test="F")

a6 <- a9.5
#a9.1 <- update(a6,~.-row:lfClass)
#a9.2 <- update(a6,~.-row:aphids2)
a9.3 <- update(a6,~.-pos:lfClass)
a9.4 <- update(a6,~.-pos:aphids2)
#a9.5 <- update(a6,~.-lfClass:aphids2)
#anova(a9.1,a6,test="F")
#anova(a9.2,a6,test="F")
anova(a9.3,a6,test="F")
anova(a9.4,a6,test="F")
#anova(a9.5,a6,test="F")

a5 <- a9.3
a4 <- glm(chewhole ~ row + pos + lfClass + aphids2,data=d4b,family=quasibinomial)
a4.1 <- update(a4,~.-row)
a4.2 <- update(a4,~.-pos)
a4.3 <- update(a4,~.-lfClass)
a4.4 <- update(a4,~.-aphids2)
anova(a4.1,a4,test="F")
anova(a4.2,a4,test="F")
anova(a4.3,a4,test="F")
anova(a4.4,a4,test="F")

a3 <- a4.1
#a4.1 <- update(a3,~.-row)
a4.2 <- update(a3,~.-pos)
a4.3 <- update(a3,~.-lfClass)
a4.4 <- update(a3,~.-aphids2)
#anova(a4.1,a3,test="F")
anova(a4.2,a3,test="F")
anova(a4.3,a3,test="F")
anova(a4.4,a3,test="F")

a2 <- a4.2
#a4.1 <- update(a2,~.-row)
#a4.2 <- update(a2,~.-pos)
a4.3 <- update(a2,~.-lfClass)
a4.4 <- update(a2,~.-aphids2)
#anova(a4.1,a2,test="F")
#anova(a4.2,a2,test="F")
anova(a4.3,a2,test="F")
anova(a4.4,a2,test="F")

a1 <- a4.3

null <-  glm(chewhole ~ 1,data=d4b,family=quasibinomial)
anova(null,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,test="F")

#best model:

#opt <- glm(chewhole ~ basalLfCt + aphids2 + pos + basalLfCt:pos + aphids2:pos, data=d4b,family=quasibinomial)
opt <- glm(chewhole ~ pos + lfClass + aphids2, data=d4b,family=quasibinomial)

anova(opt,test="F")
#visualize:
par(mfcol=c(2,2))
plot(opt)
par(mfcol=c(1,1))

median(d4b$row)
median(d4b$pos)
stem(d2b$basalLfCt)
table(d4b$lfClass)
#fitted values for mode leaf count in median location
newd <- data.frame(aphids2=c("0","1-10","11-80",">80"),pos=rep(948.33,4),row=rep(46,4),lfClass=rep(">5",4))
pred <- predict(opt,type="response", se.fit=T ,newdata=newd)
newd$fit <- pred$fit
newd$se <- pred$se.fit
newd
bp <- barplot(newd$fit,ylim=c(0,0.8),ylab="prop. of plants with fol. Damage")
axis(1,at=bp,labels=c("0","1-10","11-80",">80"))
segments(bp,newd$fit+newd$se,bp,newd$fit-newd$se)

###################
# 1996 exp. 2007
table(d5b$lfClass,d5b$aphids2)
a11 <-  glm(chewhole ~ row*lfClass*aphids2 + pos*lfClass*aphids2,data=d5b,family=quasibinomial)
summary(a11)
a11.1 <- update(a11, ~.-row:lfClass:aphids2 )
a11.2 <- update(a11, ~.-pos:lfClass:aphids2 )
anova(a11.1,a11,test="F")
anova(a11.2,a11,test="F")

a10 <- a11.2

a9 <-  glm(chewhole ~ row*lfClass + row*aphids2 + pos*lfClass + pos*aphids2 + lfClass:aphids2,data=d5b,family=quasibinomial)
summary(a9)
a9.1 <- update(a9,~.-row:lfClass)
a9.2 <- update(a9,~.-row:aphids2)
a9.3 <- update(a9,~.-pos:lfClass)
a9.4 <- update(a9,~.-pos:aphids2)
a9.5 <- update(a9,~.-lfClass:aphids2)
anova(a9.1,a9,test="F")
anova(a9.2,a9,test="F")
anova(a9.3,a9,test="F")
anova(a9.4,a9,test="F")
anova(a9.5,a9,test="F")

a8 <- a9.4
a9.1 <- update(a8,~.-row:lfClass)
a9.2 <- update(a8,~.-row:aphids2)
a9.3 <- update(a8,~.-pos:lfClass)
#a9.4 <- update(a8,~.-pos:aphids2)
a9.5 <- update(a8,~.-lfClass:aphids2)
anova(a9.1,a8,test="F")
anova(a9.2,a8,test="F")
anova(a9.3,a8,test="F")
#anova(a9.4,a8,test="F")
anova(a9.5,a8,test="F")

a7 <- a9.3
a9.1 <- update(a7,~.-row:lfClass)
a9.2 <- update(a7,~.-row:aphids2)
#a9.3 <- update(a7,~.-pos:lfClass)
#a9.4 <- update(a7,~.-pos:aphids2)
a9.5 <- update(a7,~.-lfClass:aphids2)
anova(a9.1,a7,test="F")
anova(a9.2,a7,test="F")
#anova(a9.3,a7,test="F")
#anova(a9.4,a7,test="F")
anova(a9.5,a7,test="F")

a6 <- a9.5
a9.1 <- update(a6,~.-row:lfClass)
a9.2 <- update(a6,~.-row:aphids2)
#a9.3 <- update(a6,~.-pos:lfClass)
#a9.4 <- update(a6,~.-pos:aphids2)
#a9.5 <- update(a6,~.-lfClass:aphids2)
anova(a9.1,a6,test="F")
anova(a9.2,a6,test="F")
#anova(a9.3,a6,test="F")
#anova(a9.4,a6,test="F")
#anova(a9.5,a6,test="F")

a5 <- a9.1
a4 <- glm(chewhole ~ row + pos + lfClass + aphids2,data=d5b,family=quasibinomial)
a4.1 <- update(a4,~.-row)
a4.2 <- update(a4,~.-pos)
a4.3 <- update(a4,~.-lfClass)
a4.4 <- update(a4,~.-aphids2)
anova(a4.1,a4,test="F")
anova(a4.2,a4,test="F")
anova(a4.3,a4,test="F")
anova(a4.4,a4,test="F")

a3 <- a4.1
#a4.1 <- update(a3,~.-row)
a4.2 <- update(a3,~.-pos)
a4.3 <- update(a3,~.-lfClass)
a4.4 <- update(a3,~.-aphids2)
#anova(a4.1,a3,test="F")
anova(a4.2,a3,test="F")
anova(a4.3,a3,test="F")
anova(a4.4,a3,test="F")

a2 <- a4.4
#a4.1 <- update(a2,~.-row)
a4.2 <- update(a2,~.-pos)
a4.3 <- update(a2,~.-lfClass)
#a4.4 <- update(a2,~.-aphids2)
#anova(a4.1,a2,test="F")
anova(a4.2,a2,test="F")
anova(a4.3,a2,test="F")
#anova(a4.4,a2,test="F")

a1 <- a4.2

null <-  glm(chewhole ~ 1,data=d5b,family=quasibinomial)
anova(null,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,test="F")
anova(null,a1,a2,a3,a4,a5,a6,a7,a8,a9,test="F")

#best model (don't include 3-way interaction term)

#opt <- glm(chewhole ~ row + basalLfCt + aphids2 + pos + row:aphids2 + basalLfCt:aphids2, data=d5b,family=quasibinomial)
#best modl (with lfClass)
opt <- glm(chewhole ~ row + lfClass + aphids2 + pos + row:lfClass + row:aphids2 + lfClass:aphids2, data=d5b, family=quasibinomial)
anova(opt,test="F")
#visualize:
par(mfcol=c(2,2))
plot(opt)
par(mfcol=c(1,1))

median(d5b$row)
range(d5b$row)
range(d5b$pos)
median(d5b$pos)
table(d5b$basalLfCt)
table(d5b$lfClass)
stem(d5b$basalLfCt)
#fitted values for mode leaf count in median location
#newd <- data.frame(aphids2=c("0","1-10","11-80",">80"),pos=rep(948,4),row=rep(25,4),basalLfCt=rep(3,4))
newd <- data.frame(aphids2=c("0","1-10","11-80",">80"),pos=rep(948,4),row=rep(25,4),lfClass=rep("1-3",4))

pred <- predict(opt,type="response", se.fit=T ,newdata=newd)
newd$fit <- round(pred$fit,digits=4)
newd$se <- pred$se.fit
newd

bp <- barplot(newd$fit,ylim=c(0,0.8),ylab="prop. of plants with fol. Damage")
axis(1,at=bp,labels=c("0","1-10","11-80",">80"))
segments(bp,newd$fit+newd$se,bp,newd$fit-newd$se)


#summarize plant size for the different cohorts:
mean(d1b$basalLfCt) #1997 garden--2004
mean(d2b$basalLfCt) #1999 garden, 2004
mean(d3b$basalLfCt) # 1996 garden, 2005
mean(d4b$basalLfCt) # 1999 garden, 2005
mean(d5b$basalLfCt) # 1996 garden, 2007

#end section
#####################
#next step: use a glmm and combine 2004-2008 into a single model (1997 garden)

ss <- dt[dt$expNm==1997 & dt$Status=="basal",] #only non-flowering plants
table(ss$basalLfCt)
plot(sort(ss$basalLfCt))
ss[ss$basalLfCt>=1 & ss$basalLfCt <= 3,"lfClass"] <- "1-3"
ss[ss$basalLfCt>=4 & ss$basalLfCt <= 6,"lfClass"] <- "4-6"
ss[ss$basalLfCt>=7,"lfClass"] <- ">6"
ss$lfClass <- factor(ss$lfClass,levels=c("1-3","4-6",">6"))
table(ss$lfClass)

ss$measureYr <- factor(ss$measureYr)
ss[ss$measureYr==2004,"dateNum"] <- ss[ss$measureYr==2004,"measureDt2"]- as.Date("2004-01-01",format=("%Y-%m-%d"))
ss[ss$measureYr==2005,"dateNum"] <- ss[ss$measureYr==2005,"measureDt2"]- as.Date("2005-01-01",format=("%Y-%m-%d"))
ss[ss$measureYr==2006,"dateNum"] <- ss[ss$measureYr==2006,"measureDt2"]- as.Date("2006-01-01",format=("%Y-%m-%d"))
ss[ss$measureYr==2007,"dateNum"] <- ss[ss$measureYr==2007,"measureDt2"]- as.Date("2007-01-01",format=("%Y-%m-%d"))
ss[ss$measureYr==2008,"dateNum"] <- ss[ss$measureYr==2008,"measureDt2"]- as.Date("2008-01-01",format=("%Y-%m-%d"))

#ss$cgPlaId <- factor(ss$cgPlaId)
ss$measureDt2 <- factor(ss$measureDt2)
ss$row <- factor(ss$row)
ss$pos <- factor(ss$pos)

#Exclude dates with only a few measurements:
table(ss$measureDt2,ss$measureYr)
ss <- ss[!ss$measureDt2 %in% c("2005-08-29","2006-08-03","2006-08-05","2007-08-08","2008-07-23"),]
ss$measureDt2 <- factor(ss$measureDt2,levels=unique(ss$measureDt2))


#visualize change in chewhole by year
library(lattice)
windows()
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
library(lme4)


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

par(mfrow=c(2,2))
plot(ft.fix ~ aphids2, data=ss)
plot(ft.fix ~ measureYr, data=ss)
plot(ft.fix ~ lfClass, data=ss)
plot(ft.fix ~ row, data=ss)

ss[ss$ft.fix < -3,]

#newd <- data.frame(aphids2 = rep(c("0","1-10","11-80",">80"),each=2),row=median(as.numeric(as.character(ss$row))),measureYr="2004",lfClass="4-6",chewhole=rep(c(0,1),4))

ss$fit <- fitted(best)

plot(fit~aphids2,data=ss)

##check for normality of random effects
qqnorm(rr, pch = 19, las = 1, cex = 1)
abline(0,1,lwd = 1.5) #not sure what that means


head(sort(rr))

inv.logit <- function(x) exp(x)/(1+exp(x))

plot(inv.logit(ft.fix) ~ aphids2, data=ss)


#question: how do I present the fitted values as barplots, say, for a typical individual in a particular year?



#question: what's the best way to present the model results?
#how do I present the fitted values as barplots, say, for a typical individual in a particular year?

#qualities of typical individual?
newd <- data.frame(aphids2 = rep(c("0","1-10","11-80",">80"),each=2),row=median(as.numeric(as.character(ss$row))),measureYr="2005",lfClass="4-6",chewhole=rep(c(0,1),4))

