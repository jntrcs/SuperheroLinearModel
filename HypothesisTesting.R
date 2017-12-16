#Hypothesis testing and analysis

#Test if there is a didfferrence between Marvel and DC comics on Fan popularity

betahats<-coef(mod)
Y<- superhero$IMDb
X<-model.matrix(mod)
s2<-as.numeric(1/(64-12)*t(Y-X%*%betahats)%*%(Y-X%*%betahats))

#No effect for comic book franchise whatsoever (no change in slope, no change in intercept)
betahats<-as.matrix(betahats)
C.comic<-matrix(c(1,-1, rep(0, 10),
                  rep(0,11), 1), byrow = T, nrow = 2)
f.stat.comic<-t(C.comic%*%betahats)%*%solve(C.comic%*%solve(t(X)%*%X, t(C.comic)),C.comic%*%betahats)/(2*s2)
p.val.comic<-pf(f.stat.comic, 2, 52,lower.tail=F)

lines=data.frame(inter=c(betahats[2]+mean(superhero$Tomato)*betahats[11],betahats[1]+mean(superhero$Tomato)*betahats[11] ),
                 slope=c(betahats[10]+betahats[12],betahats[10]), comic=c("Marvel", "DC"))
pdf(file="ComicEffect.pdf")
ggplot(superhero)+geom_point(aes(x=BudSqrt, y=IMDb, color=Comic)) + 
  geom_abline(data=lines, aes(slope=slope, intercept=inter, color=comic))+ggtitle("Comic Effect at Mean Critic Rating")+
  theme(axis.text.x=element_text(size=16, angle=0, vjust=0.3), axis.title.x = element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text.y=element_text(size=16),
        plot.title=element_text(size=16))
dev.off()


#Try the same test with full and reduced models
red.mod<-lm(IMDb~Studio+sqrtBudget+Tomato, data=superhero)
anova(mod, red.mod)
#Same p-value! That's encouraging...

#Create confidence intervals for change in intercept between marvel and dc (with bonferroni correction)
a.vec.inter<-c(1,-1,rep(0, 10))
comic.int<-as.numeric(a.vec.inter%*%betahats)+c(1,-1)*qt(.0125, 52)*sqrt(s2*as.numeric(a.vec.inter%*%solve(t(X)%*%X, a.vec.inter)))
comic.int

#Create confidence intervals for change in slope between marvel and dc (with bonferroni correction)
a.vec.slope<-c(rep(0, 11), 1)
comic.slope<-as.numeric(a.vec.slope%*%betahats)+c(1,-1)*qt(.0125, 52)*sqrt(s2*as.numeric(a.vec.slope%*%solve(t(X)%*%X, a.vec.slope)))
comic.slope


##H0 No difference between any studios
#I will test this with full and reduced models
red.mod<-lm(IMDb~Comic+sqrtBudget+Tomato+Comic:sqrtBudget, data=superhero)
anova(mod, red.mod)
betatilde<-coef(red.mod)
X.red<-model.matrix(red.mod)
Nh<-t(Y-X.red%*%betatilde)%*%(Y-X.red%*%betatilde)-t(Y-X%*%betahats)%*%(Y-X%*%betahats)
f.stat<-Nh/(7*s2)
pf(f.stat, 7, 52, lower.tail=F)

#Check it with a c matrix
C.studio<-matrix(c(rep(0, 14), diag(7), rep(0,21)), byrow = F, nrow=7)
f.stat.comic<-t(C.studio%*%betahats)%*%solve(C.studio%*%solve(t(X)%*%X, t(C.studio)),C.studio%*%betahats)/(7*s2)
p.val.studio<-pf(f.stat.comic, 7, 52,lower.tail=F)

#Make a boxplot to demonstrate
pdf(file="StudioBoxplot.pdf")
ggplot(superhero, aes(x=Studio, y=IMDb, fill=Studio))+geom_boxplot()+guides(fill=F)+ylab("Fan Popularity")+
  theme(axis.text.x=element_text(size=16, angle=0, vjust=0.3), axis.title.x = element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text.y=element_text(size=16),
        plot.title=element_text(size=16))
dev.off()
#####Confidence interval for effect of budget on DC
a.vec<-c(rep(0, 9), 1, 0, 0)
dc.bud.int<-as.numeric(a.vec%*%betahats)+c(1,-1)*qt(.025, 52)*sqrt(s2*as.numeric(a.vec%*%solve(t(X)%*%X, a.vec)))

#Find change in expectation using the low end and high end of the interval
dc.bud.int/(2*sqrt(mean(superhero$Budget)))

#####Confidence interval for effect of budget on Marvel
a.vec.marvel<-c(rep(0, 9), 1, 0, 1)
mar.bud.int<-as.numeric(a.vec.marvel%*%betahats)+c(1,-1)*qt(.025, 52)*sqrt(s2*as.numeric(a.vec.marvel%*%solve(t(X)%*%X, a.vec.marvel)))
mar.bud.int
#Find change in expectation using the low end and high end of the interval
mar.bud.int/(2*sqrt(mean(superhero$Budget)))
