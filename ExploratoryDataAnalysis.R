load("MovieData.RData")

#Understand the spread of the quantitative variables
 hist(superhero$Budget)
 hist(superhero$Tomato)
 hist(superhero$IMDb)

#Look at counts to see how unbalanced the data is 
table(superhero$Comic)
table(superhero$Studio)


t.test(IMDb~Comic, data=superhero)

#assess the linearity of relationships
plot(superhero$IMDb~superhero$Tomato)
plot(superhero$IMDb~superhero$Budget)

#Explore taking the square root to account for the upper bound of fan score
superhero$BudSqrt<-sqrt(superhero$Budget)
plot(superhero$IMDb~superhero$BudSqrt)


plot(superhero$IMDb~superhero$Studio)
plot(superhero$IMDb~superhero$Comic)

cor(superhero$Budget, superhero$Tomato)
cor(superhero$Budget, superhero$IMDb)


#Look at cell means counts of studio vs comic
table(superhero$Comic, superhero$Studio)




#Compute the square root of budget to linearize the relationship
superhero$sqrtBudget<-sqrt(superhero$Budget)

mod<-lm(IMDb~+Comic+Tomato+sqrtBudget+Studio+Comic:sqrtBudget, data=superhero)
X<-model.matrix(mod)

require(car)
pdf(file="AVPlot.pdf")
avPlots(mod, terms=~Comic:BudSqrt, marginal.scale=F, ylab="Fan | Others", xlab="Marvel:Sqrt(Budget) | Others", main=" Plot for Franchise")
dev.off()
summary(mod)
plot(mod$residuals)
hist(mod$residuals)
qqnorm(mod$residuals)
abline(a=0,b=1)
plot(cooks.distance(mod))
plot(lm.influence(mod)$hat)


superhero$sqrtBudget<-sqrt(superhero$Budget)

require(xtable)
xtable(summary(data.frame(Tomato=superhero$Tomato)))
xtable(table(superhero$Comic))
xtable(table(superhero$Studio)/nrow(superhero))
table(superhero$Studio)
xtable(aggregate(cbind(Budget, Tomato,IMDb)~Comic, data=superhero, FUN = mean))

require(ggplot2)
pdf(file="Budget.pdf")
ggplot(superhero, aes(x=Budget, y=IMDb, shape=Comic, color=Comic))+geom_point()+ylab("Fan Rating")+
  theme(axis.text.x=element_text(size=16, angle=0, vjust=0.3), axis.title.x = element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text.y=element_text(size=16),
        plot.title=element_text(size=16))

dev.off()
ggplot(superhero, aes(x=sqrtBudget, y=IMDb, shape=Comic, color=Comic))+geom_point()
  

#For a fixed studio (Warner Brothers) and effect size for tomato (mean(Tomato)), this is the effect of Budget.     
ggplot(superhero, aes(color=Comic))+geom_point(aes(x=sqrtBudget, y=IMDb))+
  geom_abline(intercept = betahats[2]+mean(superhero$Tomato)*betahats[11], slope=betahats[10]+betahats[12], color="blue")+
  geom_abline(intercept=betahats[1]+mean(superhero$Tomato)*betahats[11] , slope=betahats[10], color="red")
