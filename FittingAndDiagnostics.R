#FittingAndDiagnostics
require(ggplot2)
superhero$Studio<-relevel(superhero$Studio, "WB")
superhero$sqrtBudget<-sqrt(superhero$Budget)

mod<-lm(IMDb~-1+Comic+Studio+BudSqrt+Tomato+Comic:BudSqrt, data=superhero)
X<-model.matrix(mod)
summary(mod)

#Compute the hat matrix

superhero$cooksD<-cooks.distance(mod)
#Cook's distance rule of thumb 4/(n-p)
pdf(file="CooksD.pdf")
ggplot(superhero)+geom_point(aes(x=Budget, y=cooksD))+
  geom_text(data=superhero[superhero$cooksD>.1,],aes(x=Budget, y=cooksD, label=Title),
            nudge_x = .14, nudge_y = -.01) +ylab("Cook's Distance")
dev.off()

#Rule of thumb for leverage 2*p/n
superhero$leverage<-lm.influence(mod)$hat
pdf("Leverage.pdf")
ggplot(superhero)+geom_point(aes(x=Studio, y=leverage))+
  geom_text(data=superhero[superhero$leverage>.38 & superhero$Title!="Supergirl",],aes(x=Studio, y=leverage, label=Title),
             nudge_x = -.0, nudge_y = .015)+
    geom_text(data=superhero[superhero$leverage>.38 & superhero$Title=="Supergirl",],aes(x=Studio, y=leverage, label=Title),
              nudge_x = 0, nudge_y = -.015)+ylab("Leverage")+ggtitle("High Leverage Points by Studio")+
  theme(axis.text.x=element_text(size=16, angle=0, vjust=0.3), axis.title.x = element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text.y=element_text(size=16),
        plot.title=element_text(size=16))
dev.off()

#Look at our R studentized residuals
superhero$rstudentized<-rstudent(mod)
pdf("rstudent.pdf")
ggplot(superhero)+geom_histogram(aes(rstudentized), binwidth=.5) +ylab("Count")+xlab("R Studentized Residuals")+
  theme(axis.text.x=element_text(size=16, angle=0, vjust=0.3), axis.title.x = element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text.y=element_text(size=16),
        plot.title=element_text(size=16))
dev.off()

superhero[superhero$rstudentized< -3,]
pt(-3.356742, 50)*2

#Look at correlation between explanatory variables
cor(superhero$BudSqrt, superhero$Tomato)

