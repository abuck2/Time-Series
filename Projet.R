
### Chargement des donnees et fonctions
source("/run/media/alexis/6917a5e2-e4da-439f-ada9-c2d93a4db183/alexis/stats/serieC/tp-3/FonctionsSeriesChrono.r")
source("/run/media/alexis/6917a5e2-e4da-439f-ada9-c2d93a4db183/alexis/stats/serieC/tp-3/tsdiag2.txt")
setwd("/run/media/alexis/6917a5e2-e4da-439f-ada9-c2d93a4db183/alexis/stats/serieC/")
library("fBasics")
library("forecast")
d<-ts(read.table("xsales.txt"), frequency = 12, start = 1965)

### Premiere presentation des donnees
par(mfrow=c(3,1))
plot(d, ylab="sales", main="Monthly sales of company an american company X. Jan 1965 - May 1971.")
acf(d, main="ACF", ylab="acf")
pacf(d, main="PACF", ylab="pacf")

#observation d'une tendance
par(mfrow=c(1,1))
plot(d, ylab="sales", main="Monthly sales of company an american company X. Jan 1965 - May 1971.")
t=seq(1965+1/12,by=1/12,length=length(d))
mod=lm(d~t)
abline(reg=mod,col="red")

#observation d'une saisonalite avec graphique
plot(d[1:12],type="l", ylim=c(min(d),max(d)))
for(i in 2:12){
  lines(d[12*(i-1)+(1:12)],col=i)
  legend("topleft",legend =1965:1971, fill= 1:12)
}

#observation d'une saisonalite avec un tableau
# Calcul des moyennes annuelles
moyenne=NULL
for (i in 1:6){
  moyenne=c(moyenne,mean(d[6*(i-1)+(1:12)]))
}
moyenne
# Calculs de Signe
sais=rep(NA,length(d))
sais=ts(sais,start = 1965, frequency= 12)
for (y in 1:6){
  for (i in 1:12){
    sais[12*(y-1)+i]=ifelse(d[12*(y-1)+i]>moyenne[y],"+","-")
  }
}
sais

#observation de la variabilite
plot(diff(d),xlab="temps(mois)",ylab="(1-B)Xt")

x=matrix(d[1:72],ncol=12,byrow=TRUE)
y=sapply(1:6,function(i)
  diff(range(x[i,])))
plot(1965:1970,y,type="b", xlab="Annee",ylab="Amplitude", main = "Variation de l'amplitude au cours du temps")

x=matrix(log(d[1:72]),ncol=12,byrow=TRUE)
y=sapply(1:6,function(i)
  diff(range(x[i,])))
plot(1965:1970,y,type="b", xlab="Annee",ylab="Amplitude", main = "Variation de l'amplitude au cours du temps")

x=matrix(BoxCox(d[1:72], BoxCox.lambda(d)),ncol=12,byrow=TRUE)
y=sapply(1:6,function(i)
  diff(range(x[i,])))
plot(1965:1970,y,type="b", xlab="Annee",ylab="Amplitude", main = "Variation de l'amplitude au cours du temps")


#decomposition
plot(decompose(d, "multiplicative")) 

###Determination de l'ordre
#stabilisation de la serie en utilisant le log
d.s<-log(d)
plot(d.s)
d.sta<-diff(d.s, lag=1)

par(mfrow=c(1,1))
plot(d.sta, ylab="sales", main="Monthly sales of company an american company X. Jan 1965 - May 1971.")
t=seq(1965+1/12,by=1/12,length=length(d.sta))
mod=lm(d.sta~t)
abline(reg=mod,col="red")

d.sta<-diff(d.sta, lag=12)
par(mfrow=c(3,1))
plot(d.sta)
acf(d.sta, lag.max = 25)
pacf(d.sta, lag.max = 25)
#p.max=1, q.max=3, P.max=1, Q.max=1

#stabilisation de la serie en utilisant BoxCox
lambda<-BoxCox.lambda(d)
dc.s<-BoxCox(d, BoxCox.lambda(d))
plot(dc.s)
dc.sta<-diff(d.s, lag=1)

par(mfrow=c(1,1))
plot(dc.sta, ylab="sales", main="Monthly sales of company an american company X. Jan 1965 - May 1971.")
t=seq(1965+1/12,by=1/12,length=length(dc.sta))
mod=lm(dc.sta~t)
abline(reg=mod,col="red")

dc.sta<-diff(dc.sta, lag=12)
par(mfrow=c(3,1))
plot(dc.sta)
acf(dc.sta, lag.max = 25)
pacf(dc.sta, lag.max = 25)
#comparaison de modeles
#Le suivant fait la comparaison la plus probables
Comp.Sarima(d.s[12:77], d=1, saison = 12, D=1, p.max = 1, q.max = 2, P.max = 1, Q.max = 1)
Comp.Sarima(dc.s, d=1, saison = 12, D=1, p.max = 1, q.max = 2, P.max = 1, Q.max = 1)
Comp.Sarima(d.s, d=1, saison = 12, D=1, p.max = 1, q.max = 3, P.max = 1, Q.max = 1)

###Estimation du modele
mod = arima(d.s, order = c( 0,1, 3), seasonal=list(order= c(0 ,1,1 ), period =12))
mod2 = arima(d.s, order = c( 1,1, 0), seasonal=list(order= c(0 ,1,1 ), period =12))
mod3 = arima(d.s, order = c( 0,1, 3), seasonal=list(order= c(1 ,1,1 ), period =12))
mod4 = arima(d.s, order = c( 1,1, 3), seasonal=list(order= c(0 ,1,1 ), period =12))
mod5 = arima(d.s, order = c( 1,1, 3), seasonal=list(order= c(1 ,1,1 ), period =12))
mod6 = arima(d.s[12:77], order = c( 1,1, 0), seasonal=list(order= c(1 ,1,1 ), period =12))
mod7 = arima(d.s, order = c( 1,1, 1), seasonal=list(order= c(0 ,1,1 ), period =12))
modc = Arima(dc.s, order = c( 1,1,0), seasonal=list(order= c(0 ,1,1 ), period =12), lambda=lambda)
moda = arima(d.s, order = c( 1,1, 3), seasonal=list(order= c(1 ,0,0 ), period =12))

###Validation des modeles
tsdiag2(mod,gof.lag=floor(sqrt(length(d.s))))
tsdiag2(mod2,gof.lag=floor(sqrt(length(d.s))))
tsdiag2(mod3,gof.lag=floor(sqrt(length(d.s))))
tsdiag2(mod4,gof.lag=floor(sqrt(length(d.s))))
tsdiag2(mod5,gof.lag=floor(sqrt(length(d.s))))
tsdiag2(mod6,gof.lag=floor(sqrt(length(d.s))))
tsdiag2(mod7,gof.lag=floor(sqrt(length(d.s))))
tsdiag2(modc,gof.lag=floor(sqrt(length(dc.s))))
tsdiag2(moda,gof.lag=floor(sqrt(length(d.s))))

round(coef.p(mod$coef[1:4],diag(mod$var.coef)[1:4]),4)
round(coef.p(mod2$coef[1:2],diag(mod2$var.coef)[1:2]),4)
mean(mod2$residuals)
round(coef.p(mod3$coef[1:5],diag(mod3$var.coef)[1:5]),4)
round(coef.p(mod4$coef[1:5],diag(mod4$var.coef)[1:5]),4)
round(coef.p(mod5$coef[1:6],diag(mod5$var.coef)[1:6]),4)
round(coef.p(mod6$coef[1:3],diag(mod5$var.coef)[1:3]),4)
round(coef.p(mod7$coef[1:3],diag(mod5$var.coef)[1:3]),4)
round(coef.p(modc$coef[1:2],diag(modc$var.coef)[1:2]),4)
round(coef.p(moda$coef[1:5],diag(moda$var.coef)[1:5]),4)

par(mfrow=c(1,1))
qqnorm(mod$residuals)
qqnorm(mod2$residuals[12:77])
qqnorm(mod3$residuals)
qqnorm(mod4$residuals)
qqnorm(mod5$residuals)
qqnorm(mod6$residuals)
qqnorm(mod7$residuals)
qqnorm(modc$residuals)
qqnorm(moda$residuals)

OneAhead(d.s,  order = c( 0,1, 3), seasonal=list(order= c(0 ,1,1 ), period =12))[[3]]

par(mfrow=c(1,1))
round(coef.p(mod2$coef[1:2],diag(mod2$var.coef)[1:2]),4)
tsdiag2(mod2,gof.lag=floor(sqrt(length(d.s))))
qqnorm(mod2$residuals)
dagoTest(mod2$residuals)
jarque.bera.test(mod2$residuals)
shapiro.test(mod2$residuals)

#Difference dans la prediction pour les differents modeles
OneAhead(d.s,  order = c( 1,1, 0), seasonal=list(order= c(0 ,1,1 ), period =12))[[3]]  
OneAhead(d.s,  order = c( 0,1, 3), seasonal=list(order= c(0 ,1,1 ), period =12))[[3]]
OneAhead(dc.s,  order = c( 1,1, 0), seasonal=list(order= c(0 ,1,1 ), period =12))[[3]] 
OneAhead(d.s, order = c( 1,1, 3), seasonal=list(order= c(1 ,0,0 ), period =12))[[3]]

  
#Etude des residus au carre
rescar<-mod2$residuals*mod2$residuals
par(mfrow=c(3,1))
plot(rescar)
acf(rescar)
pacf(rescar)
Comp.Sarima(rescar, d=0, saison = 12, D=0, p.max = 1, q.max =1 , P.max = 1, Q.max = 1)

###Prediction
par(mfrow=c(1,1))
pred.ts =predict(mod2, n.ahead = 0.1*length(d))
pred.ts2 =predict(mod, n.ahead = 0.1*length(d))
ts.tot = ts(c(d, exp(pred.ts$pred)), start= 1965, frequency=12)
plot ( ts.tot, type = "l",lty = 1, col ="blue", ylim=c(0,3000))
lines (exp(pred.ts$pred + 1.96*pred.ts$se), lty = 2, col = "blue")
lines (exp(pred.ts$pred - 1.96*pred.ts$se), lty = 2, col = "blue")
lines (exp(pred.ts2$pred + 1.96*pred.ts2$se), lty = 2, col = "green")
lines (exp(pred.ts2$pred - 1.96*pred.ts2$se), lty = 2, col = "green")
lines(exp(pred.ts2$pred), col="green")

#Holt-Winters
mod.HW.stab=HoltWinters(d.s)
pred.HW.stab=predict(mod.HW.stab,n.ahead=0.1*length(d),prediction.interval=TRUE)
pred.HW=exp(pred.HW.stab)
ts.tot = ts(c(d,pred.HW[,"fit"]), start= 1965, frequency= 12)
plot ( ts.tot, type = "l",lty = 1, col ="blue",ylim=c(min(d,pred.HW),max(d,pred.HW)))
lines (pred.HW[,"upr"], lty = 2, col = "blue")
lines (pred.HW[,"lwr"], lty = 2, col = "blue")

#comparaison
par(mfrow=c(1,1))
pred.ts =predict(mod2, n.ahead = 0.1*length(d))
ts.tot = ts(c(d, exp(pred.ts$pred)), start= 1965, frequency=12)
plot ( ts.tot, type = "l",lty = 1, col ="blue", ylim=c(0,2500))
lines (exp(pred.ts$pred + 1.96*pred.ts$se), lty = 2, col = "blue")
lines (exp(pred.ts$pred - 1.96*pred.ts$se), lty = 2, col = "blue")
mod.HW.stab=HoltWinters(d.s)
pred.HW.stab=predict(mod.HW.stab,n.ahead=0.1*length(d),prediction.interval=TRUE)
pred.HW=exp(pred.HW.stab)
ts.tot = ts(c(d,pred.HW[,"fit"]), start= 1965, frequency= 12)
lines ( ts.tot, type = "l",lty = 1, col ="red",ylim=c(min(d,pred.HW),max(d,pred.HW)))
lines (pred.HW[,"upr"], lty = 2, col = "red")
lines (pred.HW[,"lwr"], lty = 2, col = "red")

#Comparaison entre moda, mod2, et HW
par(mfrow=c(1,1))
pred.ts =predict(mod2, n.ahead = 0.1*length(d))
ts.tot = ts(c(d, exp(pred.ts$pred)), start= 1965, frequency=12)
plot ( ts.tot, type = "l",lty = 1, col ="blue", ylim=c(0,2500), main = "Comparaison entre predictions des modeles 110x011 (Bleu), 113x110 (Vert) et HW (Rouge)")
pred.HW.stab=predict(mod.HW.stab,n.ahead=0.1*length(d),prediction.interval=TRUE)
pred.HW=exp(pred.HW.stab)
ts.tot = ts(c(d,pred.HW[,"fit"]), start= 1965, frequency= 12)
lines ( ts.tot, type = "l",lty = 1, col ="red",ylim=c(min(d,pred.HW),max(d,pred.HW)))
par(mfrow=c(1,1))
pred2.ts =predict(moda, n.ahead = 0.1*length(d))
ts.tot2 = ts(c(d, exp(pred2.ts$pred)), start= 1965, frequency=12)
lines(ts.tot2, col="green")

#Entre moda, mod, mod2
plot(exp(predict(moda, n.ahead = 10)$pred ), ylim=c(300, 1200), col='blue', main="comparaison entre 113x110 (Bleu), et 110x011 et 013x011 (noirs)")
lines(exp(predict(mod2, n.ahead = 10)$pred))
lines(exp(predict(mod, n.ahead = 10)$pred))


