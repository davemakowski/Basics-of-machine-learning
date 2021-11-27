set.seed(3)
X=0:200
A=2
B=1
sigma=40
Y=A+B*X+rnorm(201,0,sigma)
Yp=A+B*210+rnorm(1,0,sigma)
plot(X,Y,xlim=c(0,220),ylim=c(0,Yp*1.1))
mod=lm(Y~X)
summary(mod)
Xp=c(0:200,210)
lines(Xp,predict(mod, newdata=data.frame(X=Xp)),lwd=2)
points(210,Yp,pch=19,col="red")

mod5=lm(Y~X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6)+I(X^7)+I(X^8)+I(X^9)+I(X^10)+
          I(X^11)+I(X^12)+I(X^13)+I(X14)+I(X^15)+I(X^16)+I(X^17))
summary(mod5)
lines(Xp,predict(mod5, newdata=data.frame(X=Xp)),col="blue",lwd=2)