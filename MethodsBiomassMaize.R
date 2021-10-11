#D. Makowski
#Biomass (g/m2) 

DataSet<-read.table("BiomassMais.txt", sep="\t", header=T)

#Model PCR
library(pls)
Mod_pcr<-pcr(B~T1+T2+T3+RAD1+RAD2+RAD3, data=DataSet, validation="CV", segments=17,segment.type = "consecutive", scale="TRUE")
summary(Mod_pcr)
par(mfrow=c(2,3))
plot(Mod_pcr, line=TRUE, ncomp=1)
plot(Mod_pcr, line=TRUE, ncomp=2)
plot(Mod_pcr, line=TRUE, ncomp=3)
plot(Mod_pcr, line=TRUE, ncomp=4)
plot(Mod_pcr, line=TRUE, ncomp=5)
plot(Mod_pcr, line=TRUE)
par(mfrow=c(1,1))
plot(RMSEP(Mod_pcr), legend ="topright")
plot(Mod_pcr, "loadings", comps=1:2, legendpos="topleft", ylim=c(-1,1))

#PLSR
Mod_pls<-plsr(B~T1+T2+T3+RAD1+RAD2+RAD3, data=DataSet, validation="CV", segments=17,segment.type = "consecutive", scale="TRUE")
summary(Mod_pls)
par(mfrow=c(2,3))
plot(Mod_pls, line=TRUE, ncomp=1)
plot(Mod_pls, line=TRUE, ncomp=2)
plot(Mod_pls, line=TRUE, ncomp=3)
plot(Mod_pls, line=TRUE, ncomp=4)
plot(Mod_pls, line=TRUE, ncomp=5)
plot(Mod_pls, line=TRUE)
par(mfrow=c(1,1))
plot(RMSEP(Mod_pls), legend ="topright")
plot(Mod_pls, "loadings", comps=1:2, legendpos="topleft", ylim=c(-1,1))

##GAM
library(mgcv)
Mod_gam<-gam(B~s(T1)+s(T2)+s(T3)+s(RAD1)+s(RAD2)+s(RAD3), data=DataSet)
summary(Mod_gam)
dev.new()
par(mfrow=c(2,3))
plot(Mod_gam)

RMSE_gam<-sqrt(mean((DataSet$B-predict(Mod_gam))^2))
RMSE_gam

#Cross-validation with GAM
B_pred_gam<-rep(NA,length(DataSet$B))

List_year<-unique(DataSet$Year)

for (i in 1:length(List_year)) 
{
Training_i<-DataSet[DataSet$Year!=List_year[i],]
Test_i<-DataSet[DataSet$Year==List_year[i],]	
Mod_i<-gam(B~s(T1)+s(T2)+s(T3)+s(RAD1)+s(RAD2)+s(RAD3), data=Training_i)
B_gam_i<-predict(Mod_i, newdata=Test_i)
B_pred_gam[DataSet$Year==List_year[i]]<-B_gam_i	
}

RMSEP_gam<-sqrt(mean((DataSet$B-B_pred_gam)^2))
RMSEP_gam

dev.new()
par(mfrow=c(1,2))
plot(predict(Mod_gam),DataSet$B, xlab="Predicted values", ylab="Data")
abline(0,1)
title("A.                                        ")
text(2400,3100,paste("RMSE= ", round(RMSE_gam, digits=2)))
plot(B_pred_gam,DataSet$B, xlab="Predicted values", ylab="Data")
abline(0,1)
title("B.                                        ")
text(2400,3100,paste("RMSEP= ", round(RMSEP_gam, digits=2)))

####LASSO
library(glmnet)
X=as.matrix(DataSet[,3:8])
Y=DataSet$B
model <- glmnet(X, Y, alpha = 1)
plot(model, xvar="lambda", label=TRUE)

coef(model, s=exp(4)) 
coef(model, s=exp(3.5))

cv <- cv.glmnet(X, Y, alpha=1, foldid=as.numeric(as.factor(DataSet$Year)))
plot(cv)

coef(cv, s="lambda.min")

####Quantile regression
library(quantreg)

plot(DataSet$T2,DataSet$B, xlab="T2", ylab="B", pch=20, xlim=c(16.5,23))

mod<-rq(B~T2+I(T2^2), data=DataSet, tau=c(0.05,0.1, 0.5, 0.9, 0.95))
summary(mod)
print(coef(mod))

Xvec=seq(16.5,23,by=0.1)
Para=coef(mod)[,1]
lines(Xvec,Para[1]+Para[2]*Xvec+Para[3]*Xvec^2, col="red", lwd=2)

Para=coef(mod)[,2]
lines(Xvec,Para[1]+Para[2]*Xvec+Para[3]*Xvec^2, col="blue", lwd=2)

Para=coef(mod)[,3]
lines(Xvec,Para[1]+Para[2]*Xvec+Para[3]*Xvec^2, col="orange", lwd=2)

Para=coef(mod)[,4]
lines(Xvec,Para[1]+Para[2]*Xvec+Para[3]*Xvec^2, col="blue", lwd=2)

Para=coef(mod)[,5]
lines(Xvec,Para[1]+Para[2]*Xvec+Para[3]*Xvec^2, col="red", lwd=2)

####Regression tree
library(rpart)
library(rpart.plot)

Mod_tree<-rpart(B~T1+T2+T3+RAD1+RAD2+RAD3,data=DataSet)
print(Mod_tree)
#dev.new()
par(mfrow=c(1,1))
rpart.plot(Mod_tree)
#text(Mod_tree)

###Random forest
library(randomForest)
Mod_RF<-randomForest(B~T1+T2+T3+RAD1+RAD2+RAD3,data=DataSet,ntree=500, mtry=6)
Mod_RF

plot(Mod_RF)
dev.new()
par(mfrow=c(1,1))
varImpPlot(Mod_RF,type=2)
RMSE_rf<-sqrt(mean((DataSet$B-predict(Mod_RF))^2))
RMSE_rf

#Cross-validation
B_pred_rf<-rep(NA,length(DataSet$B))

List_year<-unique(DataSet$Year)

for (i in 1:length(List_year)) 
{
Training_i<-DataSet[DataSet$Year!=List_year[i],]
Test_i<-DataSet[DataSet$Year==List_year[i],]	
Mod_i<-randomForest(B~T1+T2+T3+RAD1+RAD2+RAD3,data=Training_i, ntree=200)
B_rf_i<-predict(Mod_i, newdata=Test_i)
B_pred_rf[DataSet$Year==List_year[i]]<-B_rf_i	
}

RMSEP_rf<-sqrt(mean((DataSet$B-B_pred_rf)^2))
RMSEP_rf

dev.new()
par(mfrow=c(1,2))
plot(predict(Mod_RF),DataSet$B, xlab="Predicted values", ylab="Data")
abline(0,1)
title("A.                                        ")
text(2500,3100,paste("RMSE= ", round(RMSE_rf, digits=2)))
plot(B_pred_rf,DataSet$B, xlab="Predicted values", ylab="Data")
abline(0,1)
title("B.                                        ")
text(2600,3100,paste("RMSEP= ", round(RMSEP_rf, digits=2)))
