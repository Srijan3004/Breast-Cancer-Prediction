
##Data selection
data=read.csv(file.choose(),header=T)


##install.packages("nimble")
library(nimble)

##Randomize the rows of the data
set.seed(42)
rows=sample(nrow(data))
data1=data[rows,]

##Training and Testing
train=data1[1:469,]
test=data1[470:569,]

###
X_train = cbind(1,train$mean_radius,train$mean_texture,train$mean_perimeter,train$mean_area,train$mean_smoothness)
Y_train = train$diagnosis
K = ncol(X_train)
X_test = cbind(1,test$mean_radius,test$mean_texture,test$mean_perimeter,test$mean_area,test$mean_smoothness)
Y_test = test$diagnosis
##Train the model
#Logistic
model1=glm(train$diagnosis~train$mean_radius+train$mean_texture+train$mean_perimeter+train$mean_area+train$mean_smoothness,
           data=train,
           family=binomial
)
summary(model1)

#Probit
model2=glm(train$diagnosis~train$mean_radius+train$mean_texture+train$mean_perimeter+train$mean_area+train$mean_smoothness,
           data=train,
           family = binomial(link = "probit"))
summary(model2)

#t-distribution with 43 degrees of freedom
probit.nll <- function (beta) {
  # linear predictor
  eta <- X_train %*% beta
  # probability
  p <- pt(eta,43)
  # negative log-likelihood
  -sum((1 - Y_train) * log(1 - p) + Y_train * log(p))
}

# requires model matrix `X` and binary response `Y`
probit.gr <- function (beta) {
  # linear predictor
  eta <- X_train %*% beta
  # probability
  p <- pt(eta,43)
  # chain rule
  u <- dt(eta,43) * (Y_train - p) / (p * (1 - p))
  # gradient
  -crossprod(X_train, u)
}

vi = lm(train$diagnosis ~ train$mean_radius+train$mean_texture+train$mean_perimeter+train$mean_area+train$mean_smoothness)
v=c(3.485330,0.034005,-0.022130,-0.031211,0.000955,-6.906395)
fit1 <- optim(v, probit.nll, gr = probit.gr, method = "BFGS", hessian =  TRUE)
fit1

#double exponential
probit.nll1 <- function (beta) {
  # linear predictor
  eta <- X_train %*% beta
  # probability
  p <- pdexp(eta)
  # negative log-likelihood
  -sum((1 - Y_train) * log(1 - p) + Y_train * log(p))
}

# requires model matrix `X` and binary response `Y`
probit.gr1 <- function (beta) {
  # linear predictor
  eta <- X_train %*% beta
  # probability
  p <- pdexp(eta)
  # chain rule
  u <- ddexp(eta) * (Y_train - p) / (p * (1 - p))
  # gradient
  -crossprod(X_train, u)
}

vi = lm(train$diagnosis ~ train$mean_radius+train$mean_texture+train$mean_perimeter+train$mean_area+train$mean_smoothness)
v=c(3.485330,0.034005,-0.022130,-0.031211,0.000955,-6.906395)
fit2 <- optim(v, probit.nll1, gr = probit.gr1, method = "BFGS", hessian =  TRUE)
fit2

##Prediction
library(caret)
#install.packages("mltools")
library(mltools)
#Logistic
beta1=model1$coefficients
m1=X_test %*% beta1
val1=exp(m1)/(1+exp(m1))
pred1=c(1:100)*0
for(i in 1:100){
  if(val1[i]>0.18) pred1[i]=1  
  else pred1[i]=0}
confusionMatrix(as.factor(pred1),as.factor(test$diagnosis))
mcc1=mcc(pred1,test$diagnosis)

#Probit
beta2=model2$coefficients
m2=X_test %*% beta2
val2=pnorm(m2)
pred2=c(1:100)*0
for(i in 1:100){
  if(val2[i]>0.20) pred2[i]=1
  else pred2[i]=0}
confusionMatrix(as.factor(pred2),as.factor(test$diagnosis))
mcc2=mcc(pred2,test$diagnosis)

#t-distribution
beta3=fit1$par
m3=X_test %*% beta3
val3=pt(m3,43)
pred3=c(1:100)*0
for(i in 1:100){
  if(val3[i]>0.20) pred3[i]=1
  else pred3[i]=0}
confusionMatrix(as.factor(pred3),as.factor(test$diagnosis))
mcc3=mcc(pred3, test$diagnosis)

#Double Exponential
beta4=fit2$par
m4=X_test %*% beta4
val4=pdexp(m4)
pred4=c(1:100)*0
for(i in 1:100){
  if(val4[i]>0.17) pred4[i]=1
  else pred4[i]=0
}
confusionMatrix(as.factor(pred4),as.factor(test$diagnosis))
mcc4=mcc(pred4, test$diagnosis)


##Testing on Random testing sets

#Probit
mccp=c(100:569)*0
cutp=c(100:569)*0+0.15
sigm=c(0.15,0.16,0.17,0.18,0.19,0.20,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,0.30,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,0.40)
for(i in 100:569){
  test=data[sample(nrow(data),i),]
  X_test = cbind(1,test$mean_radius, test$mean_texture, test$mean_perimeter, test$mean_area, test$mean_smoothness)
  beta2 = model2$coefficients
  mp=X_test %*% beta2
  valp = pnorm(mp)
  pred = c(1:i)*0
  mccp[i-99]=0
  for (k in sigm){
    for(j in 1:i) {
      if( valp[j] > k) pred[j] = 1
      else pred[j] =0
    }
    mccp1[i-99]=mcc(pred, test$diagnosis)
    if(mccp1[i-99]>mccp[i-99]) {
      mccp[i-99]=mccp1[i-99];
      cutp[i-99]=k
      }
    else {mccp[i-99]=mccp[i-99]
          cutp[i-99]=cutp[i-99]}
  }}


#T-Distribution with df=43
mcct=c(100:569)*0
mcct1=c(100:569)*0
cutt=c(100:569)*0+0.15
sigm=c(0.15,0.16,0.17,0.18,0.19,0.20,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,0.30,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,0.40)
for(i in 100:569){
  test=data[sample(nrow(data),i),]
  X_test = cbind(1,test$mean_radius, test$mean_texture, test$mean_perimeter, test$mean_area, test$mean_smoothness)
  beta3 = fit1$par
  mt=X_test %*% beta3
  valt = pt(mt,43)
  predt = c(1:i)*0
  mcct[i-99]=0
  for (k in sigm){
    for(j in 1:i) {
      if( valt[j] > k) predt[j] = 1
      else predt[j] =0
    }
    mcct1[i-99]=mcc(predt, test$diagnosis)
    if(mcct1[i-99]>mcct[i-99]) {
      mcct[i-99]=mcct1[i-99];
      cutt[i-99]=k
    }
    else {mcct[i-99]=mcct[i-99]
    cutt[i-99]=cutt[i-99]}
  }}


###Final judgement
plot(density(mcct))
mean(mcct)
mean(mccp)
sum((mccp-mean(mccp))^2)
sum((mcct-mean(mcct))^2)
(0.8624161-0.8600604)/(0.4642769*sqrt(2/470))
qt(0.95,938)
mean(cutp)
mean(cutt)