data=read.csv(file.choose(),header=T)

y=model2$fitted.values
pred=c(1:569)*0
for(i in 1:569){
  if(y[i]>0.13) pred[i]=1  #####0.13 is the best cutoff
  else pred[i]=0}
##install.packages('caret')
#library(caret)
confusionMatrix(as.factor(pred),as.factor(data$diagnosis))

model2=glm(data$diagnosis~data$mean_radius+data$mean_texture+data$mean_perimeter+data$mean_area+data$mean_smoothness,
           data=data,
           family=binomial
           )

modelp=glm(data$diagnosis~data$mean_radius+data$mean_texture+data$mean_perimeter+data$mean_area+data$mean_smoothness,
           data=data,
           family = binomial(link = "probit"))
y_p=modelp$fitted.values
pred_p=c(1:569)*0
for(i in 1:569){
  if(y_p[i]>0.15) pred_p[i]=1 ##optimum is 15
  else pred_p[i]=0}
confusionMatrix(as.factor(pred_p),as.factor(data$diagnosis))

model3=glm(data$diagnosis~data$mean_texture+data$mean_smoothness,
           data=data,
           family=binomial
)

model4=glm(data$diagnosis~data$mean_radius+data$mean_texture+data$mean_perimeter+data$mean_smoothness,
           data=data,
           family=binomial
) ###cutoff 0.15


####New

X = cbind(1,data1$mean_radius,data1$mean_texture,data1$mean_perimeter,data1$mean_area,data1$mean_smoothness)
Y = data1$diagnosis
K = ncol(X)

# requires model matrix `X` and binary response `Y`
probit.nll <- function (beta) {
  # linear predictor
  eta <- X %*% beta
  # probability
  p <- pdexp(eta)
  # negative log-likelihood
  -sum((1 - Y) * log(1 - p) + Y * log(p))
}

# requires model matrix `X` and binary response `Y`
probit.gr <- function (beta) {
  # linear predictor
  eta <- X %*% beta
  # probability
  p <- pdexp(eta)
  # chain rule
  u <- ddexp(eta) * (Y - p) / (p * (1 - p))
  # gradient
  -crossprod(X, u)
}

vi = lm(data1$diagnosis ~ data1$mean_radius+data1$mean_texture+data1$mean_perimeter+data1$mean_area+data1$mean_smoothness)
v=c(3.4931149,0.0131106,-0.0235992,-0.0266567,0.0008555,-7.3098598)
v=c(3.90325,-0.06844,-0.02553,-0.01870,0.00115,-9.09506)
fit <- optim(v, probit.nll, gr = probit.gr, method = "BFGS", hessian =  TRUE)


beta_pred=fit$par
Y=cbind(1,data2$mean_radius,data2$mean_texture,data2$mean_perimeter,data2$mean_area,data2$mean_smoothness)
a=Y%*%beta_pred
b=pdexp(a)


pred_t=c(1:100)*0
for(i in 1:100){
  if(b[i]>0.25) pred_t[i]=1 ##optimum is 
  else pred_t[i]=0}
confusionMatrix(as.factor(pred_t),as.factor(data2$diagnosis))

##install.packages("nimble")
#library(nimble)

#Randomize the rows of the data
random_row=sample(nrow(data))
random_row
data[random_row,]
