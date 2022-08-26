## ----setup, include=FALSE,echo=FALSE------------------------------------------------------------------------------------------
library(knitr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(reshape2)
library(RColorBrewer)
library(plotly)
library(mlr)
library("DataExplorer")
library(plyr)
library(arm)
library(ggpubr)
library(MASS)
library(car)
library(xtable)
library(sjstats)
library(leaps)
library(AER)
require(broom) # for tidy()
library(papeR)
library(agricolae)


## ----echo=FALSE,include=FALSE-------------------------------------------------------------------------------------------------
#reading data
car_data <-read.csv("CarPrice.csv")
#Data processing
#Removing rows with price=0
car_data<-subset(car_data, car_data$price!=0)
sum(is.na(car_data))


## ----echo=FALSE,include=FALSE-------------------------------------------------------------------------------------------------
# Seperate CarCompany from CarName
car_data$CarCompany<-str_split_fixed(car_data$CarName, " ", 2)[,1]

#cheking and correcting  misspelled values
unique(car_data$CarCompany)
car_data$CarCompany=str_replace(car_data$CarCompany,"alfa-romero","alfa-romeo")
car_data$CarCompany=str_replace(car_data$CarCompany,"porcshce","porsche")
car_data$CarCompany=str_replace(car_data$CarCompany,"maxda","mazda")
car_data$CarCompany=str_replace(car_data$CarCompany,"toyouta","toyota")
car_data$CarCompany=str_replace(car_data$CarCompany,"vokswagen","volkswagen")
car_data$CarCompany=str_replace(car_data$CarCompany,"vw","volkswagen")
car_data$CarCompany=str_replace(car_data$CarCompany,"Nissan","nissan")

unique(car_data$CarCompany)  #Levels: "alfa-romeo"  "audi" "bmw"  "chevrolet" "dodge" "honda" "isuzu" "jaguar" "mazda" "buick" "mercury" "mitsubishi" "nissan" "peugeot" "plymouth" "porsche" "renault" "saab" "subaru" "toyota" "volkswagen" "volvo" 
unique(car_data$fueltype) # Levels: gas and diesel
unique(car_data$aspiration) # Levels: std turbo
unique(car_data$doornumber) # Levels: four two
unique(car_data$carbody) #Levels: convertible hardtop hatchback sedan wagon
unique(car_data$drivewheel) #Levels: 4wd fwd rwd
unique(car_data$enginelocation) #Levels: front rear
unique(car_data$enginetype) # Levels: dohc dohcv l ohc ohcf ohcv rotor
unique(car_data$cylindernumber) #Levels: eight five four six three twelve two
unique(car_data$fuelsystem) #Levels: 1bbl 2bbl 4bbl idi mfi mpfi spdi spfi





#Removing unuseful covariates
unused <- c(grep("car_ID", colnames(car_data)),
            grep("CarName", colnames(car_data)))

car_data<-car_data[,-unused]




## ----echo=FALSE, message=FALSE, warning=FALSE,fig.height=3--------------------------------------------------------------------
#descriptive statistics of price
kable(as.data.frame(as.matrix(summary(car_data$price))), col.names = "Price" ,caption = " Descriptive statistics of price" )


## ----echo=FALSE, fig.cap="Car price boxplot (US dollar) ", fig.height=4,fig.align="center"------------------------------------
#price boxplot
boxplot(car_data$price,
main = " ",
col = "pink",
border = "brown",
horizontal = TRUE,
xlab = " ")


## ----echo=FALSE, message=FALSE, warning=FALSE, include=FALSE------------------------------------------------------------------
#aggregate(car_data[, 24], list(car_data$enginetype), median)


## ----echo=FALSE, message=FALSE, warning=FALSE,fig.height=3,fig.cap="Price by Engine type",fig.align="center"------------------
#plot violin with boxplot
p <- ggplot(car_data, aes(x=enginetype, y=log(price), fill = enginetype)) + geom_violin() 

# violin plot with median and quartile
p + geom_boxplot(width=0.2, fill="white") 


## ----echo=FALSE, message=FALSE, warning=FALSE, include=FALSE------------------------------------------------------------------
#aggregate(car_data[, 24], list(car_data$CarCompany), mean)


## ----echo=FALSE, message=FALSE, warning=FALSE,fig.cap="Car Company Prices",fig.align="center" ,fig.height=3-------------------
##plot categorical covariates

ggplot(car_data, aes(x=CarCompany, y=price)) + geom_bar( fill = "#56B4E9", stat = "summary", fun.y = "mean") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



## ----echo=FALSE, message=FALSE, warning=FALSE, include=FALSE------------------------------------------------------------------
#aggregate(car_data[, 24], list(car_data$carbody), max)
#aggregate(car_data[, 24], list(car_data$carbody, car_data$fueltype), max)


## ----echo=FALSE, message=FALSE, warning=FALSE,fig.cap="Prices for carbody per fuel type ",fig.align="center",  fig.height=2.7----
#plot carbody, fuel type and prices 
ggplot(car_data, aes(fueltype, price, fill = carbody)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2")


## ----echo=FALSE, message=FALSE, warning=FALSE,fig.cap="Interaction between horsepower and fuel type ",fig.align="center",  fig.height=4, fig.width=6----

#plot  that data for front engine location
plot(car_data$horsepower[car_data$fueltype =='gas'], log(car_data$price[car_data$fueltype=='gas']), col='blue', xlab = 'Horsepower', ylab = 'Price', main = '')

#add the points for rear engine location
points(car_data$horsepower[car_data$fueltype=='diesel'],log(car_data$price[car_data$fueltype=='diesel']), col='red', pch=16)
#add legend
legend(250,9,legend=c('Gas','Diesel'), col=c('blue','red'), pch=c(1,16), bty='n')
#add reg line for gas
abline(a=8.15, b= 0.011, col='blue', lwd=3)
#add reg line for diesel
abline(a=8.62, b= 0.011, col='red', lwd=3)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------
#mc <- lm(log(price)~horsepower+fueltype, data=car_data)
#summary(mc)


## ----echo=FALSE, message=FALSE, warning=FALSE, include=FALSE------------------------------------------------------------------

anova1 <- aov(log(car_data$price) ~ car_data$enginetype , data=car_data)
summary(anova1)


## ----echo=FALSE, message=FALSE, warning=FALSE, include=FALSE, fig.cap=" "-----------------------------------------------------
TukeyHSD(anova1, ordered = TRUE)


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.cap=" Differences in mean levels of enginetype"----------------------------
#plot to visualize the differences 
plot(TukeyHSD(anova1), las=1)


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------
# using numerical Variables
categ<- c(grep("fueltype", colnames(car_data)),
          grep("aspiration", colnames(car_data)),
          grep("doornumber", colnames(car_data)),
          grep("carbody", colnames(car_data)),
          grep("drivewheel", colnames(car_data)),
          grep("enginelocation", colnames(car_data)),
          grep("enginetype", colnames(car_data)),
          grep("cylindernumber", colnames(car_data)),
          grep("fuelsystem", colnames(car_data)),
          grep("symboling", colnames(car_data)),
          grep("CarCompany",colnames(car_data)))
          
car_data_num<-car_data[,-categ]



## ----pressure, echo=FALSE, fig.cap="A correlation heatmap for numerical categories", fig.align="center"-----------------------
#checking correlation between numerical covariates.
plot_correlation(car_data_num,)


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------
car_data_pred <- subset(car_data, select = -c( symboling, fueltype, aspiration, doornumber, carbody, drivewheel, enginelocation, enginetype, cylindernumber, fuelsystem, CarCompany, price))
#using Eigensystem analysis to check how each variable correlate with all the other variables not just pairwise

#condition number: ratio of max to min Eigen values of the correlation matric, we can use kappa(cor(cardata_pred)) function to have the condition number

#eigen(cor(car_data_pred))$values # return eigen values for each predictor

max_eigen_value <- max(eigen(cor(car_data_pred))$values)
min_eigen_value <- min(eigen(cor(car_data_pred))$values)

Condition_number <- max(eigen(cor(car_data_pred))$values) / min(eigen(cor(car_data_pred))$values)

eigen_values <- cbind(max_eigen_value, min_eigen_value, Condition_number)
kable(eigen_values, col.names = c('Max', 'Min', 'Condition number'),caption = "Eigensystem analysis")


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------


kable(vif(lm(price ~.,data=car_data_num)),col.names ="VIF", caption = "VIF for  numerical covariates")


## ----echo=FALSE,include=FALSE-------------------------------------------------------------------------------------------------
vif(lm(price ~.,data=car_data_num))
#lm(log(price) ~.,data=car_data_num)$coef


## ----echo=FALSE,include=FALSE-------------------------------------------------------------------------------------------------
#We removed citypmg
model1<-lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + 
             enginesize + boreratio + stroke + compressionratio+ horsepower + 
             peakrpm  + highwaympg, data = car_data_num)
vif(model1)


## ----echo=FALSE,include=FALSE-------------------------------------------------------------------------------------------------
# We removed curbweight 
model2<-lm(price ~ wheelbase + carlength + carwidth + carheight  + 
             enginesize + boreratio + stroke + compressionratio + horsepower + 
             peakrpm  + highwaympg,data = car_data_num)
vif(model2)



## ----echo=FALSE,include=FALSE-------------------------------------------------------------------------------------------------
model5<-lm(log(price) ~ wheelbase  + carlength+carwidth + carheight  + log(horsepower)+
             log(enginesize) + boreratio + stroke + log(compressionratio) + 
             peakrpm  + highwaympg+fueltype+carbody+enginetype,data = car_data)



## ----  echo=FALSE, include=FALSE----------------------------------------------------------------------------------------------
n<-length(car_data[,1])
step_model<-step(model5,k = log(n),trace = F)



## ----echo=FALSE, include=FALSE------------------------------------------------------------------------------------------------
##Train and test model based on model5

model8<-lm(log(price) ~ wheelbase  + carlength+carwidth + carheight  + log(horsepower)+
             log(enginesize) + boreratio + stroke + log(compressionratio) + 
             peakrpm  + highwaympg+fueltype+carbody+enginetype,x=T,data = car_data)
X <- model8$x 
X <- X[,-c(1)]


set.seed(123)
frac <- 0.8 
trainindeces <- sample(seq(1,dim(X)[1]),round(dim(X)[1]*frac))
xx <- as.matrix(X[trainindeces,])
yy <- log(car_data$price)[trainindeces]
xxt <- as.matrix(X[-trainindeces,])
yyt <- log(car_data$price)[-trainindeces]


## ----echo=FALSE,include=FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------------
m<-regsubsets(xx,yy,int=T,nbest=1000, nvmax=dim(X)[2],really.big = T,force.in=c(12:22))
cleaps<-summary(m,matrix=T)
tt<-apply(cleaps$which,1,sum) 
tmin<-min(tt)
tmax<-max(tt)
tsec<-seq(tmin,tmax)
cleaps$which


## ----echo=FALSE,include=FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------------
col.order <- c(colnames(cleaps$which))
xx <- xx[,col.order[-1]]   # now the order of columns in xx is consistent with the order in cleaps$which
xxt <- xxt[,col.order[-1]] # now the order of columns in xxt is consistent with the order in cleaps$which


mses<-cleaps$rss/length(yy) ## corresponding MSEs


msevec<-rep(0,length(tsec))
for (tk in 1:length(tsec)) {
  msevec[tk]<-min(mses[tt==tsec[tk]])} ## the best model for each size


## ---- fig.width=10,fig.height=10, echo=FALSE, message=FALSE, warning=FALSE,fig.cap=" MSE and pMSE curves ",fig.height=8-------
pmses<-rep(0,dim(cleaps$which)[1])
for (jj in (1:dim(cleaps$which)[1])) {
  # here we fit training data
  mmr<-lm(yy~xx[,cleaps$which[jj,-1]==T])
  # (i) obtain the design matrix for the given model
  # by selecting "active variables" in TEST data
  design <- xxt[,cleaps$which[jj,-1]==T]
  # (ii) add a column of ones for the intercept
  design <- cbind(rep(1,dim(xxt)[1]),design)
  PEcp<-sum((yyt-design%*%mmr$coef)^2)/length(yyt)
  # the above is the pMSE for the current model.
  # let's store it in a vector
  pmses[jj]<-PEcp
}
pmsevec<-rep(0,length(tsec))
for (tk in 1:length(tsec)) {
  pmsevec[tk]<-min(pmses[tt==tsec[tk]])}

par(mfrow=c(2,1))
plot(tsec,msevec,xlab="number of parameters",ylab="MSE",main="MSE for best model of each size",type='b',col=4,lwd=2)
plot(tsec,pmsevec,xlab="number of parameters",ylab="pMSE",main="prediction MSE for best model of each size", type="b",lwd=2,col=2)



## ----echo=FALSE, include=FALSE------------------------------------------------------------------------------------------------
ptmin1<-sort(pmses)[2] # with 16 variables
min1<-match(c(ptmin1),pmses)
pmses[min1]
cleaps$which[min1,]

model_pmse<-lm(log(price) ~ fueltype + carbody + enginetype + log(horsepower)+log(enginesize) +boreratio+stroke  ,x=T,data = car_data)


## ----echo=FALSE, include=FALSE, warning=FALSE---------------------------------------------------------------------------------


model_pmse<-lm(log(price) ~ fueltype + carbody + enginetype + log(horsepower)+log(enginesize) +boreratio+stroke  ,x=T,data = car_data) 
X1<-model_pmse$x
X1 <- X1[,-c(1)]
n <- 205
yy_cv <- log(car_data$price)
xx_cv <- as.matrix(X1)


## ----message=FALSE,warning=FALSE,echo=FALSE, include=FALSE--------------------------------------------------------------------
rleaps <- regsubsets(xx_cv, yy_cv, int = T, nbest = 4, nvmax = dim(X1)[2], really.big = T, method=c("ex"),force.in=c(1:12)) ## all subset models
CVcleaps <- summary(rleaps, matrix = T) 
Models = CVcleaps$which

K <- 10
ii <- sample(seq(1, length(yy_cv)), length(yy_cv))
foldsize <- floor(length(yy_cv) / K)
sizefold <- rep(foldsize, K)
restdata <- length(yy_cv) - K*foldsize
if (restdata > 0) {
  sizefold[1:restdata] <- sizefold[1:restdata] + 1}



Prederrors <- matrix(0,dim(Models)[1],K)
iused <- 0
Xmat <- cbind(rep(1,n), xx_cv)
for (k in (1:K)) {
  itest <- ii[(iused + 1):(iused + sizefold[k])]
  itrain <- ii[-c((iused + 1):(iused + sizefold[k]))]
  iused <- iused + length(itest)
  Prederrors[1,k] <- sum((yy_cv[itest] - mean(yy_cv[itrain]))^2)
  for (mm in (2:dim(Models)[1])) {
    xi <- Xmat[itrain, Models[mm,]]
    xi <- xi[,-1]
    yi <- yy_cv[itrain]
    lin.mod <- lm(yi ~ xi)
    xi <- Xmat[itest, Models[mm,]]
    xi <- xi[,-1]
    yhat <- predict(lin.mod, as.data.frame(xi))
    Prederrors[mm,k] <- sum((yy_cv[itest] - yhat)^2)    
  }
}
PE <- apply(Prederrors,1,sum)/n  # final prediction errors



## ----echo=FALSE, include=FALSE------------------------------------------------------------------------------------------------
winmod <- Models[which.min(PE),]
print(winmod)
min(PE) #same model with PMSE
model_cv<-lm(log(price) ~ fueltype + carbody + enginetype + log(horsepower)+log(enginesize) +boreratio+stroke  ,x=T,data = car_data)



## ----echo=FALSE, include=FALSE------------------------------------------------------------------------------------------------

PE_LOOCV<-rep(0,dim(Models)[1])
PEwrong<-rep(0,dim(Models)[1])
for (mm in (1:dim(Models)[1])) {
     modfit<-lm(yy_cv~Xmat[,Models[mm,]]-1)
     PE_LOOCV[mm] <- (sum(resid(modfit)^2/(1-hatvalues(modfit))^2))/length(yy_cv)}


## ----echo=FALSE, include=FALSE------------------------------------------------------------------------------------------------
winmod <- Models[which.min(PE_LOOCV),]
print(winmod)

## ----echo=FALSE, include=FALSE------------------------------------------------------------------------------------------------
model_loocv<-lm(log(price) ~ fueltype + carbody + enginetype + log(horsepower)+log(enginesize)  ,x=T,data = car_data)



## ----echo=FALSE---------------------------------------------------------------------------------------------------------------
kable(anova(model_loocv, model_pmse), caption = "Comparing loocv_model and pmse_model  " )


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------
kable(anova( model_pmse,step_model),caption = "Comparing step_model and pmse_model" )



## ----echo=FALSE, include=FALSE------------------------------------------------------------------------------------------------
#Our Model

model<-lm( log(price) ~ carwidth + log(horsepower) + log(enginesize) + 
    boreratio + stroke + fueltype + carbody + enginetype, data = car_data) 
summary(model)


## ----warning=FALSE,echo=FALSE , fig.cap=" Diagnostic plots for final model" ,fig.height=5-------------------------------------
#car_data <- car_data[-c(129,17,31,50,102,103,130,19,104,135,67,128,127), ]
par(mfrow=c(2,2))
plot(model)


## ----warning=FALSE, echo=FALSE, fig.cap="Cook's distance for chosen model",fig.height=4---------------------------------------
# cook's distance for chosen model

cds <- cooks.distance(model)

plot(model, pch=18, col='red', which=c(4),sub="")



## ----warning=FALSE, echo=FALSE, include=FALSE---------------------------------------------------------------------------------

#top Cooks distance
cd <- which(rownames(car_data) %in% names(sort(cooks.distance(model), decreasing = T)[1:10]))
cd

## ----warning=FALSE, echo=FALSE, include=FALSE---------------------------------------------------------------------------------
resd <- which(rownames(car_data) %in% names(sort(abs(residuals(model)), decreasing = T)[1:10]))
resd

## ----warning=FALSE, echo=FALSE, include=FALSE---------------------------------------------------------------------------------
inf <- which(rownames(car_data) %in% names(sort(lm.influence(model, do.coef = FALSE)$hat, decreasing = T)[1:10]))
inf

## ----warning=FALSE, echo=FALSE, include=FALSE---------------------------------------------------------------------------------
srsd <- which(rownames(car_data) %in% names(sort(abs(rstandard(model)), decreasing = T)[1:10]))
srsd


## ----warning=FALSE, echo=FALSE, include=FALSE---------------------------------------------------------------------------------
#gather all points from above
indvec <- c(resd, inf, cd, srsd)
#count the frequesncy of each of these points
table(indvec)

## ----warning=FALSE, echo=FALSE, include=FALSE---------------------------------------------------------------------------------
#pick out most frequency observations: 
indout1 <- table(indvec)[which(table(indvec) == 3)]
indout2 <- table(indvec)[which(table(indvec) == 2)]
indout <- c(indout1, indout2)
indout


## ----warning=FALSE, echo=FALSE, include=FALSE---------------------------------------------------------------------------------
#Removing most extreme outliers
car_data <- car_data[-c(19 , 50 , 67, 128 ,168 ,169,135 ), ]



## ----warning=FALSE, echo=FALSE, include=FALSE---------------------------------------------------------------------------------
model<-lm( log(price) ~ carwidth + log(horsepower) + log(enginesize) + 
    boreratio + stroke + fueltype + carbody + enginetype, data = car_data) 


## ---- echo=FALSE, include=FALSE-----------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(model)


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------
kable(prettify(summary(model)), caption = " Summary for model parameters")


## ----echo=FALSE, fig.cap =" Observed prices vs predicted prices with confidence and prediction intervals" , fig.height=4------
# plot y vs yhat
#fit all data with final model
yhat <- predict(model, newdata = car_data)
#Confidence and prediction intervals
conf <- predict(model, newdata = car_data, interval = 'confidence')
pred <- predict(model, newdata = car_data, interval = 'prediction')
conf <- as.data.frame(conf)
pred <- as.data.frame(pred)


# Regression line and confidence intervals
colors <- c("Confidence interval" = "forestgreen", "Prediction interval" = "red")
q <- ggplot(car_data, aes(log(yhat), log(car_data$price))) +
   stat_smooth(method = lm, col='skyblue') + geom_point(col='black') 
# Add prediction and confidence intervals
q + geom_line(aes(y = pred$lwr, color = "Prediction interval") ,linetype = "twodash")+
    geom_line(aes(y = pred$upr ,color = "Prediction interval"), linetype = "twodash") + geom_line(aes(y = conf$lwr, color = "Confidence interval"), linetype = "longdash")+
    geom_line(aes(y = conf$upr, color = "Confidence interval"), linetype = "longdash") +
   theme(plot.caption = element_text(hjust = 0))+
  scale_color_manual(values = colors)
  
  



## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------
summ_model <- data.frame( "Residual standard error" = sigma(model), "R-squared" = 0.925,"Adjusted R-squared" =0.9183 )
kable(summ_model, caption="Summary for model")


## ----include=FALSE,echo=FALSE-------------------------------------------------------------------------------------------------
nile_data<-read.csv("nile.csv")
nile_data<-nile_data[3:10]
nile_data$equine_rate<-nile_data$equine_cases/nile_data$farms
na.omit(nile_data)


## ---- include=FALSE,echo=FALSE------------------------------------------------------------------------------------------------
plot_correlation(nile_data)


## ----echo=FALSE, message=FALSE, warning=FALSE,include=FALSE-------------------------------------------------------------------
#check types of data
str(nile_data)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------
g0 <- glm(equine_cases ~ bird_cases, family="poisson", x=T ,offset = log(farms), data=nile_data)

kable(prettify(summary(g0)), caption = "Summary for model parameters")


## ----echo=FALSE, include=FALSE------------------------------------------------------------------------------------------------
dispersiontest(g0,alternative="two.sided")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------
#update the model by adding human density
g1 <- glm(equine_cases ~ bird_cases + human_dens, family="poisson",x=T, offset = log(farms),data = nile_data)
kable(prettify(summary(g1)),caption = "Summary for model parameters" )



## ---- echo=FALSE--------------------------------------------------------------------------------------------------------------
kable(anova(g0,g1), caption = "Anova test ")


## ----echo=FALSE, include=FALSE------------------------------------------------------------------------------------------------
#a chi-squared with 1 df
qchisq(0.95,1)


## ----echo=FALSE, include=FALSE------------------------------------------------------------------------------------------------
#formal test for dispersion
dispersiontest(g1,alternative="two.sided")


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.cap="Observed and Predicted rates (Poisson model)"-------------------------
# plot WVN rate vs predicted rate
# coefficients when using the offset term
beta <- g0$coefficients 
# extract the design matrix from g0,"x=T" in glm
dm <- g0$x   
pred_rate=exp(dm%*%beta) # predicted rates
plot(nile_data$equine_rate,pred_rate,ylab="Estimated rates",xlab="Observed rates")
abline(0,1, col='red') 


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------

#using negative binomial

ng <- glm.nb(equine_cases ~ bird_cases + offset(log(farms)), data=nile_data, x=T)
kable(prettify(summary(ng)),caption = "Summary for Negative Binomial parameters ")


## ----include=FALSE,echo=FALSE-------------------------------------------------------------------------------------------------
#p value given chi squared and degrees of freedom
pchisq(ng$deviance, ng$df.residual, lower.tail=F)

## ----include=FALSE,echo=FALSE-------------------------------------------------------------------------------------------------
c("1-P(X2>Dev)=", 1-pchisq(ng$dev, df=summary(ng)$df[2]))


## ----echo=FALSE, message=FALSE, warning=FALSE ,fig.cap="Observed and Predicted rates (Negative Binomial model)"---------------
# plot WVN rate vs predicted rate
# coefficients when using the offset term
beta1 <- ng$coefficients 
# extract the design matrix from gg,"x=T" in glm
dm1 <- ng$x   
pred_rate1=exp(dm1%*%beta1) # predicted rates
plot(nile_data$equine_rate,pred_rate1,ylab="Estimated rates",xlab="Observed rates", main ="Observed and Predicted rates (Negative Binomial model)")
abline(0,1, col='red') 


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.cap="Confidence interval for Poisson and Negative Binomial models "--------
nile_data$pred_NB <- pred_rate1
nile_data$pred_poisson <- pred_rate

# Regression line and confidence intervals

q<-ggplot(nile_data, aes(equine_rate, pred_NB)) +
   stat_smooth(method = glm, col='skyblue') + geom_point(col='forestgreen')
p<-ggplot(nile_data, aes(equine_rate, pred_poisson)) +
   stat_smooth(method = glm, col='skyblue') + geom_point(col='forestgreen')
ggarrange(q, p, ncol = 2, nrow = 1)


## ----include=FALSE,echo=FALSE-------------------------------------------------------------------------------------------------
qchisq(1-0.05,1)


## ----include=FALSE,echo=FALSE-------------------------------------------------------------------------------------------------
-2*(logLik(g0)[1]-logLik(ng)[1])


## ----echo=FALSE, message=FALSE, warning=FALSE,include=FALSE-------------------------------------------------------------------
library(lmtest)
lrtest(g1,ng)

