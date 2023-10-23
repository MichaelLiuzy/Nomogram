#Install Packages  ##########
library(dplyr)
library(tableone)
library(nortest)
library(pROC)
library(glmnet)
library(caret)
library(tidyverse)
library(plyr)
library(epiDisplay)
library(gtsummary)
library(ggpubr)
library(rms)
library(lattice)
library(rmda)
library(MASS)
library(AICcmodavg)
#Import the Data   ########################
rm(list = ls())
oral_flap<- read.csv("C:/Users/Administrator/Desktop/oral_flap.csv",
                     header =  T)
##view data
head(oral_flap)
str(oral_flap)
names(oral_flap)

##clean data
oral_flap$main_outcome=as.double(oral_flap$main_outcome)
oral_flap<-subset(oral_flap, main_outcome < 2) #delete data with missing values

oral_flap<-oral_flap %>% mutate(
  Duration = if_else(!is.na(Duration),Duration,mean(Duration, na.rm = T))) 
  #fill in continuous data with the mean value

oral_flap<-na.omit(oral_flap) #delete the rest missing data

oral_flap<-oral_flap %>% mutate(oral_flap,bmi = Weight / (Height*0.01)^2)
  #add new variables

oral_flap$Age=as.double(oral_flap$Age)
oral_flap$Gender=as.factor(oral_flap$Gender) #set the right type of variables

#Table 1 (basic characteristics)##########
shapiro.test(oral_flap$Age) #normality test
myVars<-c("Age", "Duration", "Gender", "main_outcomes") #define variables
catVars<-c("Gender", "main_outcomes") #define category variables
table1<- CreateTableOne(vars = myVars,
                        factorVars = catVars,
                        data = oral_flap,
                        addOverall = T)

table_1<-print(table1,
               showALLLevels = T,
)
write.csv(table_1, file = "table1.csv")

#Univariable Logistic Regression  ##########
uni_glm_model<-
  function(x){
    FML<-as.formula(paste0("main_outcome ~",x))
    glm1<-glm(FML,data = oral_flap, family = binomial)
    glm2<-summary(glm1)
    OR<-round(exp(coef(glm1)),3)
    SE<-glm2$coefficients[,2]
    CI.5<-round(exp(coef(glm1)-1.96*SE),3)
    CI.95<-round(exp(coef(glm1)+1.96*SE),3)
    CI<-paste0(CI.5,'-',CI.95)
    p<-round(glm2$coefficients[,4],3)
    uni_glm_model<-data.frame('Characteristics'=x,
                              'OR'=OR,
                              'CI'=CI,
                              'p-value'=p
    )[-1,]
    return(uni_glm_model)
  } #construct a function to generate a list of univariate analysis results 
    #for dozens of factors at once

uni_glm<-lapply(colnames(oral_flap)[c(2,4:30,45)], uni_glm_model) #extract the colnames
uni_glm<-ldply(uni_glm,data.frame);uni_glm
summary(uni_glm) #view the results
write.csv(uni_glm,file = "uni_glm.csv") #export the results

#Multivariable Logistic Regression  ##########
mul_glm_1<-glm(main_outcome~Age+ASA+diabetes+Gender+Duration+BUN+
               operation_start_time+Handover+flap_type,
               data = oral_flap_1,
               family=binomial) # run the regression
summary(mul_glm_1)
coef_mul_1<-summary(mul_glm_1)$coef
mul_1_OR<-round(exp(coef_mul_1[,1]),3)
CI_Low_1<-round(exp(coef_mul_1[,1]-1.96*coef_mul_1[,2]),3)
CI_UP_1<-round(exp(coef_mul_1[,1]+1.96*coef_mul_1[,2]),3)
CI_1<-paste(CI_Low_1,'-',CI_UP_1)
mul_1_P<-round(coef_mul_1[,4],3)
mul_1_result<-data.frame(OR = mul_1_OR, CI = CI_1,
                         P = mul_1_P)
write.csv(mul_1_result,file = "mul_1.csv") #export the results

##Stepwise Analysis####
mul_glm_2 = step(mul_glm_1,trace = F, direction =  "both") #reduce the variables
summary(mul_glm_2)
coef_mul_2<-summary(mul_glm_2)$coef

mul_2_OR<-round(exp(coef_mul_2[,1]),3)
CI_Low_2<-round(exp(coef_mul_2[,1]-1.96*coef_mul_2[,2]),3)
CI_UP_2<-round(exp(coef_mul_2[,1]+1.96*coef_mul_2[,2]),3)
CI_2<-paste(CI_Low_2,'-',CI_UP_2)
mul_2_P<-round(coef_mul_2[,4],3)
mul_2_result<-data.frame(OR = mul_2_OR, CI = CI_2,
                         P = mul_2_P)
write.csv(mul_2_result,file = "mul_2.csv") #export the results


#Construct a Nomogram  #########
oral_flap$Gender<-as.numeric(oral_flap$Gender) #preparing data for the nomogram

nomo<-datadist(oral_flap)
options(datadist='nomo')

mul_lrm<- lrm(main_outcome~Age+Gender+Duration+BUN,
                data = oral_flap,
                x=T, y=T)

OR_lrm<-exp(mul_lrm$coefficients)

nomo <- nomogram(mul_lrm, 
                  fun= function(x)1/(1+exp(-x)),
                  fun.at=c(0.1,0.3,0.5,0.7,0.9),
                  lp=F, 
                  funlabel="Risk")
plot(nomo,
     xfrac = 0.5,
     cex.axis = 0.7,
     cex.var = 0.8,
     lmgp = 0.15,
     col.grid = gray(c(0.8,0.95)))

##ROC Curve#########
roc_glm<-glm(main_outcome~Age+Gender+Duration+BUN,
             data = oral_flap,
             family = binomial)

oral_flap$predvalue<-predict(roc_glm,type="response")
nomo_roc<-roc(oral_flap$main_outcome, oral_flap$predvalue, 
              data = oral_flap2)
auc(nomo_roc)
ci.auc(nomo_roc)

g.list<- ggroc(nomo_roc, alpha = 0.5, linetype = 1, size = 0.5,
                 linewidth = 0.7)
g.list+theme_bw()

##Calibration Curve############
cal<-calibrate(mul_lrm,method = "boot", B = 1000) #bootstrapping
plot(cal,
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Predicted Probability",
     ylab = "Observed Probability",
     cex.lab=1.2, cex.axis=1, cex.main=12, cex.sub=8,
     subtitles = F,
     legend= F)
lines(cal[,c("predy","calibrated.corrected")], 
      type = 'l', 
      lwd = 1.3, 
      pch = 12, 
      col = "#2166AC") 
lines(cal[,c("predy","calibrated.orig")],type="l",pch=12,lwd=1.3,col="tomato")
legend(0.5,0.5,
       c("Apparent","Bias-corrected","Ideal"), 
       lty = c(1,1,2), 
       lwd = c(1.3,1.3,1.3), 
       col = c("tomato","#2166AC","black"), 
       bty = "n"
)

##Decision Curve Analysis##########
oral_flap$main_outcome<-as.numeric(oral_flap$main_outcome)
mul_lrm_DCA<-decision_curve(main_outcome~Age+Gender+Duration+BUN,
                            data = oral_flap,
                            family = binomial (link = 'logit'),
                            thresholds = seq(0,1,by=0.01),
                            confidence.intervals = 0.95,
                            study.design = 'case-control',
                            population.prevalence = 0.13)
plot_decision_curve(mul_lrm_DCA,
                    curve.names = c('Nomogram'),
                    xlim = c(0,1.0),
                    cost.benefit.axis = F,
                    col = c('red'),
                    confidence.intervals = F,
                    standardize = F)




