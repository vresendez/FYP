#Running Libraries
####################################################################################
library(tidyverse)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(sjmisc)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(dplyr)
library(lme4) #for GLMER
library(mlmRev)
library(mlogit)
library(textreg)
library(jtools)
library(effects)
library(reghelper)
library(plot.matrix)
library(psych)
####################################################################################

#Create a column with ranking in characters to numbers
issuesmelted <-issuesmelted %>%
  mutate(ranking = ifelse(variable == 'MIP1', 1,
                          ifelse(variable == 'MIP2', 2,
                                 ifelse(variable == 'MIP3', 3,
                                        ifelse(variable == 'MIP4', 4,
                                               ifelse(variable == 'MIP5', 5, 0))))))
####################################################################################

#MULTILEVEL MODEL FOR TESTING THE INFLUENCE OF CHANNELS IN SALIENCE
#MULTILEVEL LOGISTIC REGRESSION MODEL
#random intercept model
model1_1 <- glmer(counts ~ 1 + ( 1 | LoginID), data=issuesmelted,
                  family=binomial(link="logit"))
summary(model1_1)
summ(model1_1)
confint(model1_1) #confidence intervals

#Adding level 1 predictors - Group as first predictor
model1_2 <- glmer(counts ~ group + (1 | LoginID), data=issuesmelted,
                  family=binomial(link="logit"))
summary(model1_2)
summ(model1_2, exp = T)
confint(model1_2) #confidence intervals
plot(allEffects(model1_2))

#Adding Level 2 - Day as another predictor 
model1_3 <- glmer(counts ~ group + Day + (1 | LoginID) , data=issuesmelted,
                  family=binomial(link="logit"))
summary(model1_3)
summ(model1_3, exp = T)
confint(model1_3) #confidence intervals
plot(allEffects(model1_3))

#Testing Day as random effect 
model1_3_r <- glmer(counts ~ group + (1 | LoginID) + (1|Day) , data=issuesmelted,
                  family=binomial(link="logit"))
summary(model1_3_r)
summ(model1_3_r, exp = T)
confint(model1_3_r) #confidence intervals
plot(allEffects(model1_3_r))


#calculate AIC for model1_3 & model1_3_r to compare if it perfoms better
AIC(logLik(model1_3))
AIC(logLik(model1_3_r))
#result "259.6697 AND 259.6923" show both are almost the same, 
#model model1_3_r is slightly performing worst when adding Day as a random effect
####################################################################################

# Visualize the Data
#TODO CREATE DATAFRAME FOR DATA VISUALS


####################################################################################

#MULTILEVEL MODEL FOR TESTING THE INFLUENCE OF 
#CHANNELS IN THE IMPORTANCE OF THE SALIENT ISSUES
#MULTILEVEL LINEAR REGRESSION MODEL
#ONLY TESTING RANDOM EFFECTS (PARTICIPANTS REPETEADED MEASURES)
model2_1 <- lmer(ranking ~ 1 + ( 1 | LoginID), data=issuesmelted)
summary(model2_1)
summ(model2_1)
confint(model2_1) #confidence intervals

#Adding level 1 predictors
model2_2 <- lmer(ranking ~ group + (1 | LoginID), data=issuesmelted)
summary(model2_2)
summ(model2_2, exp = T)
confint(model2_2) #confidence intervals
plot(allEffects(model2_2))

#Adding level 1 predictors and 2 random effects
model2_2_1<- lmer(ranking ~ group + (1 | LoginID) + (1 | Day), data=issuesmelted)
summary(model2_2_1)
summ(model2_2_1, exp = T)
confint(model2_2_1) #confidence intervals
plot(allEffects(model2_2_1, ))


#Adding level 2 predictors
model2_2_3 <- lmer(ranking ~ group + counts + (1 | LoginID) , data=issuesmelted)
summary(model2_2_3)
summ(model2_2_ran, exp = T)
confint(model2_2_3) #confidence intervals
plot(allEffects(model2_2_3))

#Adding random slope for importance the salience of issues might change per group
model2_3_r <- lmer(ranking ~ group + counts + (counts | group) + (1 | LoginID) + (1 | Day), data=issuesmelted)
summary(model2_3_r)
summ(model2_3_r, exp = T)
confint(model2_3_r) #confidence intervals
plot(allEffects(model2_3_r))


#Probing interactions
simple_slopes(model2_3_r)
graph_model(model2_3_r, y=ranking, x=group, lines=counts)
#calculate AIC for model2_2_3 & model2_3_r to compare if it perform better


#Adding random slope for importance the salience of issues might change per group and adding interaction effects and different predictors
model2_4_r <- lmer(ranking ~ group + counts + group*counts + (counts | group) + (1 | LoginID) + (1 | Day), data=issuesmelted)
summary(model2_4_r)
summ(model2_4_r, exp = T)
confint(model2_4_r) #confidence intervals
plot(allEffects(model2_4_r))



#Adding level 3 predictors
model2_3 <- glmer(ranking ~ counts + group + day + (1 | LoginID), data=issuesmelted,
                  family=binomial(link="logit"))
summary(model2_3)
summ(model2_3, exp = T)
confint(model2_3) #confidence intervals
plot(allEffects(model2_3))

#Adding level 3 predictors
model2_3 <- glmer(ranking ~ counts + group + day + (1 | LoginID), data=issuesmelted,
                  family=binomial(link="logit"))
summary(model2_3)
summ(model2_3, exp = T)
confint(model2_3) #confidence intervals
plot(allEffects(model2_3))

####################################################################
################ COMPARING MODEL PERFOMANCES ######################
# Second model performs a bit better

### MODELS FOR SALIENCE
AIC(logLik(model1_1))
AIC(logLik(model1_2))
AIC(logLik(model1_3))
AIC(logLik(model1_3_r)) # (Adding random slopes)

### MODELS FOR IMPORTANCE OF SALIENCE
AIC(logLik(model2_1))
AIC(logLik(model2_2))
AIC(logLik(model2_1))
AIC(logLik(model2_2_3))
AIC(logLik(model2_3_r))
AIC(logLik(model2_4_r))

####################################################################
################ PLOT MODELS #########################
p1<-plot_model(model1_3_r, show.values = TRUE, value.offset = .3)
p1
p2<-plot_model(model2_4_r, show.values = TRUE, value.offset = .3)
p2
################ TAB MODELS #########################
ModelsSalience <- tab_model(model1_3, model1_3_r)
ModelsImportance <- tab_model(model2_2_3, model2_4_r)
write.table(ModelsSalience, file = "ModelsSalience.txt", sep = ",", quote = FALSE, row.names = F)
write.table(ModelsImportance, file = "ModelsImportance.txt", sep = ",", quote = FALSE, row.names = F)

