#running libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(dplyr)
library(lme4)
library(mlmRev)
library(mlogit)
library(textreg)
library(jtools)
library(effects)
library(lmerTest)
library(reghelper)

#create a column with ranking in characters to numbers
issuesmelted <-issuesmelted %>%
  mutate(ranking = ifelse(variable == 'MIP1', 1,
                          ifelse(variable == 'MIP2', 2,
                                 ifelse(variable == 'MIP3', 3,
                                        ifelse(variable == 'MIP4', 4,
                                               ifelse(variable == 'MIP5', 5, 0))))))



#MULTILEVEL MODEL FOR TESTING THE INFLUENCE OF CHANNELS IN SALIENCE
#MULTILEVEL LOGISTIC REGRESSION MODEL
#random intercept model
model1_1 <- glmer(counts ~ 1 + ( 1 | LoginID), data=issuesmelted,
                  family=binomial(link="logit"))
summary(model1_1)
summ(model1_1)
confint(model1_1) #confidence intervals

#adding level 1 predictors
model1_2 <- glmer(counts ~ group + (1 | LoginID), data=issuesmelted,
                  family=binomial(link="logit"))
summary(model1_2)
summ(model1_2, exp = T)
confint(model1_2) #confidence intervals
plot(allEffects(model1_2))

#adding level 2 predictors
model1_3 <- glmer(counts ~ group + Day + (1 | LoginID), data=issuesmelted,
                  family=binomial(link="logit"))
summary(model1_3)
summ(model1_3, exp = T)
confint(model1_3) #confidence intervals
plot(allEffects(model1_3))

#random slope
model1_4 <- glmer(counts ~ group + (Day | LoginID) + (1 | Day), data=issuesmelted,
                  family=binomial(link="logit"))
summary(model1_4)
summ(model1_4, exp = T)
confint(model1_4) #confidence intervals
plot(allEffects(model1_4))


#MULTILEVEL MODEL FOR TESTING THE INFLUENCE OF CHANNELS IN THE IMPORTANCE OF THE SALIENT ISSUES
#MULTILEVEL LINEAR REGRESSION MODEL
model2_1 <- lmer(ranking ~ 1 + ( 1 | LoginID), data=issuesmelted)
summary(model2_1)
summ(model2_1)
confint(model2_1) #confidence intervals

#adding level 1 predictors
model2_2 <- lmer(ranking ~ group + (1 | LoginID), data=issuesmelted)
summary(model2_2)
summ(model2_2, exp = T)
confint(model2_2) #confidence intervals
plot(allEffects(model2_2))

#adding random slope for importance
model2_2_int <- lmer(ranking ~ group + group*counts + (counts | group) + (1 | LoginID), data=issuesmelted)
summary(model2_2_int)
summ(model2_2_int, exp = T)
confint(model2_2_int) #confidence intervals
plot(allEffects(model2_2_int))
#probing interactions
simple_slopes(model2_2_int)
graph_model(model2_2_int, y=ranking, x=group, lines=counts)

#adding level 2 predictors
model2_3 <- glmer(ranking ~ counts + group + (1 | LoginID), data=issuesmelted)
summary(model2_3)
summ(model2_3, exp = T)
confint(model2_3) #confidence intervals
plot(allEffects(model2_3))

#adding level 3 predictors
model2_3 <- glmer(ranking ~ counts + group + day + (1 | LoginID), data=issuesmelted,
                  family=binomial(link="logit"))
summary(model2_3)
summ(model2_3, exp = T)
confint(model2_3) #confidence intervals
plot(allEffects(model2_3))
