############ AIC comparison ##############

setwd(getwd()) #specify working directory
df <- read_csv("data.csv", col_types = "fifffiidf") #Read data and set correct data types into the columns


#The following code fits every simplified version of the main model for each focal taxa and prints their Akaike information criterion.
#This is solely for ensuring that there is no unjustified complexity (independent variables that increase AIC) involved in the main models.

test <- filter(df, taxa == "E. nilssonii")
fit <- glmmTMB(formula = cbind(succ, fail) ~  period+year+lat+lat*period+(1|station/place) + (1|mic),
               data = test, family = binomial(link = "logit"))
print(1)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  period+year+lat+(1|station/place) + (1|mic),
               data = test, family = binomial(link = "logit")) 
print(2)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  period+lat+lat*period+(1|station/place) + (1|mic),
               data = test, family = binomial(link = "logit"))
print(3)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  period+lat+(1|station/place) + (1|mic),
               data = test, family = binomial(link = "logit"))
print(4)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  period+year+(1|station/place) + (1|mic),
               data = test, family = binomial(link = "logit"))
print(5)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  lat+year+(1|station/place) + (1|mic),
               data = test, family = binomial(link = "logit"))
print(6)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  period+(1|station/place) + (1|mic),
               data = test, family = binomial(link = "logit"))
print(7)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  lat+(1|station/place) + (1|mic),
               data = test, family = binomial(link = "logit"))
print(8)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  year+(1|station/place) + (1|mic),
               data = test, family = binomial(link = "logit"))
print(9)
summary(fit)$AIC



###
test <- filter(df, taxa == "Myotis sp.")
fit <- glmmTMB(formula = cbind(succ, fail)~  period+year+lat+lat*period+(1|station/place),
               data = test, family = binomial(link = "logit"))
print(1)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  period+year+lat+(1|station/place),
               data = test, family = binomial(link = "logit"))
print(2)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  period+lat+lat*period+(1|station/place),
               data = test, family = binomial(link = "logit"))
print(3)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  period+lat+(1|station/place),
               data = test, family = binomial(link = "logit"))
print(4)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  period+year+(1|station/place),
               data = test, family = binomial(link = "logit"))
print(5)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  lat+year+(1|station/place),
               data = test, family = binomial(link = "logit"))
print(6)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  period+(1|station/place),
               data = test, family = binomial(link = "logit"))
print(7)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  lat+(1|station/place),
               data = test, family = binomial(link = "logit"))
print(8)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  year+(1|station/place),
               data = test, family = binomial(link = "logit"))
print(9)
summary(fit)$AIC


###
test <- filter(df, taxa == "P. nathusii", station == "Huso" | station == "Tvarminne" | station == "Seili")
fit <- glmmTMB(formula = cbind(succ, fail)~  period+year+period*year+(1|station/place) + (1|mic),
               data = test, family = binomial(link = "logit"))
fit <- update(fit, control=glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)))
print(1)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  period+year+(1|station/place) + (1|mic),
               data = test, family = binomial(link = "logit"))
print(2)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  period+(1|station/place) + (1|mic),
               data = test, family = binomial(link = "logit"))
print(3)
summary(fit)$AIC
fit <- glmmTMB(formula = cbind(succ, fail)~  year+(1|station/place) + (1|mic),
               data = test, family = binomial(link = "logit"))
print(4)
summary(fit)$AIC