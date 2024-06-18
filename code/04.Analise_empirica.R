#Dummmy variable

dummy_var <- lm (mrate ~ legal + factor(year) + factor(state), data = dt)
summary(dummy_var)

#Within estimator
within <- plm (mrate ~ legal + factor(year), index = "state", 
               model="within", data = dt)
summary(dummy_var)
