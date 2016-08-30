
require(nlme)
require(lme4)
#require(lmerTest)

rail <- data.frame(Rail)
rail$Rail <- as.factor(as.character(droplevels(rail$Rail)))


#   ---- Estimate models.  

#  i = subject          (rail)
#  j = repeated measure (trial)

m1 <- lm  (travel ~  1             ,data=rail)   # Simple intercept model.                                      Y_ij = mu       + e_ij  e_ij ~ N(0,sigma^2)
m2 <- lm  (travel ~  1 + Rail      ,data=rail)   # One-way fixed effects ANOVA (less-than-full-rank) model.     Y_ij = mu + b_i + e_ij  e_ij ~ N(0,sigma^2)   (Beta vector not unique.)
m3 <- lm  (travel ~ -1 + Rail      ,data=rail)   # One-way fixed effects ANOVA (full-rank) model.               Y_ij =      b_i + e_ij  e_ij ~ N(0,sigma^2)
m4 <- lmer(travel ~  1 + (1 | Rail),data=rail)   # One-way random effects ANOVA model.                          Y_ij = mu + b_i + e_ij  e_ij ~ N(0,sigma^2) [within variability], b_i ~ N(0,sigma_b^2) [between variability]
                                                 #  -- Recall that with elementary calculations involving covariace, we can formulate the intraclass correlation rho = sigma_b^2 / (sigma_b^2 + sigma^2)
                                                 #  -- Also recall that we don't "estimate" random effects;  rather we "predict" them.  
  
  

#   ---- Add on the residuals from each model to the rail data set. 
rail$m1residuals <- m1$residuals
rail$m2residuals <- m2$residuals
rail$m3residuals <- m3$residuals
rail$m4residuals <- residuals(m4)

#   ---- Look at the residual plots.
par(mfrow=c(4,1))
boxplot(m1residuals ~ Rail,data=rail)
boxplot(m2residuals ~ Rail,data=rail)
boxplot(m3residuals ~ Rail,data=rail)
boxplot(m4residuals ~ Rail,data=rail)
par(mfrow=c(1,1))

# Model m2 does not provide a unique Beta vector.  But XBeta is interpretable. 
# Model m3 does not provide an estimate of the between-rail variability.  But, it improves the residual error by a lot.  

# We should look at the plot of residuals e versus fitted values yHat.  Recall that we can show that these two quantities
# e = y - XBetaHat (a quadratic form) and yHat = XBetaHat (a linear form) are independent by showing that BVA = 0, where
# B = the matrix associated with the linear form, A = the matrix associated with the quadratic form, and V = var(y).  

# Also recall that we look at the plot of the residuals against the fitted value to assess the assumption of constant 
# variance across the values of the independent variable.  

par(mfrow=c(4,1))
plot(fitted.values(m1),residuals(m1))
plot(fitted.values(m2),residuals(m2))
plot(fitted.values(m3),residuals(m3))
plot(fitted.values(m4),residuals(m4))
par(mfrow=c(1,1))

# Generally, however, we use the standaradized residuals, so as to make sure they are all on an equal footing.  We use 
# the mean-square error to standardize, i.e., the square-root of the value sigma^2.  Note that we also get these 
# plots by simply calling plot with the fitted-model object, e.g., plot(m1).  
par(mfrow=c(4,1))
plot(fitted.values(m1),residuals(m1)/summary(m1)$sigma)
plot(fitted.values(m2),residuals(m2)/summary(m2)$sigma)
plot(fitted.values(m3),residuals(m3)/summary(m3)$sigma)
plot(fitted.values(m4),residuals(m4)/summary(m4)$sigma)
par(mfrow=c(1,1))


# We can form simple confidence intervals via use of the intervals function.
confint(m1)
confint(m2)
confint(m3)
confint(m4)

# We can get p-values of the fixed-effect terms via the anova function.  Note that this function doesn't include
# the intercept in the resulting table. 
anova(m1)
anova(m2)
anova(m3)
anova(m4)









ergo <- ergoStool

# i = subject
# j = treatment

m5 <- lmer(effort ~  1 + Type + (1 | Subject),data=ergo)  # Randomized-block Design   y_ij = mu + b_j + s_i + e_ij,  i = {1,...,9},  j = {1,...,4};  s_i ~ N(0,sigma_s^2),  e_ij ~ N(0,sigma^2)
                                                          #  -- Recall that the lack of an intercept here induces the so-called cell means model;  the design matrix for X is simply a 
                                                          #     diagonal matrix of ones.  The random matrix here is simply a column vector of ones.  
                                                          #  -- R, by default, uses treatment contrasts, which measure the difference between a particular group (or level of the b_j 
                                                          #     factor) 
m6 <- lmer(effort ~ -1 + Type + (1 | Subject),data=ergo)  # Cell-means Parametrizaion of the Randomized-block Design   y_ij = b_j + s_i + e_ij,  i = {1,...,9},  j = {1,...,4};  s_i ~ N(0,sigma_s^2),  e_ij ~ N(0,sigma^2)
                                                          #  -- Same estimates of the variances, but fixed-effects have different parametrization.

plot(fitted.values(m5),residuals(m5)/summary(m5)$sigma)
plot(m5,form=resid(.,type="pearson") ~ fitted(.) | Subject,abline=0)

# We can visually compare the magnitude of the effects of Type and Subject factors via the use of a "design plot."
# In this case, we can see that the variability of each of the two factors is comparable.
plot.design(ergo)

# We can test for the fixed effect Type via the anova function. Recall here that we do not have replicates, and so we cannot estimate any interaction.  In this case, the interaction
# mean-square serves as the error mean-square.  Its degrees of freedom is equal to [#(treatment) - 1] * [#(subjects) - 1] = (4 - 1) * (9 - 1) = 3*8 = 24.
anova(m5)

# Sometimes we want to form specialized contrasts, in order to test for specific differences between levels of a factor, e.g., Type here.  We use the contrast options to do this.
# The constrast options can be set via a pair of test strings;  the first specifies the contrast function to use for factors, while the second does the same for ordered factors.
options(contrasts = c(factor="contr.treatment",ordered="contr.poly"))   # <--  This is the default. 

# See the set-up induced by this contrast function.  
contrasts( ergo$Type )

# Check out the 95% confidence intervals.
confint(m5)
confint(m6)

# When using REML estimation, we can only use likelihood ratio tests or comparisons of AIC for models with the same fixed-effects structure.




# i = subject
# j = treatment

# Common-intercept model y_ij = Beta_0 + Beta_1 x_ij
# Common-slopes model    y_ij = Beta_0 + Beta_1 x_ij + b_i + e_ij  


