library(faraway)
library(magrittr)
library(tidyverse)

#### Chpt 3 --------------------------------------------------------------------

## * Prostate ----------------------------------
data(prostate)
summary(prostate)
m_prostate <- lm(lpsa ~ ., data = prostate)
confint(m_prostate, c("age"), .95)
confint(m_prostate, c("age"), .90)
# age is negatively correlated with lpsa with 90% conficence, but not with 95%

library(ellipse)
plot(ellipse(m_prostate, 3:4), type = "l")
points(coef(m_prostate)[3], coef(m_prostate)[4], pch = 1)
# since the origin is outside of the elipse we reject joint hypothesis age + lweight = 0
# since 0 is outside of the x dimension, we can regect lweight = 0
# we can't reject age = 0, since 0 is in the y dimension

ts <- m_prostate %>% summary() %>% coef()%>% .[4,]
nreps <- 10000
t_stats <- numeric(nreps)
for ( i in 1:nreps ) {
    mod <- lm(sample(lpsa) ~ ., data = prostate)
    t_stats[i] <- summary(mod)$coef[4,3]
}
mean(abs(t_stats) > abs(ts[3]))
# the larger nreps, the better the guess

summary(m)
n <- update(m, . ~ lcavol + lweight + svi)
summary(n)
anova(n, m)
# not siginificantly better model

## * Cheddar ------------------------------
data(cheddar)
str(cheddar)

m <- lm(taste ~ ., data = cheddar)
summary(m)
# Hydrogen Sulfide and Lactic Acid

log(100) # natural log
exp(4.60517) # use to revert
delog_m <- update(m, . ~ exp(Acetic) + exp(H2S) + Lactic)
summary(delog_m)
# now only Lactic Acid if signif at 5%

# f-tests are used to access variation (expected / un-expecte) in a model
anova(m, delog_m)
# there is less risidual variance in the log model

