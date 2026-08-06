
(from http://lme4.r-forge.r-project.org/book/Ch1.pdf)



library(lme4)
str(Dyestuff)
library(ggplot2)

ggplot(Dyestuff, aes(Yield, as.numeric(reorder(Dyestuff$Batch, Dyestuff$Yield, mean))))+geom_point()+geom_smooth(group=6)

fm1 <- lmer(Yield ~ 1 + (1|Batch), Dyestuff)
fm1

see stackexchange (http://stats.stackexchange.com/questions/7679/am-i-specifying-my-lmer-model-correctly) for more about specifying a model with lmer













