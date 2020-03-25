library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(multcomp)
#Data setup
df_long <- read.csv('./yield_data.csv',header = FALSE)
colnames(df_long) <- c("Fertilizer","Yield","plot_size")
df_long$pch <- factor(df_long$plot_size)

# Part A - Draw me a pretty graph
# Box-plot
ggplot(data=df_long, aes(x=Fertilizer, y=Yield))+
  geom_point(data=df_long,pch=21,alpha=0.5,aes(bg=Fertilizer, size=plot_size))+
  scale_size_continuous(range = c(10,8))+
  ggtitle("Crop yield grouped by fertilizer used")+
  xlab("Fertilizer used")+
  ylab("Yield (ton/hectare)")+
  theme(legend.position = "none",
        plot.title =element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Spectral")
# + in Mean values
# + more values on the y axis

# Part B - Explain what alpha is:
# alphj is the difference between the mean yield of the fields exposed to fertilizer 1 and the mean yield of the fields exposed to fertilizer j.
# This allows us to reframe our hypothesis such that if no their is np difference between the fertilizers then
# a2=a3=a4=a5=0

# Part C - Fit the model in Frequentist framework, Report means & alphas, Perform Freq. Hypothsis test, report conclusion with justification
## Fit the model
yield_lm <- lm(Yield ~ Fertilizer,data = df_long)
## Report Means and Alphas
alphas <- yield_lm$coefficients
names(alphas) <- c("mu_hat","alpha2","alpha3","alpha4")
alphas
## Frequentist Hypothesis test
### ANOVA test for rejecting null hypothesis of all means being equivalent
anova(yield_lm)
### "If we take a SIZE of 0.05 to be our threshold then our P value of 0.03388 means we can reject our null hypothesis that the mean yields of each of the groups is equivalent.


#Part D - Tukey
yield_aov <- aov(Yield ~ Fertilizer,data = df_long)
tukey_results <- TukeyHSD(yield_aov)
tukey_analysis <- tukey_results$Fertilizer[,"p adj"]
### "The results of our Tukey can analysis can be seen in "
tukey_analysis
### "Here we see the p-values when directly comparing each of the fertilizer groups in turn. If we continue to use a acceptabel threshold of 0.05 we can see that the only groups we can determine to be ssignificantly different are groups f1 and f4:"
tukey_analysis[which(tukey_analysis < 0.05)]
#"This allows us to reject the null hypothesis that there is no difference between groups f4 and f1 where our probability of not ENCOUNTERING a Type 1 error of 95%"

# Part E - some question about Units???
# Mean Parameterised Linear Model
yield_lm_mp <- lm(Yield ~ Fertilizer -1, data = df_long)
# Generalised Linear Hypothesis test for mu4 0.5 greater than other means
ght <- glht(yield_lm_mp,linfct="Fertilizerf4 - ((Fertilizerf1 + Fertilizerf2 + Fertilizerf3)/3) <= 0.5")
summary(ght)

