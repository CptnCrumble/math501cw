library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
#Data setup
# !!!- CHOP ??? ---------------------------------------------------------------------------------------
f_one <- c(3,2,4,3,5)
f_two <- c(5,4,2,6,6)
f_three <- c(7,6,4,6,4)
f_four <- c(7,5,5,6,9)

df_wide <-  cbind(data.frame(rbind(f_one,f_two,f_three,f_four)),c("f_one","f_two","f_three","f_four"))
colnames(df_wide) <- c("X1","X2","X3","X4","X5","Fertilizer")
# !!!- CHOP ??? ---------------------------------------------------------------------------------------

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
yield_MM <- model.matrix(yield_lm)
### "If we take a SIZE of 0.05 to be our threshold then our P value of 0.03388 means we can reject our null hypothesis that the mean yields of each of the groups is equivalent.


#Part D - Tukey



# Part E - some question about Units???
