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

df_long <- read.csv('./yield_data.csv')
colnames(df_long) <- c("Fertilizer","Yield","plot_size")
df_long$pch <- factor(df_long$plot_size)

# Part A - Draw me a pretty graph
# Box-plot
ggplot(data=df_long, aes(x=Fertilizer, y=Yield))+
  geom_point(data=df_long,pch=21,alpha=0.5,aes(bg=Fertilizer, size=plot_size))+
  scale_size_continuous(range = c(7, 10))+
  ggtitle("Crop yield grouped by fertilizer used")+
  xlab("Fertilizer used")+
  ylab("Yield (ton/hectare)")+
  theme(legend.position = "none",
        plot.title =element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Spectral")


# Scatter plot - colour co-ordinated

# Part B - Explain what alpha is:
# alphj is the difference between the mean yield of the fields exposed to fertilizer 1 and the mean yield of the fields exposed to fertilizer j.
# This allows us to reframe our hypothesis such that if no their is np difference between the fertilizers then
# a2=a3=a4=a5=0

# Part c -
df_long %>% group_by(Fertilizer) %>% summarise(mean_yeild = mean(Yield),
                                               mean_sd = sd(Yield))

#Part D - Fit the model in the frequentist framework
