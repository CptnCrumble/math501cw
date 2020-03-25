library(ggplot2)
library(tidyr)
#Data setup
f_one <- c(3,2,4,3,5)
f_two <- c(5,4,2,6,6)
f_three <- c(7,6,4,6,4)
f_four <- c(7,5,5,6,9)

df_wide <-  cbind(data.frame(rbind(f_one,f_two,f_three,f_four)),c("f_one","f_two","f_three","f_four"))
colnames(df_wide) <- c("X1","X2","X3","X4","X5","Fertilizer")

df_long <- read.csv('./yield_data.csv')
colnames(df_long) <- c("Fertilizer","Yield")

# Part A - Draw me a pretty graph
# Box-plot 

# Scatter plot - colour co-ordinated

# Part B - Explain what alpha is:
# alphj is the difference between the mean yield of the fields exposed to fertilizer 1 and the mean yield of the fields exposed to fertilizer j.
# This allows us to reframe our hypothesis such that if no their is np difference between the fertilizers then 
# a2=a3=a4=a5=0

#Part D - Fit the model in the frequentist framework