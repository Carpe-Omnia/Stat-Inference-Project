library(datasets)
library(ggplot2)
library(data.table)
library(dplyr)
data("ToothGrowth")
#View(ToothGrowth)

# taken from https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/ToothGrowth
# The response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs. 
# Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods, 
# orange juice or ascorbic acid (a form of vitamin C and coded as VC).

#The goal here is to perform some basic data analysis on this dataset to see if we can glean some info from it

#Format
#A data frame with 60 observations on 3 variables.

#[,1]	len	numeric	Tooth length
#[,2]	supp	factor	Supplement type (VC or OJ).
#[,3]	dose	numeric	Dose in milligrams/day

#You can see a bit of the raw data below to get a better idea of the format
print(head(ToothGrowth))

#getting an idea of how len responds to different doses of the 2 supplements
# Using data.table

ToothGrowth_dt <- as.data.table(ToothGrowth)
mean_min_max_dt <- ToothGrowth_dt[, .(mean_len = mean(len),
                                      min_len = min(len),
                                      max_len = max(len)),
                                  by = .(supp, dose)] %>%
  setorder(supp)
print(mean_min_max_dt)

#Getting a basic idea of how len responds to dose 
##plotting the data to establish a relationship between dose and len for each supp
ggplot(ToothGrowth, aes(x = dose, y = len)) +
  geom_point() +  # Add points
  facet_wrap(~ supp) + # Create panels based on "sup"
  geom_smooth(method = "lm", se = FALSE) + # Add linear trend lines
  scale_y_continuous(limits = c(min(ToothGrowth$len), max(ToothGrowth$len))) # Set same y-axis scale

#As you can see from the plotted data, the higher doses appear to result in more tooth growth for the rodents. 
#With Orange Juice seemingly having more impact than VC at the lower two doses.
# it appears that that at a dose of 2.0 this difference is muddied and both supps produce a similar effect

#now we will use T inteval testing to establish confidence intervals and try to more conclusively 
#establish this relationship

lowDose <- subset(ToothGrowth, dose==0.5)
t.test(len ~ supp, var.equal = TRUE, data = lowDose)
#this t test establishes a clear difference between the two groups. since the p value is 
# 0.005 we accept the hypothesis that OJ has more impact than VC

mediumDose <-  subset(ToothGrowth, dose==1.0)
t.test(len ~ supp, var.equal = TRUE, data = lowDose)
#this t test establishes a clear difference between the two groups. since the p value is 
# 0.005 we accept the hypothesis that OJ has more impact than VC

highestDose <-  subset(ToothGrowth, dose==2.0)
t.test(len ~ supp, var.equal = TRUE, data = highestDose)
#this t test does not establish a clear difference between the two supplements at the dose of 2.0
#thus we are unable to reject the null hypothesis that the true means are similar
#this keeps with our previous hypotheses created from the chart


