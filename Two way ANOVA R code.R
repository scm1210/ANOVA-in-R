####ANOVA Data Session 

#What is an ANOVA
#An ANOVA is a statistical test used to compare *two or more* groups (e.g., MPG of Red, Blue, and Green cars). 
#It's more efficient than using multiple t-tests

#What is a Two-way ANOVA?
#It's a test is used to evaluate simultaneously the effect of two grouping variables (A, B, and their interaction) on a dependent variable.

#Two-way ANOVA Null Hypotheses:
  #1. There is no difference in the means of factor A
  #2. There is no difference in means of factor B
  #3. There is no interaction between factors A and B

#Statistical Assumptions 
#1. Normality (via inspection of Q-Q plot) 
#2. Homogeneity of variance (levene's test for equality of error variances or residuals versus fits plot )

#For this exercise, we are interested in how mpg (mpg) depends on number of gears (gear) and the number of cylinders it has (cyl).
require(haven) ##using this to read in an SPSS (.sav) file into R 
#set your working directory to where you have this data file, mine is in my Quant Sem folder.
setwd("~/Quant Sem ")
df <- read_sav("mtcars.sav") ###rename to a more concise name 

#Let's check our dataframe and check our summary stats 
summary(df)
require(psych)
describe(df)
View(df)


#Visualize our continuous variable mpg
require(ggplot2)
ggplot(df, aes(x=mpg)) +
  geom_histogram(binwidth = 1, color='white', fill='black')

#From the output, R is considering our variables of interest as different classes than what is needed. 
#Our comparison groups need to be categorized as factors, not characters. Therefore, we need to reclassify them.
df$mpg <- as.numeric(df$mpg)
df$gear <- as.factor(df$gear)
df$cyl <- as.factor(df$cyl)

###check to see if they were indeed reclassified 
str(df)

#Now that we have reclassified our variables of interest, let's trim our data set to make it more manageable. 
newdf <- df[,c("cyl","mpg","gear")]
str(newdf)
View(newdf)
#Generate a table to take a peak at our design.
table(newdf$cyl, newdf$gear) 
#We can see we have a 3X3 design -- that is, we have two variables with three levels.

#visualize our data using boxplots 
install.packages("ggpubr")
library(ggpubr)
boxplot(mpg ~ gear*cyl, data=newdf, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), ylab="Vehicle MPG")

#Now let's build our ANOVA. 
#remember we are interested in how mpg differs as a function of number of cylinders, number of gears, as well as their interaction. 
aov1 <- aov(mpg ~ gear * cyl, data = newdf) ### the asterisk denotes that we are interest in two main effects AND their interaction 
summary(aov1)

#So based on our ANOVA we can see that we have significant main effects for number of gears and cylinder count. 
#But we did not observe a significant interaction. 
#Now we need to check our assumptions before we move on: 1. Normality, 2. Homogeneity of variance

plot(aov1, 2)#normality

plot(aov1, 1)#homogeneity of variance (HoV)

##can also check HoV using Levene's test for equality of error variances 
require(car)
leveneTest(mpg ~ cyl*gear, data = newdf)
###we do not a significant stat so that means we do not violate assumption of HoV. If it was significant we would need to use corrected degrees of freedom and F stat.

###Most of our points fall along the line so we are ok with normality, we can also check this with a shapiro wilks test.
# Extract the residuals
aov_residuals <- residuals(object = aov1)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals) ###we can check the assumption of normality off as well.


#Now let's compute some summary stats using dplyr 
require(dplyr)
group_by(newdf, gear) %>%
  summarise(
    count = n(),
    mean = mean(mpg, na.rm = TRUE),
    sd = sd(mpg, na.rm = TRUE)
  )

group_by(newdf, cyl) %>%
  summarise(
    count = n(),
    mean = mean(mpg, na.rm = TRUE),
    sd = sd(mpg, na.rm = TRUE)
  )

#Since our ANOVA test was significant, we can compute Tukey HSD post hoc test (Tukey Honest Significant Differences)
#The function TukeyHD() takes the fitted ANOVA as an argument.

TukeyHSD(aov1, which = "gear")
TukeyHSD(aov1, which = "cyl")
###diff: difference between means of the two groups
###lwr, upr: the lower and the upper end point of the confidence interval at 95% (default) 
#if the confidence interval crosses zero it is not significant 
###p adj: p-value after adjustment for the multiple comparisons (reduces chances of Type 1 error).

summary(aov1)
require("effectsize")
eta_squared(aov1, partial = FALSE) ###gives us our effect sizes, that is the proportion of the variance that is accounted for by each factor
###if the CI includes zero = NS

#you can check the R markdown file for an example of how to write up a factorial ANOVA

#We can also visualize our two-way ANOVA using a line graph
require(ggplot2)
ggplot(newdf, aes(x = gear, y = mpg, fill = cyl)) + 
  geom_boxplot(width = .2, alpha = .5, outlier.size = -1) + 
  stat_summary(aes(group = cyl, color = cyl), fun.y = "mean", geom = "line", 
               position = position_dodge(.2), size = 1) + 
  stat_summary(fun.y = "mean", geom = "point", shape = 22, size = 2, 
               position = position_dodge(.2), show.legend = TRUE) + 
  scale_y_continuous(limits = c(0, 30)) + 
  labs(title = "Effect of Gear Number on Car MPG by Number of Cylinders", 
       x = "Gearbox Size ", y = "Miles per Gallon") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank())
