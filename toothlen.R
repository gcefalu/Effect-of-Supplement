rm(list=ls())

library(dplyr)
library(ggplot2)
library(datasets)
library(gridExtra)
library(knitr)
library(rmarkdown)

data(ToothGrowth)
data <- ToothGrowth

cat("EFFECT OF SUPPLEMENT TYPE AND DOSE IN TOOH GROWTH\n")
cat("This is and analysis of the R TeoohGrowth data in R data sets package\n")

cat("EXPLORATORY DATA ANALYSIS\n\n")

cat("VARIABLES")
print(str(data))

cat("SUMMARY")
params <- data %>%
  group_by(supp) %>%
  summarise(Mean = mean(len, na.rm = TRUE), Sd = sd(len, na.rm = TRUE), 
            Max = max(len, na.rm = TRUE), Min = mean(len, na.rm = TRUE),
            Median = median(len, na.rm = TRUE),
            Quantile0 = quantile(len, na.rm = TRUE, disp = 0, prob = 0),
            Quantile25 = quantile(len, na.rm = TRUE, disp = 0.25, prob = 0.25),
            Quantile50 = quantile(len, na.ram = TRUE, disp = 0.5, prob = 0.5),
            Quantile75 = quantile(len, na.rm = TRUE, disp = 0.75, prob = 0.75),
            Quantile100 = quantile(len, na.rm = TRUE, disp = 1, prob = 1))

print(params)

cat("\nThere is some variation in the data between the 2 samples. I will assume that
variance is different\n")

params2 <- data %>%
  group_by(dose) %>%
  summarise(Mean = mean(len, na.rm = TRUE), Sd = sd(len, na.rm = TRUE), 
            Max = max(len, na.rm = TRUE), Min = mean(len, na.rm = TRUE),
            Median = median(len, na.rm = TRUE),
            Quantile0 = quantile(len, na.rm = TRUE, disp = 0, prob = 0),
            Quantile25 = quantile(len, na.rm = TRUE, disp = 0.25, prob = 0.25),
            Quantile50 = quantile(len, na.ram = TRUE, disp = 0.5, prob = 0.5),
            Quantile75 = quantile(len, na.rm = TRUE, disp = 0.75, prob = 0.75),
            Quantile100 = quantile(len, na.rm = TRUE, disp = 1, prob = 1))
print(params2)

p <- ggplot(data, aes(x = dose, y = len, fill =  factor(dose))) + 
  geom_dotplot(binaxis='y',  dotsize=0.75) + facet_grid(.~supp) +
  stat_summary(fun=mean, geom="point", shape=18,
               size=3, color="black") + ggtitle("Tooth growth versus dose and suplement") +
  theme(plot.title=element_text(face="bold", size=9))

OJ <- filter(data, supp == 'OJ', na.rm = TRUE)
VC <- filter(data, supp == 'VC', na.rm = TRUE)

pdOJ <- ggplot(OJ, aes(x =len, fill = 30)) + theme_bw() + 
  geom_histogram(aes(y = ..density..), alpha = 0.7, binwidth = 0.75,  col = "black") +
  ggtitle("OJ Supplement tooth growth distribution") +
  theme(plot.title=element_text(face="bold", size=9))


pdVC <- ggplot(VC, aes(x =len, fill = 30)) + theme_bw() + 
  geom_histogram(aes(y = ..density..), alpha = 0.7,  binwidth = 0.75, col = "black") +
  ggtitle("VC Supplement tooth growth dsitribution") + 
  theme(plot.title=element_text(face="bold", size=9))

grid.arrange(p, pdOJ, pdVC)

cat("\nTeeth growth may be affected by dose and type of supplement\n\n") 

cat("HYPOTHSESIS TESTING\n\n")
cat("Assumptions:\n")
cat("    The variables are (iid) independent and identically distributed.
    tooth growth changes with supplement and dose
    Tooth growth data are normally distributed in the two samples: OJ and CV
    Variance seems to be different both while grouping the data by supp or dose
    I will use a T test with a significance level with alpha=0.05 var.equal = FALSE\n\n")

cat("\nEFECT OF SUPLEMENT ON TEETH GROWTH\n\n")

OJlen <- select(OJ, len)
VClen <- select(VC, len)

print(t.test(OJlen, VClen, alternative = "two.sided", paired = FALSE, 
             var.equal = FALSE, conf.level = 0.95))

cat("It does not seem to be any difference in the effect exerted by supplement type
in teeth growth\n")

cat("\nEFFECT OF SUPPLEMENT DOSE ON TEETH GROWTH\n\n")

data0.5 <- filter(data, dose == 1/2)
data1 <- filter(data, dose == 1)
data2 <- filter(data, dose == 2)


Len05 <- select(data0.5, len)
Dose05 <- select(data0.5, dose)

Len1 <- select(data1, len)
Dose1 <- select(data1, dose)

Len2 <- select(data2, len)
Dose2 <- select(data2, dose)


print(t.test(Len05,Len1, alternative = "two.sided", paired = F,
             var.equal = F, conf.level = 0.95))
print(t.test(Len1, Len2, alternative = "two.sided", paired = F,
             var.equal = F, conf.level = 0.95))
print(t.test(Len05,Len2, alternative = "two.sided", paired = F,
             var.equal = F, conf.level = 0.95))

cat("Supplement dose affects teeth growth")

cat("\nCONCLUSSION\n\n")
cat("increasing the supplement's dose benefits tooth growth, but the type of supplement does not have
an effect on teeth growth.")



