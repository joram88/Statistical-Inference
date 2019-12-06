#Final project for Statistical Inference by Jose Ramon Pineda

library(tidyverse)
library(ggpubr)

#PART 1

lambda <-  0.2
n <- 40
sim <- 1000
set.seed(seed = 421)
mu <- 1/lambda

#To begin, we expect a mean of 5 (1/lambda)

test <- replicate(sim, rexp(n, lambda))
meantest <- data.frame(means = apply(test, 2, mean))

#Sample mean turns out to be close to the expected 5 at 5.02

var <- ((1/lambda)/sqrt(n))^2
testvar <- var(meantest$means)
testvar

#We see that the expected variance (6.25) is a bit higher than the 
#variance we got in the test (5.99)

ggplot(data = meantest, aes(x = means)) + 
  geom_histogram(binwidth=0.1, aes(y=..density..), alpha=0.2) + 
  geom_vline(xintercept = mu, size=1, colour="red") + 
  geom_density(colour="blue", size=1) +
  scale_x_continuous(breaks=seq(mu-5,mu+5,1), limits=c(mu-5,mu+6))  +
  stat_function(fun = dnorm, args = list(mean = mu , sd = sqrt(var)), colour = "red", size=1) 

#Graphically this seems like a normal distribution, but lets run a Shapiro test to confirm

shapiro.test(meantest$means)

#It seems the p value is extremely small, so we reject the Ho that this is normally distributed

#We also run a qqplot to visualize this some more. We see then that some of the points on the right tail
#fall outside the confidence interval testing normality, which explains the Shapiro test results


ggqqplot(meantest$means)

#######################################################################

#PART 2

tooth <- ToothGrowth

#This dataset contains information on:
#The Effect of Vitamin C on Tooth Growth in Guinea Pigs
# Vitamins were administered via orange juice (OJ) or ascorbic acid (VC)

g <- ggplot(tooth, aes(x = supp, y = len, fill = supp))
g +geom_boxplot()

tooth$dose <- as.factor(tooth$dose)

h <- ggplot(tooth, aes(x = dose, y = len, fill = dose))
h +geom_boxplot()

#We can tell from our early graphs that VC seems to have a weaker effect on len than
#if OC was administered. Also, unsurprisingly, if the dose is higher the effect is larger

#We will use the techniques learned in the lectures going forward

group <- as.character(tooth$supp)
testStat <- function(w, g) mean (w[g=="OJ"]) - mean (w[g =="VC"])
observedStat <- testStat(tooth$len, group)
observedStat

#The observed difference between groups is 3.7 in length in favor of OC

permutations <- sapply(1:1000, function(i) testStat (tooth$len, sample(group)))
mean(permutations > observedStat)

#After 1000 permutations we found 3% of the datasets that were larger than the original data
# The p - value is small though still, so we reject the null hypothesis at an alpha of 0.05

ggplot()+aes(permutations)+geom_histogram(binwidth=1, color="lightblue")+
    geom_vline(aes(xintercept=3.7, color="red", size =0.05), show.legend = FALSE)

# From our histogram we see that most observations fall below that mean we calculated of 3.7.
# So a 0 mean difference is very unlikely.

# Next let's build the Confidence intervals and p - values of our observations
#based on supplement

g1 <- tooth$len[1:30]; g2 <- tooth$len[31:60]
difference <-  g2 - g1

t.test(difference, paired = FALSE)


#Based on this we see we have our confidence interval and a very low p value
# of 0.0025 which implies that OJ has a more positive effect on length than VC
# of 1.41 to 5.99 in 95% of cases

#Now we do the same for dose for both supplements separately

h1oj <- tooth %>% 
      filter(supp == "OJ") %>% 
      arrange(dose) %>% 
      slice(1:10) %>% 
      select(len)

h2oj <- tooth %>% 
  filter(supp == "OJ") %>% 
  arrange(dose) %>% 
  slice(11:20) %>%
  select(len)

h3oj <- tooth %>%
  filter(supp == "OJ") %>% 
  arrange(dose) %>% 
  slice(21:30) %>% 
  select(len)

difference2oj <- h3oj-h2oj

t.test(difference2oj, paired = FALSE)

difference3oj <- h2oj-h1oj

t.test(difference3oj, paired = FALSE)

#In the case of OJ it seems that the p value is a bit higher when comparing a dose of 2 with 1
#With a possibility that there is no noticeable effect (lower CI goes below 0)
#which passes the p-test at 10%. Whereas for doses of 1 to 0.5 the effect is clearly
#positive, with a very low p-value and high CI.

h1vc <- tooth %>% 
  filter(supp == "VC") %>% 
  arrange(dose) %>% 
  slice(1:10) %>% 
  select(len)

h2vc <- tooth %>% 
  filter(supp == "VC") %>% 
  arrange(dose) %>% 
  slice(11:20) %>%
  select(len)

h3vc <- tooth %>% 
  filter(supp == "VC") %>% 
  arrange(dose) %>% 
  slice(21:30) %>% 
  select(len)

difference2vc <- h3vc-h2vc

t.test(difference2vc, paired = FALSE)

difference3vc <- h2vc-h1vc

t.test(difference3vc, paired = FALSE)

#As for VC we can see that increased dosing has a very statstically significant positive effect
#on length, with high/positive CI and p-values that are extremely low. The Ho is rejected.




