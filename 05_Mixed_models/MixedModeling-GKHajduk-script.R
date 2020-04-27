######################################
#                                    #
#   Mixed effects modeling in R     #
#                                    #
######################################

## authors: Gabriela K Hajduk, based on workshop developed by Liam Bailey
## contact details: gkhajduk.github.io; email: gkhajduk@gmail.com
## date: 2017-03-09
## https://ourcodingclub.github.io/tutorials/mixed-models/index.html

# Mixed models were developed to deal with messy data 


###---- Explore the data -----###

## load the data and have a look at it

load("dragons.RData")

head(dragons)

## Let's say we want to know how the body length affects test scores.

## Have a look at the data distribution:

hist(dragons$testScore)  # seems close to normal distribution - good!

## It is good practice to  standardise your explanatory variables before proceeding - you can use scale() to do that:

dragons$bodyLength2 <- scale(dragons$bodyLength)

## Back to our question: is test score affected by body length?

###---- Fit all data in one analysis -----###

## One way to analyse this data would be to try fitting a linear model to all our data, ignoring the sites and the mountain ranges for now.

library(lme4)

basic.lm <- lm(testScore ~ bodyLength2, data = dragons)

summary(basic.lm)

## Let's plot the data with ggplot2

library(ggplot2)

ggplot(dragons, aes(x = bodyLength, y = testScore)) +
  geom_point()+
  geom_smooth(method = "lm")


### Assumptions?

## Plot the residuals - the red line should be close to being flat, like the dashed grey line

plot(basic.lm, which = 1)  # not perfect, but look alright

## Have a quick look at the  qqplot too - point should ideally fall onto the diagonal dashed line

plot(basic.lm, which = 2)  # a bit off at the extremes, but that's often the case; again doesn't look too bad


## However, what about observation independence? Are our data independent?
## We collected multiple samples from eight mountain ranges
## It's perfectly plausible that the data from within each mountain range are more similar to each other than the data from different mountain ranges - they are correlated. Pseudoreplication isn't our friend.

## Have a look at the data to see if above is true
boxplot(testScore ~ mountainRange, data = dragons)  # certainly looks like something is going on here

## We could also plot it colouring points by mountain range
ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange))+
  geom_point(size = 2)+
  theme_classic()+
    theme(legend.position = "none")

## From the above plots it looks like our mountain ranges vary both in the dragon body length and in their test scores. This confirms that our observations from within each of the ranges aren't independent. We can't ignore that.

## So what do we do?

###----- Run multiple analyses -----### ----


## We could run many separate analyses and fit a regression for each of the mountain ranges.

## Lets have a quick look at the data split by mountain range
## We use the facet_wrap to do that

ggplot(aes(bodyLength, testScore), data = dragons) + geom_point() +
    facet_wrap(~ mountainRange) +
    xlab("length") + ylab("test score")

# Facetwrap creates multi-panel plots 

##----- Modify the model -----###

## We want to use all the data, but account for the data coming from different mountain ranges (without decreasing sample size and increase Type 1 errors by doing multiple comparisons)

## let's add mountain range as a fixed effect to our basic.lm

mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)

## estimating the difference in test scores between the mountain ranges

## now body length is not significant, but we just want to know whether body length affects test scores and we want to simply control for the variation coming from mountain ranges.

# This is what we refer to as "random factors"  -> need mixed effects models!

###----- Mixed effects models -----### ----

# Accounts for the correlations between data coming from the sites and mountain ranges

library(lme4)
# Fixed and random effects depend more on the research question than the variables
# Fixed effects are expected to have an effect on response variable
# Random effects are grouping factors which we are trying to control (categorical) - not interested in their impact, but know they may influence the response variable
# Often want to know how much VARIATION is attributable to the random effects
# Random effects need to have at least 5 levels/factors - for precise variance estimates

# Eg mountain ranges may be correlated and therefore may have an effect on dragon intelligence, so need to contro, for that.



##----- First mixed model -----## ----

# residual variation (i.e. unexplained variation) - by using random effects, we are modeling that unexplained variation through variance.

#We will fit the random effect usingv the syntax (1|variableName): Whatever is on the right side of the | operator is a factor and referred to as a “grouping factor” for the term.

mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)

# Fit a linear mixed-effect model, with test score as a function of body length, with mountain range as a random effect

summary(mixed.lmer)

# The random effects part tells us how much variance there is between grouping factors + residual variance 
# The fixed effect part is similar to the linear model output 

# The model estimate (0.5377) is smaller than its associated error (1.2750) - slope cannot be distinguished from zero

# Take the variance of the mountain range (339.7) and divide by total variance:

339.7/(339.7 + 223.8)  # ~60 %

# Differences between mountain ranges explain ~60% of the variance that’s “left over” after the variance explained by our fixed effects.

### plots

plot(mixed.lmer)  # looks alright, no patterns evident

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!

## Crossed vs Nested random effect ----

# Not all MM are hierachal, can have crossed random factors which aren't levels in a hierachy

# An effect is (fully) crossed when all the subjects have experienced all the levels of that effect.

# Nested random effects are hierachal - some element of scale, avoids
# pseudoreplication = increasing your sampling size by using non-independent data

# Nested random effects occur when a lower level factor appears only within a particular level of an upper level factor.
#   (1|class/pupil)  # or  (1|class) + (1|class:pupil)

# Crossed random effects means that a given factor appears in more than one level of the upper level factor.

# (1|class) + (1|pupil)

#e.g leaves in plants in beds ->

# leafLength ~ treatment + (1|Bed/Plant/Leaf)

##-- implicit vs explicit nesting --##

head(dragons)  # we have site and mountainRange
str(dragons)  # we took samples from three sites per mountain range and eight mountain ranges in total

# or nested random effects, the factor appears ONLY within a particular level of another factor 
# (each site belongs to a specific mountain range and only to that range); for crossed effects, 
# a given factor appears in more than one level of another factor (dragons appearing within more than one mountain range)

# If they're not nested then they're crossed!

# Can create a new variable, so that is explicitly nested (level linked to a factor e.g site within a mountain is assigned just to that mountain)

dragons <- within(dragons, sample <- factor(mountainRange:site))

##----- Second mixed model -----##

# The below code is wrong, as it implies there are only 3 sites with observations at each of the 8 mountains (crossed):

mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)  # treats the two random effects as if they are crossed
summary(mixed.WRONG)

# Can see grouping of observations is wrong: says three when there are 24 locations.

# Need to ask:  Is there an association between body length and intelligence in dragons after controlling for variation in mountain ranges and sites within mountain ranges

dragons <- within(dragons, sample <- factor(mountainRange:site))


mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)

# Or use (1|mountainRange/site) or even (1|mountainRange) + (1|mountainRange:site)

#Can see group is now 24

# Visualise 

(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)


### ## Random slopes----

# Lines are straight because we've fitted random-intercept models (intercept can vary for each level of the random effect, but slop is constant

#e.g expect drangons in all mountaisn to exbibit same relationship between body length and intelligence (fixed slope), but
# acknowledge some populatiosn may be smarter/dumber (random intercept)

# But actually, sites far apart may have important differences - need to fit a random slope and random intercept model

# Need just one change to model for random slopes as well as intercept - add fixed variable into the random effects bracket!

mixed.slope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange/site), data = dragons) 
summary(mixed.slope)

# let’s model the intelligence of dragons as a function of body length, knowing that populations have different intelligence baselines and that the relationship may vary among populations.

# Visualise


### plot
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.ranslope)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)


### Summary ----


#Body length affected the test score until we accounted for the variation coming from mountain ranges.
#  We can see now that body length doesn’t influence the test scores



###  Plotting model predictions ----

# Good to visualise 

library(ggeffects)  # install the package first if you haven't already, then load it

# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer2, terms = c("bodyLength2"))  # this gives overall predictions for the model

# Plot the predictions 

(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = dragons,                      # adding the raw data (scaled values)
               aes(x = bodyLength2, y = testScore, colour = mountainRange)) + 
    labs(x = "Body Length (indexed)", y = "Test Score", 
         title = "Body length does not affect intelligence in dragons") + 
    theme_minimal()
)

# visualise how the relationships vary according to different levels of random effects?

# random intercepts and fixed slopes

ggpredict(mixed.lmer2, terms = c("bodyLength2", "mountainRange"), type = "re") %>% 
  plot() +
  labs(x = "Body Length", y = "Test Score", title = "Effect of body size on intelligence in dragons") + 
  theme_minimal()

# To show the variation among levels of your random effects, plot the departure from the overall model estimate for intercept and slope:


library(sjPlot)

# Visualise random effects 
(re.effects <- plot_model(mixed.slope, type = "re", show.values = TRUE))

# The values are the DIFFERENCES between the general intercept and slope in the model summary, & the estimate for this specific level of random effect

# show summary
summary(mixed.slope)

## Tables ----


library(stargazer) # Produces table for lme4

# https://www.jakeruss.com/cheatsheets/stargazer/


stargazer(mixed.lmer2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# broom package is good for collating model results from multiple models /plots 

##----- Model selection for the keen ----

# Fixed effect structure

# DOn't have P-values, so need to assess model with other ways 

# MCMC or parametric bootstrap confidence intervals is best but will discuss in another workshop

# Look at liklihood ratio tests using ANOV (best for large sample sizes)

# Fit the models: 1 full and 1 reduced (dropped fixed effect bodylength2)


full.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), 
                  data = dragons, REML = FALSE)

reduced.lmer <- lmer(testScore ~ 1 + (1|mountainRange) + (1|sample), 
                     data = dragons, REML = FALSE)

### comparison

anova(reduced.lmer, full.lmer)  # the two models are not significantly different

p = 0.59

# Or can use AIC- if models are within 2 AICc units of each other they are very similar. Within 5 units they are quite similar, over 10 units difference and you can probably be happy with the model with lower AICc.

# Random effects structure ----

# These deal with pseudo replication, doesn't matter if they are significant or not

# use REML estimators for comparison of models with different random effects (we keep fixed effects constant)

# Do NOT vary random and fixed effects at the same time - either deal with your random effects structure or with your fixed effects structure at any given point.


## Entire model selection ----

# Can do top-down or step-up, but may get different results with there

# Top-down is recommended 











