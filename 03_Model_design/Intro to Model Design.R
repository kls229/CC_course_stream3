##%######################################################%##
#                                                          #
####               Intro to model design                ####
#                                                          #
##%######################################################%##
# Katie Spencer
# 20.04.2020
# https://ourcodingclub.github.io/tutorials/model-design/index.html

#Good practise: What is the research question? What is the hypothesis? What is the null hypothesis?

# Hierarchical models to minimize the assumptions that we are making about our data

# Does the model need to be detection or attribution?

# Load libraries ----
library(dplyr)  # for data manipulation
library(ggplot2)  # for data visualisation
library(lme4)  # for models
install.packages("sjPlot");library(sjPlot)  # to visualise model outputs
install.packages("ggeffects");library(ggeffects)  # to visualise model predictions
install.packages("MCMCglmm");library(MCMCglmm)  # for models
install.packages("MCMCvis");library(MCMCvis)  # to visualise model outputs
install.packages("brms");library(brms)  # for models
install.packages("stargazer");library(stargazer)  # for tables of model outputs
install.packages("glmmTMB"); library(glmmTMB)

# Load data ----
# Remember to set your working directory to the folder

getwd()
setwd("C:/Users/katie/OneDrive/PhD/My_Stats_from_Scratch/CC_course_stream3-master/CC_course_stream3/03_Model_design")
# where you saved the workshop files
toolik_plants <- read.csv("toolik_plants.csv")

# Inspect data
head(toolik_plants)
str(toolik_plants)

toolik_plants$Plot <- as.factor(as.character(toolik_plants$Plot))
toolik_plants

# Get the unique site names
unique(toolik_plants$Site) # 5 sites
length(unique(toolik_plants$Site)) 

# Group the dataframe by Site to see the number of blocks per site
toolik_plants %>% group_by(Site) %>%
  summarise(block.n = length(unique(Block)))

unique(toolik_plants$Year)

# How many species are representd in the data?

length(unique(toolik_plants$Species)) # 129 species

unique(toolik_plants$Species) # Probably best to look at the data

# Not every entry is a plant, time for data manipulation with filter and dplyr


# We use ! to say that we want to exclude
# all records that meet the criteria

# We use %in% as a shortcut - we are filtering by many criteria
# but they all refer to the same column: Species
toolik_plants <- toolik_plants %>%
  filter(!Species %in% c("Woody cover", "Tube",
                         "Hole", "Vole trail",
                         "removed", "vole turds",
                         "Mushrooms", "Water",
                         "Caribou poop", "Rocks",
                         "mushroom", "caribou poop",
                         "animal litter", "vole poop",
                         "Vole poop", "Unk?"))

# A much longer way to achieve the same purpose is:
# toolik_plants <- toolik_plants %>%
#  filter(Species != "Woody cover" &
#	       Species != "Tube" &
#         Species != "Hole"&
#				 Species != "Vole trail"....))
# But you can see how that involves unnecessary repetition.

length(unique(toolik_plants$Species)) # Only 115 species now

# How many species were recorded in each plot in each survey year:

# Calculate species richness
toolik_plants <- toolik_plants %>%
  group_by(Year, Site, Block, Plot) %>%
  mutate(Richness = length(unique(Species)))

# Make a histogram

(hist <- ggplot(toolik_plants, aes(x = Richness)) +
    geom_histogram() +
    theme_classic())

(hist2 <- ggplot(toolik_plants, aes(x = Relative.Cover)) +
    geom_histogram() +
    theme_classic())   #Skewed to the left, need poisson distribution, not normal

# How has plant species richness changed over time at Toolik lake? ----

# e.g  richness ~ time, but what other things need to be accounted for?


# General linear models ----

(plant_m <- lm(Richness ~ I(Year-2007), data = toolik_plants)) # - I(Year - 2007) means that the year 2008 will become Year 1 - then your model is estimating richness across the first, second, etc., year from your survey period
summary(plant_m)

# Need to account for site-level effects that makes data non-independent

# Model convergence: Whether the underlying maths in the model works or not

# With parametric tests, check the residual vs the predicted plots 

# FOr Bayesian, there are seperate plots and stats (see MCMC)

# plot() plots the residuals vs fitted, Q-Q etc this helps identify outliers and confirm model has run

plot(plant_m)

# Hierachal models using lme4 ----

# Only treat site as one random effect first, ignoring other things like plots etc


plant_m_plot <- lmer(Richness ~ I(Year-2007) + (1|Site), data = toolik_plants)
summary(plant_m_plot)
plot(plant_m_plot)  # Checking assumptions

# Affect sizes tell us about the strengths of the relationships we are testing

# Add plots and blocks to see how effect size changes

plant_m_plot2 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block), data = toolik_plants)
summary(plant_m_plot2)

plant_m_plot3 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block/Plot), data = toolik_plants)
summary(plant_m_plot3)

# (effect size = I(Year - 2007) -0.706851   0.006783 -104.216)

# The final model shows how species richness changes over time & accounts for hierachal structure of the data

# Set a clean theme for the graphs
set_theme(base = theme_bw() + 
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm")))

# Visualises random effects 
(re.effects <- plot_model(plant_m_plot3, type = "re", show.values = TRUE))
save_plot(filename = "model_re.png",
          height = 11, width = 9)  # Save the graph if you wish


#The first two show the interaction effects. Here, we are only interested in the plot that shows us the random effects of site

# To see the estimate for our fixed effect (default): Year
(fe.effects <- plot_model(plant_m_plot3, show.values = TRUE))
save_plot(filename = "model_fe.png",
          height = 11, width = 9)  # Save the graph if you wish




plant_m_temp <- lmer(Richness ~ Mean.Temp + (1|Site/Block/Plot) + (1|Year),
                     data = toolik_plants)
summary(plant_m_temp)


# Visualise the random effect terms
(temp.re.effects <- plot_model(plant_m_temp, type = "re", show.values = TRUE))
save_plot(filename = "model_temp_re.png",
          height = 11, width = 9)

# Visualise the fixed effect
(temp.fe.effects <- plot_model(plant_m_temp, show.values = TRUE))
save_plot(filename = "model_temp_fe.png",
          height = 11, width = 9)

# The last figure (one line) the random effect of year.


## Random slopes vs random intercepts ----

# Each plot can have its own relationship with temperature:

plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Site/Block/Plot) + (1|Year), data = toolik_plants)
summary(plant_m_rs)


plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Plot) + (1|Year), data = toolik_plants)
summary(plant_m_rs)

# These models are not converging because the model structure is too complicated 

# Try with just a plot random intercept and with random slopes to see what a random slope looks like:

plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Plot),
                   data = toolik_plants)
summary(plant_m_rs)

# Visulise the results:

(plant.re.effects <- plot_model(plant_m_rs, type = "re", show.values = TRUE))
save_plot(filename = "model_plant_re.png",
          height = 17, width = 15)

(plant.fe.effects <- plot_model(plant_m_rs, show.values = TRUE))
save_plot(filename = "model_plant_fe.png",
          height = 14, width = 9)


## Use the ggeffects package to calculate model predictions and plot them:

# Calculate the overall predictions for the relationship between species richness
# and temperature. Then, we calculate the predictions for each plot, thus visualising
# the among-plot variation


ggpredict(plant_m_rs, terms = c("Mean.Temp")) %>% plot()
save_plot(filename = "model_temp_richness.png",
          height = 9, width = 9)

ggpredict(plant_m_rs, terms = c("Mean.Temp", "Plot"), type = "re") %>% plot()
save_plot(filename = "model_temp_richness_rs_ri.png",
          height = 9, width = 9)

## I'm not sure what's going on here tbf.. maybe come back


## Hierachal models using MCMCglmm ----


# MCMCglmm fits Generalised Linear Mixed-effects Models using a Markov chain Monte Carlo approach under a Bayesian statistical framework.

# Like lme4, in MCMCglmm, we can add random and fixed effects 

# There is greater flexibility in terms of specifying priors - that is, you can give your model additional information that is then taken into account when the model runs.

# Also good for zer-infated data

# 1) Built the model slowly, start woth site random effect

plant_mcmc <- MCMCglmm(Richness ~ I(Year - 2007), random = ~Site,
                       family = "poisson",  data = toolik_plants)

# Error - possible because there isn't enough sites or enough variation in the data

# Include block and plot as random effects (here they are random intercepts)


plant_mcmc <- MCMCglmm(Richness ~ I(Year-2007), random = ~Block + Plot,
                       family = "poisson", data = toolik_plants)
 
summary(plant_mcmc)

# species richness has declined overtime - The posterier mean (slop) for the year term in -0.07 (log scale)

# Check model convergence using trace plots - they should look like fuzzy caterpillars

plot(plant_mcmc$VCV)# random effects
plot(plant_mcmc$Sol)# fixed effects

# They don't look like caterpillars - don't trust the mcmc model, even though it gave outputs, because it's not accounting for site effects

## See what the MCMCglmm models are like when we estimate changes in the cover of one species - Betula nana, dwarf birch.

## Added code for parameter-expanded priors - improve model convergence 


# Set weakly informative priors
prior2 <- list(R = list(V = 1, nu = 0.002),
               G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000),
                        G2 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000),
                        G3 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000)))

# Extract just the Betula nana data
betula <- filter(toolik_plants, Species == "Bet nan")

betula_m <- MCMCglmm(round(Relative.Cover*100) ~ Year, random = ~Site + Block + Plot,
                     family = "poisson", prior = prior2, data = betula)

summary(betula_m) # Effect size for year is v.small (0.00493) Bet nan hasn't changed over the years by much
plot(betula_m$VCV)
plot(betula_m$Sol)

# trace plots for this model are a bit better than the previous one and we have included all three levels of our experimental hierarchy as random intercept

# Visualise the model outputs

MCMCplot(betula_m$Sol)
MCMCplot(betula_m$VCV)

# If the credible intervals overlap zero, then those effects are not significant, so we can see here that Betula nana cover hasn’t changed.




















