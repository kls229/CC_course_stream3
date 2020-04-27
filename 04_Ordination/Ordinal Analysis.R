##%######################################################%##
#                                                          #
####       Ordination - finding patterns in data        ####
#                                                          #
##%######################################################%##
# Katie Spencer
# 22/04/2020
# https://ourcodingclub.github.io/tutorials/ordination/index.html

# Use ordination to explore patterns in multivariate datasets
#Use package "vegan" for 3 techniques: PCA, PCoA, NMDS

setwd("C:/Users/katie/OneDrive/PhD/My_Stats_from_Scratch/CC_course_stream3-master/CC_course_stream3/04_Ordination")

# Install and load the following packages
install.packages("vegan")
install.packages("ape")
install.packages("dplyr")

library(vegan)
library(ape)
library(dplyr)

# Ordination summarises a multidimensional dataset so patterns can become visualised

#  In ecoloogical terms: Ordination summarizes community data (such as species abundance 
# data: samples by species) by producing a low-dimensional ordination space in which similar 
# species and samples are plotted close together, and dissimilar species and samples are placed far apart.

# Ordination aims at arranging samples or species continuously along gradients.

# Load the community dataset which we`ll use in the examples today
data(varespec)

# Open the dataset and look if you can find any patterns
View(varespec)
# It is probably very difficult to see any patterns by just looking at the data frame!

# With this command, you`ll perform a NMDS and plot the results
varespec %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")

# Can easily see which samples have similar species compositions

# 1) Perform ordination on species abudnance matrix
# 2) Use environmental data to interpret the gradients 



## Principal Component Analysis ----

# Uses a rotation of the original axes to derive new axes, which maximize the variance in the data set.

# The first axis has the highest eigenvalue and thus explains the most variance, the second axis has the second highest eigenvalue, etc

# Number of axes usually = number of samples - one 

## Conduct PCA in R use rda from vegan package 

PCA <- rda(varespec, scale = FALSE)
# Use scale = TRUE if your variables are on different scales (e.g. for abiotic variables).
# Here, all species are measured on the same scale 
# So use scale = FALSE

# Now plot a bar plot of relative eigenvalues. This is the percentage variance explained by each axis
barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig)) 
# How much of the variance in our dataset is explained by the first principal component?

# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 79%, this is ok.
# Also try to do it for the first three axes

# Now, we`ll plot our results with the plot function
plot(PCA)
plot(PCA, display = "sites", type = "points")
plot(PCA, display = "species", type = "text")


# You can extract the species and site scores on the new PC for further analyses:
(sitePCA <- PCA$CA$u) # Site scores
speciesPCA <- PCA$CA$v # Species scores

# In a biplot of a PCA, species' scores are drawn as arrows 
# that point in the direction of increasing values for that variable
biplot(PCA, choices = c(1,2), type = c("text", "points"), xlim = c(-5,10)) # biplot of axis 1 vs 2
biplot(PCA, choices = c(1,3), type = c("text","points")) # biplot of axis 1 vs 3

# come back when need


