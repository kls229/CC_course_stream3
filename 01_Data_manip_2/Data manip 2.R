##%######################################################%##
#                                                          #
####                Data manipulation 2                 ####
#                                                          #
##%######################################################%##
# Katie Spencer
# 20/04/2020

# Piping, dplyr etc :)

library(dplyr)     # for data manipulation
library(ggplot2)

# LOAD DATA
trees <- read.csv(file = "trees.csv", header = TRUE)

getwd()

head(trees)

#find number of unique trees:

# Count the number of trees for each species

trees.grouped <- group_by(trees, CommonName)    # create an internal grouping structure, so that the next function acts on groups (here, species) separately. 

trees.summary <- summarise(trees.grouped, count = length(CommonName))   # here we use length to count the number of rows (trees) for each group (species). We could have used any row name.

# Alternatively, dplyr has a tally function that does the counts for you!
trees.summary <- tally(trees.grouped)

# But, this creates intermediate objects - gets cluttered.

# We use a pipe to just pass the functions:


# Count the number of trees for each species, with a pipe!

trees.summary <- trees %>%                   # the data frame object that will be passed in the pipe
  group_by(CommonName) %>%    # see how we don't need to name the object, just the grouping variable?
  tally()                     # and we don't need anything at all here, it has been passed through the pipe!

#Specify species and age groups

trees.subset <- trees %>%
  filter(CommonName %in% c('Common Ash', 'Rowan', 'Scots Pine')) %>% 
  group_by(CommonName, AgeGroup) %>% 
  tally()

# Was the same as the first tutorial on piping...






