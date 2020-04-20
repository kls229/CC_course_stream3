##%######################################################%##
#                                                          #
####                Loops and functions                 ####
#                                                          #
##%######################################################%##
# Katie Spencer
# 20.04.20
# https://ourcodingclub.github.io/tutorials/funandloops/index.html

# Often need to run code multiple time wiht just the data input being changed - could
# copy and paste but this increases chance of mistakes and lengthens script.
getwd()
setwd("C:/Users/katie/OneDrive/PhD/My_Stats_from_Scratch/CC_course_stream3-master/CC_course_stream3/02_Loops_and_functions")
trees_bicuar <- read.csv("trees_bicuar.csv")
trees_mlunguya <- read.csv("trees_mlunguya.csv")

head(trees_bicuar)
str(trees_bicuar)

#The basic syntax for creating a function looks like:

example.fn <- function(x, y){ #We are assigning the function to an object called example.fn
  # Perform an action using x and y (arguments)
  x + y
}

example.fn(x=1, y =2) # output of 3

# Function that calculates the basal area of each stem in cm

basal.area <- function(x){ # Just one input (X)
  (pi*(x)^2)/40000
}

basal.area(x = trees_bicuar$diam)

# Can add more arguments by using (...) operator, e.g to combine basal areas from multiple sites

basal.area <- function(...){
  (pi*c(...)^2)/40000
}

basal.area(trees_bicuar$diam, trees_mlunguya$diam)

# Assign to a new object

trees_bicuar$ba <- basal.area(dbh = trees_bicuar$diam) # New column in trees_bicuar


## Functions in loops ----

# Can use functions with loops; loops run the same operation on a group of objects

# Come as for()(set number of actions) and while() (set number of times)

# for() loop iterates through a number of items (in a list) and performs actions on them

# Basic syntax for the for() loop:


for(i in list){
  # PERFORM SOME ACTION
}

# For example, if theres multiple field sites all with different data frames,
# to calculate basal areas for stems at both sites:


trees_bicuar$ba <- basal.area(trees_bicuar$diam)
trees_mlunguya$ba <- basal.area(trees_mlunguya$diam)

# Fine for a couple of sites, but need a loop if there were lots:


trees <- list("trees_bicuar" = trees_bicuar, "trees_mlunguya" = trees_mlunguya)

# Each element in the list is a dataframe

# List items within a list can be accessed using double square brackets, e.g. trees[[1]] selects the first list item, the dataframe for trees_bicuar.

# Construct for loop:

for( i in 1:length(trees) ){ #  creates a sequence of integers from 1 to the length of the list (trees), so in this case the sequence will be 1, 2 as there are two list items
  trees[[i]]$ba <- basal.area(trees[[i]]$diam)
}

#  1: length creates a sequence of integers from 1 to the length of the list (trees), so in this case the sequence will be 1, 2 as there are two list items
# i will take each value of 1:length(trees) in turn, then run the actions in the curly brackets once

# Main body: creates a new column in each dataframe in the list, then runs the function basal.area() using the diam column from the same dataframe as the input
# So, the first time the loop runs, it will create a new column called ba in the first list item in trees, trees[[1]].

# But data is often not split into different dataframes...

# e.g want the stem basal area for each year, which is all in one column

# First, seperate trees_mlunguya into a list of dataframes, each based on the contents of the year column:

trees_mlunguya_list <- split(trees_mlunguya, trees_mlunguya$year) # Could split sun bears by site, year, habitat etc..

# Then run a for() loop to fill an empty list with the mean basal area of each year

# Create an empty list
mean_ba_list <- list()

for( i in 1:length(trees_mlunguya_list) ){
  ba <- basal.area(trees_mlunguya_list[[i]]$diam)
  mean_ba <- mean(ba)
  year <- mean(trees_mlunguya_list[[i]]$year)
  dat <- data.frame(year, mean_ba)
  mean_ba_list[[i]] <- dat
}

# Could also create this function:

ba.mean.year <- function(dbh, year){
  data.frame(
    mean_ba = mean(basal.area(dbh)),
    year = mean(year)
  )    
}

ba.mean.year(trees_mlunguya_list[[1]]$diam, trees_mlunguya_list[[1]]$year)

# And then use this new function for the loop

for( i in 1:length(trees_mlunguya_list) ){
  mean_ba_list[[i]] <- ba.mean.year(
    trees_mlunguya_list[[i]]$diam,
    trees_mlunguya_list[[i]]$year)
}


## Functions with lapply() family----

# Loops can be slow because R likes to store everything as new objects with each iteration

# Alternatively, lapply() runs operations on lists of items, similar to the for() loops above. 

# To replicate the previous for() loop, where we calculated the mean basal area per year in trees_mlunguya, can run:

lapply(trees_mlunguya_list, function(x){ba.mean.year(dbh = x$diam, year = x$year)})

# Object to be iterated -> Defines unamed function (x will be replaced by each list in the item as
# lapply() iterates -> code in curly brackets is unamed function with custom function ba.mean.year())

# Find the mean height of trees in trees_bicuar for each taxonomic family

# First, create a list of vectors of height (rather than dataframes) where each list is a different family of species.

bicuar_height_list <- split(trees_bicuar$height, trees_bicuar$family) #Split height by family 

#Then run lapply()

lapply(bicuar_height_list, mean, na.rm = TRUE)


# Don't need curly brackets or functions - just passed mean as the second argument

# sapply() simplifies the output of lapply to a vector, withe elements in the vector named according to the name of items in the orginal list


sapply(bicuar_height_list, mean, na.rm = TRUE)


## Conditional statements ----

# Allows code to be run depending on whether certain conditions are met

# Trees were measured with two apparatus, need to adjust height estimates usng ifelse()

# ifelse looks for logical true/false conditions then performs actions based on the outcome

# Constructs a function with an ifelse() statement to calculate Lorey’s mean height for the Bicuar plots


stick.adj.lorey <- function(height, method, ba){
  height_adj <- ifelse(method == "stick", height + 1, round(height, digits = 1))
  
  lorey_height <- sum(height_adj * ba, na.rm = TRUE) / sum(ba, na.rm = TRUE)
  
  return(lorey_height)
}

#Then we can test the function on each plot using lapply() like we did before:

trees_bicuar_list <- split(trees_bicuar, trees_bicuar$plotcode)

lapply(trees_bicuar_list, function(x){stick.adj.lorey(height = x$height, method = x$height_method, ba = x$ba)})


## Write a loop for multiple graphs ----

LPI <- read.csv("LPI_data_loops.csv")

#Scatter plot to examine how Griffon vulture populations have changed between 1970 and 2014 in Croatia and Italy:

vulture <- filter(LPI, Common.Name == "Griffon vulture / Eurasian griffon")
vultureITCR <- filter(vulture, Country.list == c("Croatia", "Italy"))

(vulture_scatter <- ggplot(vultureITCR, aes(x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                              # Changing point size
    geom_smooth(method = lm, aes(fill = Country.list)) +                # Adding a linear model fit and colour-coding by country
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B"),               # Adding custom colours
                        labels = c("Croatia", "Italy")) +               # Adding labels for the legend
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),       # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                # Removing the background grid lines                
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),           # Adding a 0.5cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),              # Setting the font for the legend text
          legend.title = element_blank(),                                      # Removing the legend title
          legend.position = c(0.9, 0.9)))               # Setting the position for the legend - 0 is left/bottom, 1 is top/right

#Turn a theme into a function

theme.my.own <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.title = element_blank(),                              
          legend.position = c(0.9, 0.9))
}

# Now make same plot but use theme instrad of long code again:


(vulture_scatter <- ggplot(vultureITCR, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                                
    geom_smooth(method = lm, aes(fill = Country.list)) +                    
    theme.my.own() +                                                    # Adding our new theme!
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               
    scale_colour_manual(values = c("#EE7600", "#00868B"),               
                        labels = c("Croatia", "Italy")) +                 
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear"))


# More plots...

# Filter to just the UK

LPI.UK <- filter(LPI, Country.list == "United Kingdom")

# Pick 4 species and make scatterplots with linear model fits that show how the population has varied through time
# Careful with the spelling of the names, it needs to match the names of the species in the LPI.UK dataframe

house.sparrow <- filter(LPI.UK, Common.Name == "House sparrow")
great.tit <- filter(LPI.UK, Common.Name == "Great tit")
corn.bunting <- filter(LPI.UK, Common.Name == "Corn bunting")
reed.bunting <- filter(LPI.UK, Common.Name == "Reed bunting")
meadow.pipit <- filter(LPI.UK, Common.Name == "Meadow pipit")



(house.sparrow_scatter <- ggplot(house.sparrow, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "House sparrow"))

(great.tit_scatter <- ggplot(great.tit, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "Great tit"))

(corn.bunting_scatter <- ggplot(corn.bunting, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "Corn bunting"))

(meadow.pipit_scatter <- ggplot(meadow.pipit, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "Meadow pipit"))

# Arrange all four in a  grid using gridExtra package

gridExtra

panel <- grid.arrange(house.sparrow_scatter, great.tit_scatter, corn.bunting_scatter, meadow.pipit_scatter, ncol = 2)
ggsave(panel, file = "Pop_trend_panel.png", width = 10, height = 8)
dev.off() # to close the image


# But still very cody... we can tell R to make the same type of graph for every species instead

Sp_list <- list(house.sparrow, great.tit, corn.bunting, meadow.pipit)

for (i in 1:length(Sp_list)) {                                    # For every item along the length of Sp_list we want R to perform the following functions
  data <- as.data.frame(Sp_list[i])                               # Create a dataframe for each species
  sp.name <- unique(data$Common.Name)                             # Create an object that holds the species name, so that we can title each graph
  plot <- ggplot(data, aes (x = year, y = abundance)) +               # Make the plots and add our customised theme
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = sp.name)
  
  ggsave(plot, file = paste(sp.name, ".pdf", sep = ''), scale = 2)       # save plots as .pdf, you can change it to .png if you prefer that
  
  print(plot)                                                      # print plots to screen
}










