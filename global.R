# Load packages
library(igraph)

# Load the data
InfoFarmPeople <- read.delim("data/InfoFarmPeople.txt")
InfoFarmLinks  <- read.delim("data/InfoFarmLinks.txt")

# Invert the daysTogether into weight
InfoFarmLinks$weight <- 1/InfoFarmLinks$daysTogether