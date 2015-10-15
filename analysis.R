# Set to working directory of the data...
library(data.table)
library(dplyr)
library(magrittr)
library(ggvis)
library(chron)
library(readr)

# Read in and gander at our data...
spyc <- read.csv("spycRaw.csv", header = TRUE, sep = ",", strip.white = TRUE, na.strings = "NA", row.names=NULL)

spyc %<>% select(-8) # Drop the confusing non column column
names(spyc) <- c("Timestamp", "School", "Race", "Gender", "City", "Unfairness", "ChangesDevotedTo", "Email", "Neighborhood")

spyc$Timestamp[] <- lapply(spyc$Timestamp, as.character)
dtparts = t(as.data.frame(strsplit(spyc$Timestamp, ' ')))
spyc$Timestamp <- chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
str(spyc)

##################
#### CLEANING ####
##################
# Our data is ugly! Tidy, but unclean...
# Aim for technically correct AND consistent (enough)!
# We'll make some choices about how consistent we want things... we asked for open-ended results on purpose to give
# people options for the ways they'd like to represent themselves demographically.

###################
# Clean Timestamp #
###################
# library(lubridate)
# spyc$Timestamp <- mdy(spyc$Timestamp) #Figure out timestamp...

################
# Clean School #
################

# Get rid of any blank stuff we've got in here...
spyc$School <- sub("(^.{0}$)", "N/A", spyc$School, TRUE)

# Function to get rid of our case problem...
caps <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep="", collapse=" ")
}

# Remove cases...
spyc$School <- sapply(spyc$School, caps)

# Transform!
spyc$School <- sub("(^minnehaha.*)", "Minnehaha Academy", spyc$School, TRUE) # Last TRUE refers to case INSENSITIVE matching.
spyc$School <- sub("(^highland.*)", "Highland Park Senior High School", spyc$School, TRUE)
spyc$School <- sub("(^twin cities academy.*)", "Twin Cities Academy", spyc$School, TRUE)
spyc$School <- sub("(^TCA.*)", "Twin Cities Academy", spyc$School, TRUE)
spyc$School <- sub("(^com.*)", "Como Senior High School", spyc$School, TRUE)
spyc$School <- sub("(^central.*)", "Central Senior High School", spyc$School, TRUE)
spyc$School <- sub("(^st. paul.*)", "Central Senior High School", spyc$School, TRUE)

# We don't know what this is...
spyc$School <- sub("(^Yes.*)", "N/A", spyc$School, TRUE)
table(spyc$School)

################
# Clean Gender #
################
# We clean Gender by matching anything that starts with an "M" as Male, "F" as Female, "N/A" as N/A
# The rest, we leave for now - they don't fit cleanly, but that's why we gave people an option.
table(spyc$Gender)
spyc$Gender <- sub("(^m.*)", "Male", spyc$Gender, TRUE) # Last TRUE refers to case INSENSITIVE matching.
spyc$Gender <- sub("(^f.*)", "Female", spyc$Gender, TRUE)
spyc$Gender <- sub("(^gal.*)|(^gir.*)|(^lady.*)|(^woma.*)", "Female", spyc$Gender, TRUE)
spyc$Gender <- sub("(^bo.*)", "Male", spyc$Gender, TRUE)
spyc$Gender <- sub("(^n/a.*)", "NA", spyc$Gender, TRUE)

# Display
spycSimpleGender <- c(Male = sum(spyc$Gender == "Male"), Female = sum(spyc$Gender == "Female"), Other = sum(spyc$Gender != "Male" & spyc$Gender != "Female"))

# Create a dummy code for gender - Male = Male, Female = Female, everything else = Other
spyc %<>% mutate(SimpleGender = ifelse(Gender == "Male", "Male", ifelse(Gender == "Female", "Female", "Other")))

##############
# Clean Race #
##############
table(spyc$Race)

# Get rid of any blank stuff we've got in here...
spyc$Race <- sub("(^.{0}$)|(^-$)", "N/A", spyc$Race, TRUE)

# Remove cases...
spyc$Race <- sapply(spyc$Race, caps)

# MIXED Race first... if anyone has multiple races listed
# Doesn't totally work... misses spaces, but those are a hard case...
spyc$Race <- sub("(\\w+\\s*)([,;/])(\\s*\\w+)", "Mixed Race", spyc$Race, TRUE)

# Use first listed only...
# Problem comes that sorting is done based on order we work in here...
spyc$Race <- sub("(^bla.*)", "Black", spyc$Race, TRUE) # Last TRUE refers to case INSENSITIVE matching.
spyc$Race <- sub("(^white.*)", "White", spyc$Race, TRUE)
spyc$Race <- sub("(^cauca.*)", "White", spyc$Race, TRUE)
spyc$Race <- sub("(^canad.*)", "White", spyc$Race, TRUE)
spyc$Race <- sub("(^euro.*)", "White", spyc$Race, TRUE)
spyc$Race <- sub("(^latin.*)", "Latino", spyc$Race, TRUE)
spyc$Race <- sub("(^spanish.*)", "Hispanic", spyc$Race, TRUE)
spyc$Race <- sub("(^mex.*)", "Hispanic", spyc$Race, TRUE)
spyc$Race <- sub("(^hisp.*)", "Hispanic", spyc$Race, TRUE)
spyc$Race <- sub("(^somali.*)", "Somali", spyc$Race, TRUE)
spyc$Race <- sub("(^native.*)", "Native American / Pacific Islander", spyc$Race, TRUE)
spyc$Race <- sub("(^filipino.*)", "Native American / Pacific Islander", spyc$Race, TRUE)
spyc$Race <- sub("(^mix.*)", "Mixed Race", spyc$Race, TRUE)
spyc$Race <- sub("(^vietnam.*)", "Asian", spyc$Race, TRUE)
spyc$Race <- sub("(^japan.*)", "Asian", spyc$Race, TRUE)
spyc$Race <- sub("(^karen.*)", "Asian", spyc$Race, TRUE)
spyc$Race <- sub("(^asian.*)", "Asian", spyc$Race, TRUE)
spyc$Race <- sub("(^asain.*)", "Asian", spyc$Race, TRUE)
spyc$Race <- sub("(^hmong.*)", "Asian", spyc$Race, TRUE)
spyc$Race <- sub("(^chin.*)", "Asian", spyc$Race, TRUE)
spyc$Race <- sub("(^camb.*)", "Asian", spyc$Race, TRUE)
spyc$Race <- sub("(^mediterranean.*)", "White", spyc$Race, TRUE)
spyc$Race <- sub("(^africa)", "African", spyc$Race, TRUE)
spyc$Race <- sub("(^ethiop.*)", "Ethiopian", spyc$Race, TRUE)
spyc$Race <- sub("(^eritr.*)", "Eritrean", spyc$Race, TRUE)
spyc$Race <- sub("(^afric.*)", "African American", spyc$Race, TRUE)
spyc$Race <- sub("(^afric.*)", "African American", spyc$Race, TRUE)

# Deal with "left overs" -- NEED TO FIX THESE
spyc$Race <- sub("(mexi.*)", "Hispanic", spyc$Race, TRUE)
spyc$Race <- sub("(hisp)", "Hispanic", spyc$Race, TRUE)
spyc$Race <- sub("(chine.*)", "Asian", spyc$Race, TRUE)
spyc$Race <- sub("(middle.*)", "Middle Eastern", spyc$Race, TRUE)

# Display some summaries, add further here...
black <- c("Black","African","African American","Eritrean","Ethiopian","Somali")
latinoHispanic <- c("Latino","Hispanic","Puerto Rican","Peruvian","El Salvadorian")
spycSimpleRace <- c(Black = sum(spyc$Race %in% black), NativeAmerican = sum(spyc$Race == "Native American / Pacific Islander"), White = sum(spyc$Race == "White"), Asian = sum(spyc$Race == "Asian"), "Latino / Hispanic" = sum(spyc$Race %in% latinoHispanic), Other = sum(spyc$Race != "White" & !(spyc$Race %in% latinoHispanic) & (!spyc$Race %in% black) & spyc$Race != "Asian"))

# Dummy code Race by simple...
spyc %<>% mutate(SimpleRace = ifelse(Race %in% black, "Black", ifelse(Race == "White", "White", ifelse(Race == "Native American / Pacific Islander", "Native", ifelse(Race == "Asian", "Asian", ifelse(Race %in% latinoHispanic, "Latino / Hispanic", ifelse(Race == "Mixed Race", "Mixed Race", "Other")))))))
table(spyc$SimpleRace)

##############
# Clean City #
##############
table(spyc$City)
# First let's get rid of our case problem...
spyc$City <- sapply(spyc$City, caps)

spyc$City <- sub("(^.{0}$)", "N/A", spyc$City, TRUE)
spyc$City <- sub("(^west.*)", "West St. Paul", spyc$City, TRUE)
spyc$City <- sub("(^north.*)", "North St. Paul", spyc$City, TRUE)
spyc$City <- sub("(^falcon.*)", "Falcon Heights", spyc$City, TRUE)

######################
# Clean Neighborhood #
######################
table(spyc$Neighborhood)

# Can I do a fuzzy match with St. Paul and Minneapolis neighborhoods?

spyc$Neighborhood <- sub("(^.{0}$)", "N/A", spyc$Neighborhood, TRUE)
spyc$Neighborhood <- sapply(spyc$Neighborhood, caps)

####################
# Save Cleaned CSV #
####################

write.table(spyc, file = "spycCleaned.csv", sep = ",")


################
#### CODING ####
################
# Is it possible to fuzzy match some codes? Or should this be a human process...

# Either way, for the human process, the data needs to be exported, uploaded to Google Drive, and coded.
# Then the data needs to be saved as a new version, and pulled in.
spyc <- read.csv("spycCleanedAndCoded.csv", header = TRUE, sep = ",", na.strings = "NA")
str(spyc)

##################
#### GROUPING ####
##################
# We're going to want to build some groups
# Change to the CODED names of columns here...

# How many emails did we get?
spyc %>% select(Email) %>% filter(Email != "") %>% count()

# View Unfairness / ChangesDevotedTo by School
spyc %>% group_by(School) %>% select(Unfairness, ChangesDevotedTo)

# View Unfairness / ChangesDevotedTo by Gender
spyc %>% group_by(Gender) %>% select(Unfairness, ChangesDevotedTo)

# View Unfairness / ChangesDevotedTo by Race
spyc %>% group_by(Race) %>% select(Unfairness, ChangesDevotedTo)

# View Unfairness / ChangesDevotedTo by City
spyc %>% group_by(City) %>% select(Unfairness, ChangesDevotedTo)

# View Unfairness / ChangesDevotedTo by Neighborhood
spyc %>% group_by(Neighborhood) %>% select(Unfairness, ChangesDevotedTo)

#####################
#### VISUALIZING ####
#####################

##############
# Crosstabs? #
##############

# Omg - library(descr), crosstab(x,y) is awesome

# These give us a sense of how representative our data is...

# Race by Gender
spyc %>% ggvis(~Race, ~Gender) %>% layer_points()
# Race by School
# Gender by School
# Gender by City
# Race by City
# Gender by Neighborhood
# Race by Neighborhood

######################
# Grouped Histograms #
######################

# Use Shiny and build with text inputs that sort by Group & by Unfairness/ChangesDevotedTo

###############
# Word Clouds #
###############

# Can we build Wordles from R? Yes, yes we can :)
# Can we do it with Shiny so it can go into our data dashboard...?
library(tm)
library(wordcloud)

# Build school word cloud for Unfairness
wordcloud(spyc$Unfairness)

# Build school word cloud for ChangesDevotedTo
wordcloud(spyc$ChangesDevotedTo)

# Build more specific word cloud based on grouping inputs from Shiny...
# see server.R, ui.R

####################
# Neighborhood Map #
####################

# Map populations data is gathered from by Neighborhood / City
# Map Unfairnesses by Neighborhood / City
# Map ChangesDevotedTo by Neighborhood / City


#################
#### LAUNCH! ####
#################

# Have a Shiny data dashboard that allows exploration of the Histograms, Word Clouds, and Maps...
# Prop it up on a server to allow Commissioners to gander at over the weekend and on Monday...

# Questions to ask:
# - Are we representing the people we want to represent? If we need to gather more data... what do we gather?
# - Why does X come up?
# - For whom does X come up? (Demographics)
# - What does this tell us about who?
# - What do we do with what it tells us about X group?
# - Where are matches between what the data says and our values / commitments / passion?
# - Where are disconnects? Why do we see those disconnects?
# - Where there seem to be disconnects, are there ways we can use this information to attract / lure others into our project?
