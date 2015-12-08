library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(dplyr)
library(rmarkdown)
library(descr)
library(gdata)

spyc <- read.csv("spycCleanedAndCoded.csv", header = TRUE, sep = ",")

# The list of valid groups to choose from
groups <- list("All" = "All",
              "Gender" = "Gender",
              "Race" = "Race",
              "School" = "School",
              "City" = "City",
              "Neighborhood" = "Neighborhood")

gender <- list("Male" = "Male",
                "Female" = "Female",
                "Other" = "Other")

race <- list("Black" = "Black",
              "White" = "White",
              "Asian" = "Asian",
              "Native American / Pacific Islander" = "Native",
              "Mixed Race" = "Mixed Race",
              "Latino / Hispanic" = "Latino / Hispanic")

school <- list("Minnehaha Academy" = "Minnehaha Academy",
                "Highland Park Senior High School" = "Highland Park Senior High School",
                "Twin Cities Academy" = "Twin Cities Academy",
                "Como Senior High School" = "Como Senior High School",
                "Central Senior High School" = "Central Senior High School")

city <- list("Apple Valley" = "Apple Valley",
              "Bloomington" = "Bloomington",
              "Burnsville" = "Burnsville",
              "Crystal" = "Crystal",
              "Eagan" = "Eagan",
              "Eden Prairie" = "Eden Prairie",
              "Edina" = "Edina",
              "Falcon Heights" = "Falcon Heights",
              "Inver Grove Heights" = "Inver Grove Heights",
              "Lakeville" = "Lakeville",
              "Maplewood" = "Maplewood",
              "Mendota Heights" = "Mendota Heights",
              "Midway" = "Midway",
              "Minneapolis" = "Minneapolis",
              "Minnetonka" = "Minnetonka",
              "North St. Paul" = "North St. Paul",
              "Oakdale" = "Oakdale",
              "Plymouth" = "Plymouth",
              "Prior Lake" = "Prior Lake",
              "Rosemount" = "Rosemount",
              "Roseville" = "Roseville",
              "South Bloomington" = "South Bloomington",
              "St. Louis Park" = "St. Louis Park",
              "St. Paul" = "St. Paul",
              "Sunfish Lake" = "Sunfish Lake",
              "West St. Paul" = "West St. Paul",
              "Woodbury" = "Woodbury")

neighborhood <- list()

#######################################
# Build the Term Matrix from a Vector #
#######################################

# NOT CURRENTLY Using "memoise" to automatically cache the results

# "group" is the way you want to group by ("Gender","Race")
# "by" is the variable to choose ("Male","Female")
# "query" is Unfairness/DevotedTo by code
# "toRemove" is the words to remove from the query

getTermMatrix <- memoise(function(group, by, query, toRemove) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  #if (!(group %in% groups))
  #    stop("Unknown group")

  # if query is CODED version... pull by ", " as separator rather than " "

  # Recode to our simplified dummy vars.
  if (group == "Gender") {
    group <- "SimpleGender"
  }
  if (group == "Race") {
    group <- "SimpleRace"
  }

  # Need to do a conversion here to force into Filter - add quotes
  by <- paste0("'", by, "'")

  # Filter to get the groups...
  if (group != "All") {
    text <- spyc %>% filter_(paste(group, " == ", by)) %>% select_(as.name(query))
  } else {
    text <- spyc %>% select_(as.name(query))
  }

  # Separate toRemove by commas and remove extra spaces...
  toRemove <- unlist(strsplit(toRemove, split=", "))
  toRemove <- trim(toRemove)


  if (query == "CodedUnfairness" || query == "CodedChangesDevotedTo") {
    text <- unlist(text[ , 1])

    trim <- function (trimmed) gsub("^\\s+|\\s+$", "", trimmed)
    f <- function(s) {
      return(strsplit(as.character(s), "[,]"))
    }

    x <- unlist(sapply(text, f))
    final <- unlist(lapply(x, trim))
    final <- gsub(" ", "-", final)
    myCorpus = Corpus(VectorSource(final))
    myCorpus = tm_map(myCorpus, removeWords,
           c(stopwords("SMART"), "the", "and", "but", "are", "that", "they", "much", "many", "dont", toRemove))

  } else {
    myCorpus = Corpus(VectorSource(text))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,
           c(stopwords("SMART"), "the", "and", "but", "are", "that", "they", "much", "many", "dont", toRemove))
  }

  myDTM = TermDocumentMatrix(myCorpus,
              control = list(minWordLength = 1))

  m = as.matrix(myDTM)

  sort(rowSums(m), decreasing = TRUE)
})
