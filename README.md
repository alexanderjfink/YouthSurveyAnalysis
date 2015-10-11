# YouthSurveyAnalysis
R Analysis and Shiny Dashboard for Youth Survey

## Data cleaning
Use a base raw dataset taken from the survey and run through the analysis.R script.

This script will take the raw dataset as-is, clean it in ways we agreed upon as data analyzers, then re-save it as a "cleaned" version of the document. Once cleaned, the document can be coded to provide wordclouds based on the coded version.

## Data Dashboard
After processing the data, a file should exist entitled spycCleanedAndCoded.csv. This is the data file which should contain a cleaned and coded version of the file. The dashboard will operate based on this file.

To launch a local version of the app, open R, then type:
  library(shiny)
  runApp()
