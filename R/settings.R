# Specific libraries
#require(shinythemes)
#require(shinycssloaders)
#require(shinyjs)
#require(shiny)
#require(data.table)
#require(parallel)
#require(plotly)
#require(ggplot2)
#require(dplyr)

###################
# INPUT VARIABLES #
###################

maxNumberOfCores = parallel::detectCores()

# Settings For MachineLearneR

SamplingPercentage = 15
numberOfValuesDiscreteVariable = 5

percentUniqueCutoff = 5

#UoA = c("Row", "Col")
#UoA = c("wellLocation")
#UoA = c("WellName")
#UoA = c('RowIdNumber', 'ColIdNumber')

# Preprocessing
minSamplingSize = 10000
maxSamplingSize = 100000


selectedTransformationBoundary = 0.001

ClassVar = 'reagentCategories' # argument class variable
NormalizeVar = 'plateName'

#Scaling Method
scalingMethod = 'robustZscore'

# Outlier settings
SDs = 8

# Missing Data row wise
rowWiseMissingPercentage = 100

imputationMethod = 'CWD'