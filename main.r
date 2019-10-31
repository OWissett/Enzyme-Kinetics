########################
##      Libraries     ##
########################

## Nice little function that I found that will install packages if they are not
## installed.

{ 
  ipak <- function(pkg)
    {
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  libs <- c(
    "ggplot2",
    "ggthemes",
    "gridExtra"
  )
  
  ipak(libs)   ## Installs and loads all packages listed in "libs".
  libs <- NULL ##Clear the memory.
}

####################
##    Load Data   ##
####################

##################
my.detectReplicates <- function()
{
  numberOfReps <- list()
  for(i in 1:length(dat))
  {
    numberOfReps[[i]] <- length(dat[[i]]) - 1 ##Assumes that table only contains reps
                                         ##and concentrations. 
  }
  return(numberOfReps)
}

dat <- list() ##Define dat to have globabl scope
reps <- list()

setwd("~/Enzyme-Kinetics/Kinetic_Data")
files <- list.files(pattern = "\\.csv$")
#Load all CSVs
dat <- lapply(files, read.csv)

reps <- my.detectReplicates()
print(paste("Number of replicates detected for each set: ",
            reps))

while (1 == 1)
  ##Infinite response loop
{
  response <-
    readline(prompt = "Confirm number of replicates are correct (y/n) ")
  if (response == "Y" | response == "y")
  {
    break
  }
  else if (response == "N" | response == "n")
  {
    reps <- my.manualReplicateEntry()
    break
  }
  else
  {
    print("Please enter a valid resonse! e.g. y or n")
  }
}

my.manualReplicateEntry <- function()
{
  
}

######################

##Creates a new data frame that is with conc and average rate
my.average <- function()
{
  ## Create a new list with a 1 to 1 correspondance to dat
  mat_list <- lapply(dat, data.matrix) 
  
  ## Map dat as a matrix to mat_list
  Map(function(m ,p) {m[1:nrow(p), 1:ncol(p)]=p; m}, dat, mat_list)
  
  ## Remove the concentrations from the matrix leaving the replicates
  ## mat_list <- lapply(mat_list, function(x) return(x[,-1]))
  
  RetDFs <- lapply(mat_list, function(x){
    temp <- x[,-1]
    means <- rowSums(temp) / ncol(temp)
    df <- data.frame(conc = x[,1], rate = means)
    return(df)
    })
  
  return(RetDFs)
 
}


my.restructure <- function()
{
  tempList <- lapply(dat, function(x){
    tempDF <- data.frame(conc = double(),
                         rate = double(),
                         trial = factor())
    repNum <- ncol(x) - 1
    for(columnIndex in 1:repNum)
    {
      tempDF.new <- data.frame(conc = x[, 1],
                               rate = x[, columnIndex + 1],
                               trial = columnIndex)
      tempDF <- rbind(tempDF, tempDF.new)
    }
    return(tempDF)
  })
  return(tempList)
}

########################
##  Lineweaver - Burk ##
########################
my.recipDat <- function()
{
  ## Create a list of restructured dataframes
  rawDat <- my.restructure()
  
  recipDat <- lapply(rawDat, function(x)
    {
    tempDF <- data.frame(recip.conc = sapply(x[[1]], function(y) 1/y),
                       recip.rate = sapply(x[[2]], function(y) 1/y),
                       trial = x[[3]])
    return(tempDF)
  })
  return(recipDat)
}

my.LBModel <- function()
{
  ## Create a reciprocal data set
  recipDat <- my.recipDat()
  
  ## Generic function for the glm
  rateModel <- function(x)
  {
    glm(data = x,
        formula = recip.rate ~ recip.conc,
        family = gaussian)
  }
  
  LBmodels <- lapply(recipDat, rateModel)
  return(LBmodels)
}

my.LBPlot <- function()
{
  
}