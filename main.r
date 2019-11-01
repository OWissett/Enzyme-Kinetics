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
  ##Not sure I need this
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

#####REDUNDANT CODE##########
# 
# my.removeZero <- function()
# {
#   ## Create a list of matrices to contain
#   mat_list <- lapply(dat, data.matrix)
# 
#    ## Map dat as a matrix to mat_list
#   Map(function(m ,p) {m[1:nrow(p), 1:ncol(p)]=p; m}, dat, mat_list)
# 
#   RetDFs <- lapply(mat_list, function(x){
#     temp <- x[,-1] ##Create a temp matrix only containing rates
#     meanVector <- rowSums(temp) / ncol(temp) ##take an average of each row of the matrix
#     meanMatrix <- data.frame(conc = x[,1], ##x[,1] is the concentrations
#                              T1 = meanVector,
#                              T2 = meanVector,
#                              T3 = meanVector)
# 
#     replace <- function(y)
#     {
#       if(0 %in% y){
#         for(index in 1:length(y))
#         {
#           y[[index]] <-
#         }
#       }
#     }
# 
#     apply(x, replace)
# 
#     return(x)
#     })
# 
#   return(RetDFs)
# 
# }
#########################

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
                               trial = factor(columnIndex))
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
    tempDF <- data.frame(
      recip.conc = sapply(x[[1]], function(y)
        1 / y),
      recip.rate = sapply(x[[2]], function(y)
        1 / y),
      trial = x[[3]]
    )
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

my.LBModelCall <- function(dataSet)
{
  model <- glm(data = dataSet,
      formula = recip.rate ~ recip.conc,
      family = gaussian)

  return(model)
}

my.getStatLB <- function(x)
{
  ty.intercept <- coef(my.LBModelCall(x))[[1]] ## y.intercept = 1/Vmax
  tgrad <- coef(my.LBModelCall(x))[[2]]        ## Grad = Km/Vmax
  tVmax <- 1 / ty.intercept
  tKm <- tVmax * tgrad
  tx.intercept <- -1 / tKm
  df <- data.frame(
    y.intercept = ty.intercept,
    x.intercept = tx.intercept,
    grad = tgrad,
    Km = tKm,
    Vmax = tVmax
  )
  return(df)
}

my.LBPlot <- function()
{
  wiggleFactor <- 1
  
  ##Create a list of dataframes containing the reciprocal data set.
  recipDat <- my.recipDat()
  
  plots <- lapply(recipDat, function(x) {
    gg <- ggplot(data = x, aes(x = recip.conc, y = recip.rate)) +
      geom_point(shape = x$trial) +
      geom_smooth(method = "glm",
                  se = F,
                  fullrange = T) +
      labs(title = "Lineweaver-Burk Plot",
           x = "1/[S]",
           y = "1/v") +
      theme_classic() +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme(axis.line = element_blank()) +
      xlim(my.getStatLB(x)$x.intercept * wiggleFactor, NA)
    
  })
  do.call(grid.arrange, plots)
}

##########################
##    Michaelis-Menten  ##
##########################

my.recipDatCall <- function(dataset)
{
  tempDF <- data.frame(recip.conc = sapply(dataset[[1]], function(y) 1/y),
                       recip.rate = sapply(dataset[[2]], function(y) 1/y),
                       trial = dataset[[3]])
  return(tempDF)
}

my.MMModel <- function()
{
  ## Create a restructured data frame list
  rawDat <- my.restructure()
  
  ## Function that returns an NLS MM model using the LB stats to find start vals
  rateModel <- function(x)
  {
    ## Create an LB stat datafram for current dataset
    lbStat <- my.getStatLB(my.recipDatCall(x))
    
    ## Create an MM model using NLS and the LB start table
    nls(rate ~ (Vmax * conc) / (Km + conc),
        start = list(Vmax = lbStat$Vmax, 
                     Km = lbStat$Km),
        data = x)
  }
  
  MMmodels <- lapply(rawDat, rateModel)
  return(MMmodels)
  
}

my.MMModelCall <- function(x)
{
  ## Create an LB stat datafram for current dataset
  lbStat <- my.getStatLB(my.recipDatCall(x))
    
  ## Create an MM model using NLS and the LB start table
  model <- nls(rate ~ (Vmax * conc) / (Km + conc),
      start = list(Vmax = lbStat$Vmax, 
                     Km = lbStat$Km),
      data = x)
  
  return(model)
}

my.MMPlot <- function()
{
  ## Create a dataset of raw data
  rawDat <- my.restructure()
  
  ## Obtain models
  models <- my.MMModel()
  
  plots <- lapply(rawDat, function(x){
    gg <- ggplot(data = x, aes(y = rate, x = conc)) +
      geom_point(aes(shape = x$trial)) +
      geom_smooth(method = "nls",
                  formula = y ~ (Vmax * x) / (Km + x),
                  method.args = list(start = c(Vmax = coef(my.MMModelCall(x))[[1]],
                                               Km = coef(my.MMModelCall(x))[[2]])),
                  se = F) +
      theme_classic() +
      labs(title = "Michaelis-Menten Plot",
           x = "[S]",
           y = "Rate") 
  })
  
  do.call(grid.arrange, plots)
}


## Inhibitor stuff

my.InhibitorRestructure <- function()
{
  restrDat <- my.restructure()
  restrDat$inhib
  restrDat.new <- data.frame(conc = double(),
                             rate = double(),
                             trial = factor())
  inhibConc <- c()
  
  for(index in 1:length(restrDat))
  {
    inhibConc[[index]] <- readline(prompt = "Enter inhibitor conc: ")
    for(index2 in 1:length(restrDat[[index]]))
    {
      restrDat[[index]]$inhib[[index2]] <- inhibConc[[index]]
    }
    restrDat.new <- rbind(restrDat.new, restrDat[[index]])
  }
  
  return(restrDat.new)
}

my.InhibtorLBPlot <- function()
{
  ## Create a dataframe containing all data, indexed by trial and inhibitor
  iDat <- my.InhibitorRestructure()
  inhibList <- list()
  
  #iDat$inhib <- factor(iDat$inhib)

  tempDF <- data.frame(
      recip.conc = sapply(iDat[[1]], function(y)
        1 / y),
      recip.rate = sapply(iDat[[2]], function(y)
        1 / y),
      trial = iDat[[3]],
      inhib = iDat[[4]])
  
  recip.plot <- ggplot(data = tempDF, 
                       aes(x = recip.conc, 
                           y = recip.rate, 
                           colour = inhib)) +
    geom_point(shape = iDat$trial) +
    geom_smooth(method = "glm",
                se = F,
                fullrange = T) +
    labs(title = "Lineweaver-Burk Plot",
         x = "1/[S]",
         y = "1/v") +
    theme_classic() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    theme(axis.line = element_blank()) #+
    #xlim(my.getStatLB(iDat)$x.intercept, NA)
  
  do.call(grid.arrange, recip.plot)
  ##################
  # ldat <- list()
  # ldat[[1]]<- subset(iDat, inhib == 0.5)
  # ldat[[2]]<- subset(iDat, inhib == 1)
  # ldat[[3]]<- subset(iDat, inhib == 0)
  # 
  # models <- list()
  # 
  # for(i in 1:3)
  # {
  #   models[[i]] <- glm(data = ldat[[i]],
  #                      formula = recip.rate ~ recip.conc,
  #                      family = gaussian)
  #   summary(models[[i]])
  #   
  # }
  
  
  ## Finish later
  
  # for(index in 1:level)
  # {}
 
}
