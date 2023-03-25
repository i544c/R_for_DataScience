# R Programming - John Hopkins University (Coursera)
# Pratice - Air Pollution
# My IDE for Data Science: DataSpell - JetBrains

# R Version: (My version: 4.2.2)
print(R.version.string)

# Working directory:
getwd()

#Packages install:
library(plyr)
cat("\n") ## 'cat("\n")' <- skip a row! ##

# Function 1: Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.

pollutantmean <- function(directory, pollutant, id = 1:332) {

  # list csv files in directory and subdirectories:
  file_list <- list.files(directory, full.names = TRUE, recursive = TRUE)

  # extracts data from monitors according to specified id:
  data_list <- lapply(file_list, function(x) {
    data <- read.csv(x)
    data[data$ID %in% id, ]
  })

  # creates a data frame with monitor data:
  data_all <- do.call(rbind, data_list)

  # averages the specified pollutant, ignoring NAs:
  mean(data_all[[pollutant]], na.rm = TRUE)
}

# Function 2: Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases.

complete <- function(directory, id = 1:332) {
  # Creates a vector of zeros to store the number of complete cases for each file:
  nobs <- rep(0, length(id))
  # Creates an array to store the name of each file:
  fileNames <- character(length(id))
  # Loop over each given file ID:
  for (i in seq_along(id)) {
    # Creates file name based on ID:
    fileName <- paste0(directory, "/", formatC(id[i], width = 3, flag = "0"), ".csv")
    # Reads the CSV file:
    dat <- read.csv(fileName)
    # Counts the number of complete cases (no missing values):
    nobs[i] <- sum(complete.cases(dat))
    # Store the file name:
    fileNames[i] <- fileName
  }
  # Returns a data frame with the file name and the number of complete cases for each file:
  data.frame(id = id, nobs = nobs, fileName = fileNames)
}

# Function 3: Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.

corr <- function(directory, threshold = 0) {
  # loads the "plyr" package:
  require(plyr)

  # get list of files in directory:
  file_list <- list.files(directory, full.names = TRUE)

  # initializes a vector to store the correlations:
  cor_list <- numeric()

  # for each data file in the list:
  for (file in file_list) {
    # lê o arquivo de dados
    data <- read.csv(file)

    # calculates the number of cases completely observed:
    nobs <- sum(complete.cases(data))

    # if the number of completed cases is greater than the threshold:
    if (nobs > threshold) {
      # calculates the correlation between sulfate and nitrate:
      cor_val <- cor(data$sulfate, data$nitrate, use = "complete.obs")

      # adds the correlation to the list of correlations:
      cor_list <- c(cor_list, cor_val)
    }
  }

  # returns the correlation vector:
  return(cor_list)
}

########### >>>>>>>>>> TESTING THE FUNCTIONS <<<<<<<<<< ##########
print("Funtion test 1:")
  pollutantmean("specdata", "sulfate", 1:10)
  pollutantmean("specdata", "nitrate", 70:72)
  pollutantmean("specdata", "nitrate", 23)
  cat("\n") ## 'cat("\n")' <- skip a row! ##

print("Funtion test 2:")
  complete("specdata", 1)
  cat("\n")
  complete("specdata", c(2, 4, 8, 10, 12))
  cat("\n")
  complete("specdata", 30:25)
  cat("\n")
  complete("specdata", 3)
  cat("\n")

print("Funtion 2 and 3 test:")
  cr <- corr("specdata", 150)
    head(cr)
    summary(cr)
    cat("\n")
  cr <- corr("specdata", 400)
    head(cr)
    summary(cr)
    cat("\n")
  cr <- corr("specdata", 5000)
    summary(cr)
    length(cr)
    cat("\n")
  cr <- corr("specdata")
    summary(cr)
    length(cr)