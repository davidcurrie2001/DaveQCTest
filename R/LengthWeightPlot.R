# A simple Length-Weight plot
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

LengthWeightPlot <- function(BaseURL, ConfigObject, SurveyName, Species){

  # Ensure we have the right packages available - can't run the function otherwise
  if (!require(jsonlite)){
    install.packages("jsonlite")
    if (!require(jsonlite)){
      return("Error- could not install jsonlite")
    }
  }

  if (!require(RCurl)){
    install.packages("RCurl")
    if (!require(RCurl)){
      return("Error- could not install RCurl")
    }
  }


  # Build the URL

  # (we need to URLencode the config object before we add it into the URL)
  myURL <- paste(BaseURL,"ConfigObject=",URLencode(ConfigObject, reserved=TRUE), sep="")

  # These are the parameters we can pass in to the GetBioData at the moment
  MySurveyYear <- NA
  MySurveyName <- SurveyName
  MyNumberOfRows <- NA
  MySpecies <- Species

  # Check whether the parameters need to be added to the URL (i.e. they are not NA) - add them to the URL if required
  if(!is.na(MySurveyYear)){
    myURL <- paste(myURL,"&SurveyYear=",MySurveyYear, sep="")
  }
  if(!is.na(MySurveyName)){
    myURL <- paste(myURL,"&SurveyName=",MySurveyName, sep="")
  }
  if(!is.na(MySpecies)){
    myURL <- paste(myURL,"&Species=",MySpecies, sep="")
  }
  if(!is.na(MyNumberOfRows)){
    myURL <- paste(myURL,"&NumberOfRows=",MyNumberOfRows, sep="")
  }


  ## Now try and get the current data!

  # fromJSON shoudl identify that myURL is a URL and try to fetch data from it - this didn't always work though
  # (possibly due to the length of the URL?) so we can use the RCurl getURL function to fetch the data before trying to parse it
  # from the JSON format
  myCurrentData <- fromJSON(getURL(myURL))

  # check we've got soem data returned
  #head(myCurrentData)


  ## Now get the historical data

  # Build the URL

  # (we need to URLencode the config object before we add it into the URL)
  myURL <- paste(BaseURL,"ConfigObject=",URLencode(ConfigObject, reserved=TRUE), sep="")

  # These are the parameters we can pass in to the GetBioData at the moment
  MySurveyYear <- NA
  MySurveyName <- NA
  MyNumberOfRows <- NA
  MySpecies <- Species

  # Check whether the parameters need to be added to the URL (i.e. they are not NA) - add them to the URL if required
  if(!is.na(MySurveyYear)){
    myURL <- paste(myURL,"&SurveyYear=",MySurveyYear, sep="")
  }
  if(!is.na(MySurveyName)){
    myURL <- paste(myURL,"&SurveyName=",MySurveyName, sep="")
  }
  if(!is.na(MySpecies)){
    myURL <- paste(myURL,"&Species=",MySpecies, sep="")
  }
  if(!is.na(MyNumberOfRows)){
    myURL <- paste(myURL,"&NumberOfRows=",MyNumberOfRows, sep="")
  }


  # fromJSON shoudl identify that myURL is a URL and try to fetch data from it - this didn't always work though
  # (possibly due to the length of the URL?) so we can use the RCurl getURL function to fetch the data before trying to parse it
  # from the JSON format
  myHistoricalData <- fromJSON(getURL(myURL))

  # check we've got soem data returned
  #head(myHistoricalData)


  # simple plot
  plot(myHistoricalData$Length, myHistoricalData$Weight, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), pch=20, xlab="Length", ylab="Weight")
  title(main=paste("QC Plot for",Species,"in",SurveyName, sep=" "))
  points(myCurrentData$Length, myCurrentData$Weight,  col = "black", bg="red", pch=24)
  legend("topleft", legend=c("Current Data", "All Data"), col=c("red","black"), pch=c(24,20))


}
