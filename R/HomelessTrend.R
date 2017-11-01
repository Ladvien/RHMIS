#' Merges two HMIS CSV sets.
#'
#' @param string path to folder contaning first CSV set.
#' @param string path to folder contaning second CSV set.
#' @param string path to output where merged CSVs will be written.
#' @param boolean save in feather file format.  Default is false.
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' mergeHmisCSVs('/Users/user/local/CSV_2016', 
#'               '/Users/user/local/CSV_2016',
#'               '/Users/user/local/CSV_2016_and_2017')
#' mergeHmisCSVs('/Users/user/local/CSV_2016', 
#'               '/Users/user/local/CSV_2016',
#'               '/Users/user/local/CSV_2016_and_2017'
#'                feather = TRUE)

trendActivelyHomeless <- function(allDataPath, 
                                  dropboxPath, 
                                  interval = "week",
                                  startDate = "", 
                                  servicesThreshold = 0
                                  ){
  
  ##########################
  # Paths                  #
  ##########################
  hmisFunctionsFilePath <- paste(dropboxPath, "/RScripts/HMIS_R_Functions.R", sep ="")
  primaryPersonalIDFunctionPath <- paste(dropboxPath, "RScripts/RecordLinkInR.R", sep = "")
  outputPath <- paste(dropboxPath, "Output", sep = "")
  
  #####################
  # Load Sources      #
  #####################
  source(hmisFunctionsFilePath)
  source(primaryPersonalIDFunctionPath)
  
  #####################################
  # SSN<->PersonalID Annualized Count #
  #####################################
  
  # Load all the data
  setwd(allDataPath)
  
  client <- loadClient(allDataPath)
  
  primaryPersonalIDs <- getPrimaryPersonalID(client)
  primaryPersonalIDs <- sqldf("SELECT PrimaryPersonalID, PersonalID FROM primaryPersonalIDs")
  
  client <- sqldf("SELECT a.PrimaryPersonalID, b.* FROM primaryPersonalIDs a LEFT JOIN client b ON a.PersonalID=b.PersonalID")
  client <- within(client, rm(PersonalID))
  colnames(client)[1] <- "PersonalID"
  client <- unique(client)
  
  enrollment <- loadEnrollment(allDataPath)
  enrollment <- sqldf("SELECT a.PrimaryPersonalID, b.* FROM primaryPersonalIDs a LEFT JOIN enrollment b ON a.PersonalID=b.PersonalID")
  enrollment <- within(enrollment, rm(PersonalID))
  colnames(enrollment)[1] <- "PersonalID"
  enrollment <- unique(enrollment)
  
  exit <- loadExit(allDataPath)
  exit <- sqldf("SELECT a.PrimaryPersonalID, b.* FROM primaryPersonalIDs a LEFT JOIN exit b ON a.PersonalID=b.PersonalID")
  exit <- within(exit, rm(PersonalID))
  colnames(exit)[1] <- "PersonalID"
  exit <- unique(exit)
  
  # Load Services
  services <- loadServices(allDataPath)
  services <- sqldf("SELECT a.PrimaryPersonalID, b.* FROM primaryPersonalIDs a LEFT JOIN services b ON a.PersonalID=b.PersonalID")
  services <- within(services, rm(PersonalID))
  colnames(services)[1] <- "PersonalID"
  services <- unique(services)
  
  project <- loadProject(allDataPath)
  
  enrollmentAndProject  <- addProjectInfoToEnrollment(enrollment, project)
  
  # Get just Emergency Shelter Entry/Exit, Safe Haven, or Transitional Housing
  enrollmentAndProject <- sqldf("SELECT * FROM enrollmentAndProject 
                                              WHERE (ProjectType = 1 AND TrackingMethod != 3)
                                                    OR ProjectType = 2 
                                                    OR ProjectType = 8")
  
  houseless_flat <- sqldf("SELECT a.PersonalID, a.LastName, a.SSN, b.EnrollmentID, b.EntryDate, b.ProjectName, b.ProjectType, b.TrackingMethod
                          FROM client a
                          INNER JOIN enrollmentAndProject b
                          ON a.PersonalID=b.PersonalID
                          ")
  
  houseless_flat <- sqldf("SELECT a.*, b.ExitDate
                          FROM houseless_flat a
                          LEFT JOIN exit b
                          ON a.EnrollmentID=b.EnrollmentID
                          ORDER BY EntryDate DESC
                          ")
  
  
  # Gets max and min date
  bfr <- sqldf("SELECT MIN(EntryDate) As MinimumDate FROM houseless_flat")
  min_date <- ""
  if(startDate == ""){
    min_date <- as.character(bfr[1,1])  
  } else {
    min_date <- startDate
  }
  bfr <- sqldf("SELECT MAX(EntryDate) As MaximumDate FROM houseless_flat")
  max_date <- as.character(bfr)
  
  intervalConstant <- switch(interval,
         week = 7,
         month = 30,
         quarter = 120)
  
  numberOfIntervals <- switch(interval,
                           week = as.integer(getWeeksBetween(min_date, max_date)),
                           month = as.integer(getMonthsBetween(min_date, max_date)),
                           quarter = as.integer(getQuartersBetween(min_date, max_date)))
  
  # Get Start and End Date
  houseless_flat$EntryDate <- as.character(houseless_flat$EntryDate)
  houseless_flat$ExitDate <- as.character(houseless_flat$ExitDate)
  
  services <- sqldf("SELECT * FROM services WHERE RecordType = 12 OR RecordType = 200")
  services <- unique(services)
  
  # Create Entry / Exit dataframe
  total_houseless_count <- data.frame()
  
  # Gets the number of homeless in Entry / Exit Shelter.
  for(i in 0:numberOfIntervals){
    intervalStartDate <- as.Date(min_date) + i * intervalConstant
    intervalEndDate <- as.Date(min_date) + (i + 1) * intervalConstant
    
    # Get a distinct list of those active in Entry / Exit Shelter.
    bfr <- activeFilter(houseless_flat, "EntryDate", "ExitDate", intervalEndDate, intervalStartDate)
    active_ee_list <- sqldf("SELECT DISTINCT PersonalID FROM bfr")
    
    # Get NBN or Contact (12 or 200) in 90 days prior to date
    str <- paste("SELECT PersonalID FROM services WHERE DateProvided > '", as.Date(intervalStartDate) - servicesThreshold, "' AND DateProvided < '", intervalEndDate, "'", sep = "")
    active_nbn_or_outreach <- unique(sqldf(str))
    
    active_ee_list <- sqldf("SELECT DISTINCT(PersonalID) FROM active_ee_list")
    active_nbn_or_outreach <- sqldf("SELECT DISTINCT(PersonalID) FROM active_nbn_or_outreach")
    
    es_nbn_outreach <- unique(rbind(active_ee_list, active_nbn_or_outreach))
    # es_nbn_outreach <- unique(active_nbn_or_outreach)
    
    str <- paste("SELECT '", intervalEndDate, "' As 'Date', COUNT(PersonalID) As 'Count' FROM es_nbn_outreach", sep = "")
    this_count <- sqldf(str)
    print(this_count)
    total_houseless_count <- rbind(total_houseless_count, this_count)
  }
  
  remove(list=c("enrollment","exit", "project"))
  tmp <- total_houseless_count
  total_houseless_count$Date <- as.character(total_houseless_count$Date)
  
  total_houseless_count
}