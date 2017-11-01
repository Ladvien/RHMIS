getDataErrorsByUserID <- function(client, 
                                  enrollment,
                                  exit
                                  ) {


    # Set Java memory high
    options(java.parameters = "-Xmx14336m") ## memory set to 14 GB

    # Determine the computer being run from.
    nodename <- Sys.info()['nodename']
    nodename
    dropboxPath <- ""
    mac <- FALSE
    generatedBy <- ""
    if (nodename == "DESKTOP-9ARS7LE") {
    dropboxPath <- "C:/Dropbox (TCHC)/TCHC Team Folder/HMIS Warehouse/"
    generatedBy = "C. Thomas Brittain"
    } else if (nodename == "Users-MBP"){
    dropboxPath <- "/Users/user/Dropbox (TCHC)/TCHC Team Folder/HMIS Warehouse/"
    mac <- TRUE
    generatedBy = "C. Thomas Brittain"
    } else if (nodename == "Users-MacBook-Pro.local"){
    dropboxPath <- "/Users/user/Dropbox (TCHC)/TCHC Team Folder/HMIS Warehouse/"
    mac <- TRUE
    generatedBy = "C. Thomas Brittain"
    } else if (nodename == "DESKTOP-UB9QQTU"){
    dropboxPath <- "E:/Dropbox (TCHC)/TCHC Team Folder/HMIS Warehouse/"
    generatedBy = "C. Thomas Brittain"
    } else if (nodename == "LAPTOP-1DT8SNN0"){
    dropboxPath <- "C:/Users/kemac/Dropbox (TCHC)/TCHC Team Folder/HMIS Warehouse/"
    generatedBy = "Kelly McWilliams"
    } else if (nodename == "LAPTOP-DFNDBFS3"){
    dropboxPath <- "C:/Users/jqual/Dropbox (TCHC)/TCHC Team Folder/HMIS Warehouse/"
    generatedBy = "J'Qualin Scott"
    } else if (nodename == "DESKTOP-N2ED2NE"){
    dropboxPath <-"C:/Users/Trudy Hernandez/Dropbox (TCHC)/TCHC Team Folder"
    generatedBy <- " Trudy Hernandez"
    }
    dropboxPath

    allDataPath <- paste(dropboxPath, "AllData/",sep = "")
    hmisFunctionsFilePath <- paste(dropboxPath, "/RScripts/HMIS_R_Functions.R", sep ="")
    source(hmisFunctionsFilePath)



    setError <- function(vector){
        vector <- as.numeric(vector)
        vector <- replace(vector, vector==NA | vector==8 | vector==9 | vector== 99, 255)
        vector
    }

    client <- loadClient(allDataPath)
    client$FirstName <- as.character(client$FirstName)
    client$FirstName[is.na(client$FirstName)] <- "255"

    client$LastName <- as.character(client$LastName)
    client$LastName[is.na(client$LastName)] <- "255"

    client$NameDataQuality <- as.numeric(client$NameDataQuality)
    client$NameDataQuality[is.na(client$NameDataQuality)] <- 255
    client$NameDataQuality <- setError(client$NameDataQuality)


client
    # enrollment <- loadEnrollment(allDataPath)
    # exit <- loadExit(allDataPath)

    # allData <- sqldf("SELECT *
    #                   FROM client a
    #                   LEFT JOIN enrollment b
    #                   ON a.PersonalID=b.PersonalID
    #                   LEFT JOIN exit c
    #                   ON b.EnrollmentID=c.EnrollmentID")


}