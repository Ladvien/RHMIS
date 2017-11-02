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

    # Convert to number, find SSNs greater than 8 digits.
    client$SSN <- as.numeric(client$SSN)
    client$SSN[client$SSN <= 99999999] <- 255
    client$SSN[is.na(client$SSN)] <- 255
    
    client$DOB <- as.character(client$DOB)
    client$DOB[client$DOB == ''] <- "255"
    
    client$DOBDataQuality <- setError(client$DOBDataQuality)
    
    simpleParseColumns <- c("SSNDataQuality",
                            "NameDataQuality",
                            "DOBDataQuality",
                            "AmIndAKNative",
                            "Asian",
                            "BlackAfAmerican",
                            "NativeHIOtherPacific",	
                            "White", 
                            "RaceNone",
                            "Ethnicity",	
                            "Gender",
                            "VeteranStatus")

    client[simpleParseColumns] <- 
    lapply(client[simpleParseColumns],function(x){
        x <- as.numeric(x)
        x <- replace(x, x==NA | x==8 | x==9 | x== 99, 255)
        x
    })

    # Get a dataframe of all UserIDs
    errorsByUsers <- data.frame(unique(client$UserID))
    colnames(errorsByUsers)[1] <- "UserID"

    for(column in simpleParseColumns){
        tmp <- count(client[client[column] == 255,], "UserID")
        colnames(tmp)[2] <- paste(column, "Errors", sep = "")
        errorsByUsers <- merge(x = errorsByUsers, y = tmp, by = "UserID", all.x = TRUE)
    }

    # Also get FirstName, LastName, SSN, DOB
    complexParseColumns <- c("FirstName", "LastName", "SSN", "DOB")
    for(column in complexParseColumns){
        client[column] <- as.character(client[column])
        tmp <- data.frame(count(client[client[column] == "255",], "UserID"))
        colnames(tmp)[2] <- paste(column, "Errors", sep = "")
        errorsByUsers <- merge(x = errorsByUsers, y = tmp, by = "UserID", all.x = TRUE)
    }
tmp
    errorsByUsers[is.na(errorsByUsers)] <- 0
    errorsByUsersTotal <- aggregate(errorsByUsers$Frequency, by=list(Category=x$Category), FUN=sum)
    errorsByUsers$TotalErrors <- rowSums(errorsByUsers)

    errorsByUsers[order(-errorsByUsers$TotalErrors),]
}