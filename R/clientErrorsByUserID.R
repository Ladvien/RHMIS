#' Takes the Client.csv as a dataframe, parses it for data errors, and returns a count of 
#' errors by element, total of all errors, by UserID.
#' 
#'
#' @param dataframe path to folder contaning first CSV set.
#' @export
#' @examples
#' 
#' 
#' clientDf1 <- data.frame(PersonalID=c("ZP1U3EPU2FKAWI6K5US5LDV50KRI1LN7", 
#'                       "IA26X38HOTOIBHYIRV8CKR5RDS8KNGHV", 
#'                       "LASDU89NRABVJWW779W4JGGAN90IQ5B2"), 
#'                       FirstName=c("", "Fela", "Sarah"),
#'                       LastName=c("Tesa", "Falla", "Kerrigan"),
#'                       SSN=c("123456789", "", "987654321",
#'                       DOB=c("1980-01-01", "", "1983-08-26"))
#'
#' errorsByUserID <- clientErrorsByUserID(clientDf1)
clientErrorsByUserID <- function(client) {

    setError <- function(vector){
        vector <- as.numeric(vector)
        vector <- replace(vector, vector==NA | vector==8 | vector==9 | vector== 99, 255)
        vector
    }

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

    errorsByUsers[is.na(errorsByUsers)] <- 0
    errorsByUsers$TotalErrors <- rowSums(errorsByUsers[,2:ncol(errorsByUsers)])

    errorsByUsers[order(-errorsByUsers$TotalErrors),]
}