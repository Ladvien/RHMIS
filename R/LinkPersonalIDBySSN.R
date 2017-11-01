#' Takes Client, removes records where SSNs are invalid, then links the PersonalIDs
#' based upon matching SSNs.  The Client is returned with a column called PrimaryPersonalID
#' which should be a more unique identifier than the PersonalID alone.
#'
#' @param dataframe path to folder contaning first CSV set.
#' @param primaryPersonalIDsOnly Default false.  Returns a dataframe of only PrimaryPersonalIDs and PersonalIDs
#' @export
#' @examples
#' 
#'
#' clientDf1 <- data.frame(PersonalID=c("ZP1U3EPU2FKAWI6K5US5LDV50KRI1LN7", 
#'                       "IA26X38HOTOIBHYIRV8CKR5RDS8KNGHV", 
#'                       "LASDU89NRABVJWW779W4JGGAN90IQ5B2"), 
#'                       FirstName=c("Timmy", "Fela", "Sarah"),
#'                       LastName=c("Tesa", "Falla", "Kerrigan"),
#'                       SSN=c("123456789", "123456789", "987654321"))
#'
#' clientDf2 <- getPrimaryPersonalID(clientDf1)
#' # Remove the old PersonalID
#' clientDf2 <- within(clientDf2, rm(PersonalID))
#' colnames(clientDf2)[1] <- "PersonalID"
#' # Get a deduplicated Client
#' clientDf2 <- unique(clientDf2)

linkPersonalIDBySSN <- function(client, primaryPersonalIDsOnly = FALSE){

  originalClient <- client
  client <- sqldf("SELECT DISTINCT PersonalID, SSN FROM client")
  # Remove bad SSNs -- since we couldn't use them for record linking
  
  # Remove SSNs which are not less than 
  client$SSNCharCount <- nchar(client$SSN)
  client <- client[!(client$SSNCharCount < 9),]
  
  # Remove SSNs which are blank.
  client <- client[!is.na(client$SSN),]
  
  # Remove unreal
  client <- client[client$SSN!='123456789',]
  client <- client[client$SSN!='111111111',]
  client <- client[client$SSN!='000000000',]
  client <- client[client$SSN!='999999999',]
  client <- client[client$SSN!='0',]
  
  # Get the number of duplicates by SSN
  client$SSN <- as.factor(client$SSN)
  client <- client %>% group_by(SSN) %>% mutate(IDRank = row_number())
  primaryKeys <- client %>% group_by(SSN) %>% mutate(PrimaryPersonalID = PersonalID[IDRank == 1])
  
  clientWithPrimaryPersonalID <- sqldf("SELECT 
                                               PersonalID, 
                                               PrimaryPersonalID, 
                                               SSN, 
                                               IDRank 

                                               FROM primaryKeys")
  
  client <- sqldf("SELECT b.PrimaryPersonalID, a.*
                   FROM originalClient a
                   LEFT JOIN clientWithPrimaryPersonalID b
                   ON a.PersonalID=b.PersonalID")
  
  client$PrimaryPersonalID <- as.character(client$PrimaryPersonalID)
  client$PersonalID <- as.character(client$PersonalID)
  client$PrimaryPersonalID[is.na(client$PrimaryPersonalID)] <- client$PersonalID[is.na(client$PrimaryPersonalID)]
  
  if(primaryPersonalIDsOnly){
      client <- dataframe(client$PrimaryPersonalID, client$PersonalID)
      colnames(client) <- c("PrimaryPersonalID", "PersonalID")
  }
  client
}
