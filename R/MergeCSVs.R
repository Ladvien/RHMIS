#' Merges two HMIS CSV sets.
#'
#' @param string Path to folder contaning first CSV set.
#' @param string Path to folder contaning second CSV set.
#' @param string Path to output where merged CSVs will be written.
#' @param boolean Save in feather file format.  Default is false. Loading binary data from file takes around 1/10 the time compared to loading a CSV
#' @export
#' @examples
#' 
#' # Data will be saved in CSV format.
#' mergeHmisCSVs('/Users/user/local/CSV_2016', 
#'               '/Users/user/local/CSV_2016',
#'               '/Users/user/local/CSV_2016_and_2017')
#'
#' # Data will be saved in binary format.
#' mergeHmisCSVs('/Users/user/local/CSV_2016', 
#'               '/Users/user/local/CSV_2016',
#'               '/Users/user/local/CSV_2016_and_2017'
#'                feather = TRUE)
#'
mergeHmisCSVs <- function(dataPathOne, 
                             dataPathTwo, 
                             pathForCombinedData,
                             feather = FALSE){

  library(dplyr)  
  library(tcltk)

  newExportID <- "12345"
  
  # Checks to see if directory exits, otherwise, create it.
  dir.create(file.path(pathForCombinedData), showWarnings = FALSE)
  setwd(pathForCombinedData)
  
  mergeHmisCsvs <- function(df1, df2, newExportID){
    # Merge the data
    mergedDf <- rbind(df1, df2)
    # Drop columns which would resist removing duplicates
    mergedDf$ExportID <- newExportID
    mergedDf
  }
  
  #####################
  # Merge Affiliation #
  #####################
  cat("Merging Affilation\n")
  affiliationOne <- loadAffiliation(dataPathOne)
  affiliationTwo <- loadAffiliation(dataPathTwo)
  affiliationCombined <- rbind(affiliationOne, affiliationTwo)
  affiliationCombined <- unique(affiliationCombined)
  remove(list=c("affiliationOne", "affiliationTwo"))
  cat("Finished Affilation\n")
  
  #####################
  # Merge Client      #
  #####################
  cat("Merging Client\n")
  clientOne <- loadClient(dataPathOne)
  clientTwo <- loadClient(dataPathTwo)
  
  # Merge and deduplicate based upon most recent record.
  clientCombined <- mergeHmisCsvs(clientOne, clientTwo, newExportID)
  clientCombined <- getMostRecentRecordsPerId(clientCombined, "PersonalID", "DateUpdated")
  
  # Clears max flag
  clientCombined <- within(clientCombined, rm(list=c('MAX(DateUpdated)', 'MaxDateUpdated')))
  
  # Clean up
  remove(list=c("clientOne", "clientTwo"))
  cat("Finished Client\n")
  
  
  ######################
  # Merge Disabilities #
  ######################
  cat("Merging Disabilities\n")
  disabilitiesOne <- loadDisabilities(dataPathOne)
  disabilitiesTwo <- loadDisabilities(dataPathTwo)
  disabilitiesCombined <- mergeHmisCsvs(disabilitiesOne, disabilitiesTwo, newExportID)
  disabilitiesCombined <- getMostRecentRecordsPerId(disabilitiesCombined, "DisabilitiesID", "DateUpdated")
  disabilitiesCombined <- within(disabilitiesCombined, rm(list=c('MAX(DateUpdated)', 'MaxDateUpdated')))
  remove(list=c("disabilitiesOne", "disabilitiesTwo"))
  cat("Finished Disabilities\n")
  
  ##################################
  # Merge Employment and Education #
  ##################################
  cat("Merging Employment and Education\n")
  employmentEducationOne <- loadEmployementEducation(dataPathOne)
  employmentEducationTwo <- loadEmployementEducation(dataPathTwo)
  employmentEducationCombined <- mergeHmisCsvs(employmentEducationOne, employmentEducationTwo, newExportID)
  employmentEducationCombined <- getMostRecentRecordsPerId(employmentEducationCombined, "EmploymentEducationID", "DateUpdated")
  employmentEducationCombined <- within(employmentEducationCombined, rm(list=c('MAX(DateUpdated)', 'MaxDateUpdated')))
  remove(list=c("employmentEducationOne", "employmentEducationTwo"))
  cat("Finished Employment and Education\n")
  
  #####################
  # Merge Enrollment  #
  #####################
  cat("Merging Enrollment\n")
  enrollmentOne <- loadEnrollment(dataPathOne)
  enrollmentTwo <- loadEnrollment(dataPathTwo)
  enrollmentCombined <- mergeHmisCsvs(enrollmentOne, enrollmentTwo, newExportID)
  enrollmentCombined <- getMostRecentRecordsPerId(enrollmentCombined, "EnrollmentID", "DateUpdated")
  enrollmentCombined <- within(enrollmentCombined, rm(list=c('MAX(DateUpdated)', 'MaxDateUpdated')))
  remove(list=c("enrollmentOne", "enrollmentTwo"))
  cat("Finish Enrollment\n")
  
  #######################
  # Merge EnrollmentCoC #
  #######################
  cat("Merging EnrollmentCoC\n")
  enrollmentCocOne <- loadEnrollmentCoc(dataPathOne)
  enrollmentCocTwo <- loadEnrollmentCoc(dataPathTwo)
  enrollmentCocCombined <- mergeHmisCsvs(enrollmentCocOne, enrollmentCocTwo, newExportID)
  enrollmentCocCombined <- getMostRecentRecordsPerId(enrollmentCocCombined, "EnrollmentCoCID", "DateUpdated")
  enrollmentCocCombined <- within(enrollmentCocCombined, rm(list=c('MAX(DateUpdated)', 'MaxDateUpdated')))
  remove(list=c("enrollmentCocOne", "enrollmentCocTwo"))
  cat("Finished EnrollmentCoC\n")
  
  #####################
  # Merge Exit        #
  #####################
  cat("Merging Exit\n")
  exitOne <- loadExit(dataPathOne)
  exitTwo <- loadExit(dataPathTwo)
  exitCombined <- mergeHmisCsvs(exitOne, exitTwo, newExportID)
  exitCombined <- getMostRecentRecordsPerId(exitCombined, "ExitID", "DateUpdated")
  exitCombined <- within(exitCombined, rm(list=c('MAX(DateUpdated)', 'MaxDateUpdated')))
  remove(list=c("exitOne", "exitTwo"))
  cat("Finished Exit\n")
  
  #####################
  # Merge Export       #
  #####################
  cat("Merging Export\n")
  exportOne <- loadExport(dataPathOne)
  exportTwo <- loadExport(dataPathTwo)
  exportCombined <- exportTwo
  remove(list=c("exportOne", "exportTwo"))
  
  #####################
  # Merge Funder      #
  #####################
  cat("Merging Funder\n")
  funderOne <- loadFunder(dataPathOne)
  funderTwo <- loadFunder(dataPathTwo)
  funderCombined <- mergeHmisCsvs(funderOne, funderTwo, newExportID)
  funderCombined <- getMostRecentRecordsPerId(funderCombined, "FunderID", "DateUpdated")
  funderCombined <- within(funderCombined, rm(list=c('MAX(DateUpdated)', 'MaxDateUpdated')))
  remove(list=c("funderOne", "funderTwo"))
  cat("Finished Funder\n")
  
  #####################
  # Merge Health & DV #
  #####################
  cat("Merging Health and DV\n")
  healthAndDVOne <- loadHealthAndDv(dataPathOne)
  healthAndDVTwo <- loadHealthAndDv(dataPathTwo)
  healthAndDVCombined <- mergeHmisCsvs(healthAndDVOne, healthAndDVTwo, newExportID)
  healthAndDVCombined <- getMostRecentRecordsPerId(healthAndDVCombined, "HealthAndDVID", "DateUpdated")
  healthAndDVCombined <- within(healthAndDVCombined, rm(list=c('MAX(DateUpdated)', 'MaxDateUpdated')))
  remove(list=c("healthAndDVOne", "healthAndDVTwo"))
  cat("Finished Health and DV\n")
  
  
  
  #############################
  # Merge Income and Benefits #
  #############################
  cat("Merging Income and Benefits\n")
  incomeBenefitsOne <- loadIncomeBenefits(dataPathOne)
  incomeBenefitsTwo <- loadIncomeBenefits(dataPathTwo)
  incomeBenefitsCombined <- mergeHmisCsvs(incomeBenefitsOne, incomeBenefitsTwo, newExportID)
  incomeBenefitsCombined <- getMostRecentRecordsPerId(incomeBenefitsCombined, "IncomeBenefitsID", "DateUpdated")
  incomeBenefitsCombined <- within(incomeBenefitsCombined, rm(list=c('MAX(DateUpdated)', 'MaxDateUpdated')))
  remove(list=c("incomeBenefitsOne", "incomeBenefitsTwo"))
  cat("Finished Income and Benefits\n")
  
  #####################
  # Merge Inventory   #
  #####################
  cat("Merging Inventory\n")
  inventoryOne <- loadInventory(dataPathOne)
  inventoryTwo <- loadInventory(dataPathTwo)
  inventoryCombined <- mergeHmisCsvs(inventoryOne, inventoryTwo, newExportID)
  inventoryCombined <- getMostRecentRecordsPerId(inventoryCombined, "InventoryID", "DateUpdated")
  inventoryCombined <- within(inventoryCombined, rm(list=c('MAX(DateUpdated)', 'MaxDateUpdated')))
  remove(list=c("inventoryOne", "inventoryTwo"))
  cat("Finished Inventory\n")
  
  ######################
  # Merge Organization #
  ######################
  cat("Merging Organization\n")
  organizationOne <- loadOrganization(dataPathOne)
  organizationTwo <- loadOrganization(dataPathTwo)
  organizationCombined <- mergeHmisCsvs(organizationOne, organizationTwo, newExportID)
  organizationCombined <- getMostRecentRecordsPerId(organizationCombined, "OrganizationID", "DateUpdated")
  organizationCombined <- within(organizationCombined, rm(list=c('MAX(DateUpdated)', 'MaxDateUpdated')))
  remove(list=c("organizationOne", "organizationTwo"))
  cat("Finished Organization\n")
  
  #####################
  # Merge Project     #
  #####################
  cat("Merging Project\n")
  projectOne <- loadProject(dataPathOne)
  projectTwo <- loadProject(dataPathTwo)
  projectsCombined <- rbind(projectOne, projectTwo)
  # Get only the highest PIT Count
  projectsCombined <- projectsCombined %>% 
    group_by(ProjectID) %>% 
    filter(PITCount==max(PITCount))
  # Remove ExportID column for flattening
  projectsCombined$ExportID <- newExportID
  projectsCombined <- getMostRecentRecordsPerId(projectsCombined, "ProjectID", "DateUpdated")
  projectsCombined <- within(projectsCombined, rm(list=c('MAX(DateUpdated)', 'MaxDateUpdated')))
  remove(list=c("projectOne", "projectTwo"))
  cat("Finished Project\n")
  
  #####################
  # Merge Project CoC #
  #####################
  cat("Merging ProjectCoC\n")
  projectsCoCOne <- loadProjectCoc(dataPathOne)
  projectsCoCTwo <- loadProjectCoc(dataPathTwo)
  projectCoCCombined <- rbind(projectsCoCOne, projectsCoCTwo)
  # Get only the highest PIT Count
  projectCoCCombined <- projectCoCCombined %>% 
    group_by(ProjectID) %>% 
    filter(UserID==max(UserID))
  # Remove ExportID column for flattening
  projectCoCCombined$ExportID <- newExportID
  projectCoCCombined <- getMostRecentRecordsPerId(projectCoCCombined, "ProjectCoCID", "DateUpdated")
  projectCoCCombined <- within(projectCoCCombined, rm(list=c('MAX(DateUpdated)', 'MaxDateUpdated')))
  remove(list=c("projectsCoCOne", "projectsCoCTwo"))
  cat("Finished ProjectCoC\n")
  
  #####################
  # Merge Services    #
  #####################
  cat("Merging Services\n")
  servicesOne <- loadServices(dataPathOne)
  servicesTwo <- loadServices(dataPathTwo)
  servicesCombined <- mergeHmisCsvs(servicesOne, servicesTwo, newExportID)
  servicesCombined <- getMostRecentRecordsPerId(servicesCombined, "ServicesID", "DateUpdated")
  servicesCombined <- within(servicesCombined, rm(list=c('MAX(DateUpdated)', 'MaxDateUpdated')))
  remove(list=c("servicesOne", "servicesTwo"))
  cat("Finished Services\n")

  ############################
  # Write combined HMIS CSVs #
  ############################
  cat("Writing merged CSVs...\n")
  
  pathForOutput <- paste(pathForCombinedData,sep = "")
  if(feather){
    write_feather(affiliationCombined, paste(pathForOutput, "FeatherAffiliation", sep=""))
    write_feather(clientCombined, paste(pathForOutput, "FeatherClient", sep=""))
    write_feather(disabilitiesCombined, paste(pathForOutput, "FeatherDisabilities", sep=""))
    write_feather(employmentEducationCombined, paste(pathForOutput, "FeatherEmploymentEducation", sep=""))
    write_feather(enrollmentCombined, paste(pathForOutput, "FeatherEnrollment", sep=""))
    write_feather(enrollmentCocCombined, paste(pathForOutput, "FeatherEnrollmentCoC", sep=""))
    write_feather(exitCombined, paste(pathForOutput, "FeatherExit", sep=""))
    write_feather(exportCombined, paste(pathForOutput, "FeatherExport", sep=""))
    write_feather(funderCombined, paste(pathForOutput, "FeatherFunder", sep=""))
    write_feather(healthAndDVCombined, paste(pathForOutput, "FeatherHealthAndDV", sep=""))
    write_feather(incomeBenefitsCombined, paste(pathForOutput, "FeatherIncomeBenefits", sep=""))
    write_feather(inventoryCombined, paste(pathForOutput, "FeatherInventory", sep=""))
    write_feather(organizationCombined, paste(pathForOutput, "FeatherOrganization", sep=""))
    write_feather(projectsCombined, paste(pathForOutput, "FeatherProject", sep=""))
    write_feather(projectCoCCombined, paste(pathForOutput, "FeatherProjectCoC", sep=""))
    write_feather(servicesCombined, paste(pathForOutput, "FeatherServices", sep=""))
  }
  
  write.csv(affiliationCombined, file = paste(pathForOutput, "Affiliation.csv", sep=""), na = "", row.names = FALSE)
  write.csv(clientCombined, file = paste(pathForOutput, "Client.csv", sep=""), na = "", row.names = FALSE)
  write.csv(disabilitiesCombined, file = paste(pathForOutput, "Disabilities.csv", sep=""), na = "", row.names = FALSE)
  write.csv(employmentEducationCombined, file = paste(pathForOutput, "EmploymentEducation.csv", sep=""), na = "", row.names = FALSE)
  write.csv(enrollmentCombined, file = paste(pathForOutput, "Enrollment.csv", sep=""), na = "", row.names = FALSE)
  write.csv(enrollmentCocCombined, file = paste(pathForOutput, "EnrollmentCoC.csv", sep=""), na = "", row.names = FALSE)
  write.csv(exitCombined, file = paste(pathForOutput, "Exit.csv", sep=""), na = "", row.names = FALSE)
  write.csv(exportCombined, file = paste(pathForOutput, "Export.csv", sep=""), na = "", row.names = FALSE)
  write.csv(funderCombined, file = paste(pathForOutput, "Funder.csv", sep=""), na = "", row.names = FALSE)
  write.csv(healthAndDVCombined, file = paste(pathForOutput, "HealthAndDV.csv", sep=""), na = "", row.names = FALSE)
  write.csv(incomeBenefitsCombined, file = paste(pathForOutput, "IncomeBenefits.csv", sep=""), na = "", row.names = FALSE)
  write.csv(inventoryCombined, file = paste(pathForOutput, "Inventory.csv", sep=""), na = "", row.names = FALSE)
  write.csv(organizationCombined, file = paste(pathForOutput, "Organization.csv", sep=""), na = "", row.names = FALSE)
  write.csv(projectsCombined, file = paste(pathForOutput, "Project.csv", sep=""), na = "", row.names = FALSE)
  write.csv(projectCoCCombined, file = paste(pathForOutput, "ProjectCoC.csv", sep=""), na = "", row.names = FALSE)
  write.csv(servicesCombined, file = paste(pathForOutput, "Services.csv", sep=""), na = "", row.names = FALSE)
  
  cat("Finished writing CSVs.\n")
  cat("Merger all done.\n")
}
