#Install Mplus Automation if Needed and load the package
if("MplusAutomation" %in% rownames(installed.packages())==FALSE) install.packages("MplusAutomation")
require("MplusAutomation")
#Choose a directory for the Mplus Files etc.
cat("Choose a directory that you want to use. 
Mplus files will all be sent to there and run from there.
It is best to choose an empty directory or at least one with no existing mplus scripts.")
tempDir<- choose.dir()
#Load data
dataURL <- "http://raw.github.com/pdparker/ESEM/master/ESEM_SIM.dat"
data <- read.table(url(dataURL), header=TRUE, sep= "\t")
closeAllConnections()
data<-data.frame(apply(data,2, as.numeric))
data$gender<- ifelse(data$gender == 0, 1, 2)
data$treatment<- ifelse(data$treatment == 0, 1, 2)
#Run ESEM Invariance Script
scriptURL <- "http://raw.github.com/pdparker/ESEM/master/ESEM_Invariance_Geomin.R"
source(url(scriptURL))
closeAllConnections()
#Prepare Mplus data
prepareMplusData(data, filename=file.path(tempDir, "ESEMdata.dat"))

#---------------------------------------------------#
#Time 2 invariance across treatment group and gender#
#---------------------------------------------------#
#Write Esem Scripts
MplusData <- file.path(tempDir, "ESEMdata.dat")
esemInvaGeomin(2, data, GroupVar = "treatByGender", c("treatM", "contM", "treatF", "contF"),
               7:12, FileOut=tempDir, FileIn=MplusData)

#Run Models
runModels(tempDir)
#Readout fit and place in table
FitSummaries <- extractModelSummaries(tempDir)
showSummaryTable(FitSummaries, keepCols=c("Title", "ChiSqM_Value", "ChiSqM_DF", 
                                          "CFI", "TLI", "RMSEA_Estimate",
                                          "RMSEA_90CI_LB", "RMSEA_90CI_UB"),
                 sortBy="Title")
#Clean up directory by deleting all mplus files
junk <- dir(path=tempDir,  pattern=".inp$|.out$", full.names = TRUE) 
file.remove(junk)
#---------------------------------------------------#
#Time 2 invariance across treatment group#
#---------------------------------------------------#
#Write Esem Scripts
MplusData <- file.path(tempDir, "ESEMdata.dat")
esemInvaGeomin(2, data, GroupVar = "treatment", c("treat", "control"),
               1:6, FileOut=tempDir, FileIn=MplusData)

#Run Models
runModels(tempDir)
#Readout fit and place in table
FitSummaries <- extractModelSummaries(tempDir)
showSummaryTable(FitSummaries, keepCols=c("Title", "ChiSqM_Value", "ChiSqM_DF", 
                                          "CFI", "TLI", "RMSEA_Estimate",
                                          "RMSEA_90CI_LB", "RMSEA_90CI_UB"),
                 sortBy="Title")
#Clean up directory by deleting all mplus files
junk <- dir(path=tempDir,  pattern=".inp$|.out$", full.names = TRUE) 
file.remove(junk)
#---------------------------------------------------#
#Time 2 invariance across treatment gender#
#---------------------------------------------------#
#Write Esem Scripts
MplusData <- file.path(tempDir, "ESEMdata.dat")
esemInvaGeomin(2, data, GroupVar = "gender", c("male", "female"),
               1:6, FileOut=tempDir, FileIn=MplusData)

#Run Models
runModels(tempDir)
#Readout fit and place in table
FitSummaries <- extractModelSummaries(tempDir)
showSummaryTable(FitSummaries, keepCols=c("Title", "ChiSqM_Value", "ChiSqM_DF", 
                                          "CFI", "TLI", "RMSEA_Estimate",
                                          "RMSEA_90CI_LB", "RMSEA_90CI_UB"),
                 sortBy="Title")
#Clean up directory by deleting all mplus files
junk <- dir(path=tempDir,  pattern=".inp$|.out$", full.names = TRUE) 
file.remove(junk)
