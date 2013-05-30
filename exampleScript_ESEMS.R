#Install Mplus Automation if Needed and load the package
if("MplusAutomation" %in% rownames(installed.packages())==FALSE) install.packages("MplusAutomation")
require("MplusAutomation")
#Choose a directory for the Mplus Files etc.
cat("Choose a directory that you want to use. 
Mplus files will all be sent to there and run from there.
It is best to choose an empty directory or at least one with no existing mplus scripts.")
tempFile<- choose.dir()
#Load data
dataURL <- "http://raw.github.com/pdparker/ESEM/master/ESEM_SIM.dat"
data <- read.table(url(dataURL), header=TRUE, sep= "\t")
closeAllConnections()
data<-data.frame(apply(data,2, as.numeric))
#Run ESEM Invariance Script
scriptURL <- "http://raw.github.com/pdparker/ESEM/master/ESEM_Invariance.R"
source(url(scriptURL))
closeAllConnections()
#Prepare Mplus data
prepareMplusData(data, filename=file.path(tempFile, "ESEMdata.dat"))
#Write Esem Scripts
MplusData <- file.path(tempFile, "ESEMdata.dat")
esemInvaTarget(2, data, GroupVar = "group", c("males", "females"),
               1:12, FileOut=tempFile, FileIn=MplusData,
               Pattern=list(c(1,6), c(7,12)),LatentNames=c("Latent1", "Latent2"))
#Run Models
runModels(tempFile)
#Readout fit and place in table
FitSummaries <- extractModelSummaries(tempFile)
showSummaryTable(FitSummaries, keepCols=c("Title", "ChiSqM_Value", "ChiSqM_DF", 
                                          "CFI", "TLI", "BIC", "RMSEA_Estimate",
                                          "RMSEA_90CI_LB", "RMSEA_90CI_UB"),
                 sortBy="ChiSqM_DF")







