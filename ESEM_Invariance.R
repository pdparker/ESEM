#Fs        = Number of factors for extraction
#Data      = The data file to be used. Must be a real loaded dataset.
#GroupVar  = The multigroup variable name. Groups must be coded >0 (i.e. 1, 2 NOT 0,1)
#Use       = Numeric indication of which variables in Data will be used in the usevariables command.
#            Numbers can be determined by using "names(myData)" 
#FileOut   = The folder you want the created mplus scripts sent to.
#FileIn    = The file in which the data (.dat) file is kept.
#            Must use the prepareMplusData call from Mplus automation.
#Pattern   = A list where elements 1 to N  consist of the number of the first and last item assigned
#            to a given latent factor.
#LatentNames=The names to be assigned to the 1 to N requested latent variables
esemInvaTarget <- function(Fs, Data, GroupVar, Groups, Use, FileOut=getwd(), FileIn, Pattern,
                           LatentNames){
  #WARNINGS
  if(length(Pattern)!= Fs) stop("Number of distinct patterns must match the number of factors requested")
  if(max(unlist(Pattern)) < length(Use)) stop("Pattern requests less items than are present in Use")
  if(max(unlist(Pattern)) > length(Use)) stop("Pattern requests more items than are present in Use")
  if(!is.character(GroupVar)) stop("GroupVar must have mode character")
  if(!is.numeric(Use)) stop("Use must be numeric")
  if(!is.list(Pattern)) stop("Pattern must be a list")
  if(any(lapply(Pattern, length)>2|any(lapply(Pattern, length)<2) ) ) {
    stop("Pattern must contain position in Data of first and last item assigned to each latent variable")}
  if(any(regexpr("\\.", names(Data))>0)) stop(". is an illegal character in Mplus variable names")
  if(any(regexpr("\\.", LatentNames)>0)) stop(". is an illegal character in Mplus variable names")
  if(any(regexpr("^[0-9]", names(Data))>0)) stop("Mplus variable names cannot start with a number")
  if(any(regexpr("^[0-9]", LatentNames)>0)) stop("Mplus variable names cannot start with a number")
  if(any(grepl('\\.(csv|txt|dat)$', FileIn)==0)) stop('Mplus only accepts dat txt or csv files')
  if(any(lapply(names(Data), nchar)>8)) warning("At least one of the variable names is greater than 8 characters. This may cause unexpected output in Mplus")
  #Preprocessing
  #Variable names into single string
  varNames <- paste(names(Data), collapse=" ")
  #variables that will go in the usevariables command. Pasted as a single string
  useVars  <- paste(names(Data[,Use]), collapse=" ")
  #The number of variables in usevariables for setting constraints
  Ys       <- length(names(Data[,Use]))
  
  #Grouping variables
  #Check the referent column number for grouping variable
  x <- which(names(Data)==GroupVar)
  #Group must be numeric for mplus. This is just a local conversion.
  Data[,x] <- as.factor(Data[,x])
  #set to the group names to those set in function call
  levels(Data[,x]) <- Groups
  #Creates groups is call but also removes missing data as a level
  Grouped<- list()
            for (i in 1:length(unique(na.omit(Data[,x])))){
                Grouped[[i]]<- paste(i, "=", levels(Data[,x])[i], sep=" ")
                }
  #Pastes group is Mplus line
  GroupLine <- paste("grouping is ", GroupVar, "(", paste(Grouped, collapse=" "), ")", ";")
  #efa syntax line
  efaLine <- list()
  
  for (i in 1:Fs){
                  if(i == 1){
                     efaLine[[i]] <- paste(LatentNames[i], " BY ", names(Data[,Use])[Pattern[[i]][1]], "-", 
                                    names(Data[,Use])[Pattern[[i]][2]], " ", names(Data[,Use])[Pattern[[i+1]][1]],
                                     "-", names(Data[,Use])[Pattern[[Fs]][2]], "~", 0, " (*t);",
                                     sep="") 
                            }else if(i == Fs){
                              efaLine[[i]] <- paste(LatentNames[i], " BY ", names(Data[,Use])[Pattern[[i]][1]], "-", 
                                     names(Data[,Use])[Pattern[[i]][2]], " ", names(Data[,Use])[Pattern[[1]][1]],
                                     "-", names(Data[,Use])[Pattern[[Fs-1]][2]], "~", 0, " (*t);",
                                     sep="")
                             }else{
                               efaLine[[i]] <-paste(LatentNames[i], " BY ", names(Data[,Use])[Pattern[[i]][1]], "-",
                                names(Data[,Use])[Pattern[[i]][2]], " ", 
                               names(Data[,Use])[Pattern[[1]][1]],"-", names(Data[,Use])[Pattern[[i-1]][2]], "~", 0, " ",
                                names(Data[,Use])[Pattern[[i+1]][1]],"-", names(Data[,Use])[Pattern[[Fs]][2]], "~", 0,
                              " (*t);",sep="")
                            }
                }
  
  efaLine <- paste(efaLine, collapse="\n")
  #means
  meanLine <-  paste("[", LatentNames[1], "-", LatentNames[Fs], "@", 0, "]", ";", sep="")
  #Variances
  variances <- paste(LatentNames[1], "-", LatentNames[Fs], "@", 1, ";", sep="")
  #covariances
  cov.pairs <-   expand.grid(LatentNames, LatentNames)
  #Gets rid of variances
  cov.pairs <- cov.pairs[!(cov.pairs[,1] == cov.pairs[,2]),]
  #Gets rid of duplicate covariances
  cov.pairs <-   cov.pairs[!duplicated(t(apply(cov.pairs, 1, sort))), ]
  #Pastes covariances into single string
  covariances <- paste(cov.pairs[,1], " with ", cov.pairs[,2], "(", 1:nrow(cov.pairs), ")", ";", sep="")
  #Following lines print covariances to a max of 85 characters with lines split at ;
  covariances <- gsub(" ", "\a", covariances)
  covariances <- paste(covariances, collapse=" ")
  covariances <- strwrap(covariances, width = 85, exdent = 0)
  covariances <- gsub("\a", " ", covariances)
  #Errors. Note that constraint numbers must be different from loadings
  Error <- paste(names(Data[,Use]), "(", (nrow(cov.pairs)+1):(nrow(cov.pairs)+Ys), ")", ";", sep="")
  Error <- paste(Error, collapse=" ")
  #Intercepts
  Intercept <- paste("[", names(Data)[Use[1]], "-", names(Data)[Use[length(Use)]], "]", ";", sep="")
  
  
  #Create Master Mplus File from which all 13 ESEM files will be created.
  Groups2 <- Groups
  Groups2[1] <- ""
  #Master File
  MultiGroup1 <- list()
  for(i in 1:length(Groups)){
      MultiGroup1[[i]] <- paste(paste("Model", Groups2[i], ":"),
                               '!FactorLoadings',efaLine,
                               '!Means', meanLine,
                               '!Intercepts', Intercept, 
                               '!Variances', variances, 
                               '!Covariances',
                                 paste(covariances, collapse = "\n"), "\n",
                               '!Errors',
                                 paste(strwrap(paste(Error), width = 85, exdent = 5), collapse = "\n"),
                               '!Marker', sep="\n")
      }
  
  MultiGroup1 <- do.call(paste, c(MultiGroup1, sep="\n\n"))
  
  cat("TITLE: ESEM Model NO.;", paste("DATA: FILE =", FileIn, ";", sep=" "),
      "VARIABLE: NAMES = ", 
      paste(strwrap(paste(varNames), width = 85, exdent = 5), collapse = "\n"),
      ";\n\n", "usevariables ",  
      paste(strwrap(paste(useVars), width = 85, exdent = 5),collapse = "\n"), ";",
      "MISSING=.;\n", GroupLine, "\n",
      "ANALYSIS: estimator=mlr; ROTATION = target(OBLIQUE);", "\n",
      MultiGroup1,"\n",
      paste("OUTPUT: TECH1;"),  sep="\n", 
      file=paste0(FileOut,"/ModelTemp.inp"))
  #Reads in created file for manipulation
  Model <- readLines(paste0(FileOut,"/ModelTemp.inp"))
  #Warning
  if(any(lapply(Model, nchar)>90))stop("A line in the model is greater than the allowable 90 characters. 
                                      This may be due to variable names or file names that are too long.")
  #Line numbers used to remove relavent unneeded code for each input file 
  innits <- matrix(grep('!', Model), length(Groups), byrow=TRUE)  
#######################################
#File Creation for 13 ESEM Invariance #
#######################################  
  #Model 1
  #Master file reassigned
  Model1 <- Model
  #Loop to remove covars, vars, etc. must start at bottom of file
  for (i in length(Groups):2){
       Model1 <- Model1[-c(innits[i,4]:innits[i,7])]
       }
  #First group must be seperate as code differs
  Model1 <- Model1[-c(innits[1,4]:innits[1,7])]
  #Give each model a unique name for Mplus automation
  Model1 <- gsub('NO.', '01', Model1)
  #Save created input file
  writeLines(Model1, paste(FileOut,"Model1.inp", sep="/"))
  #Model 2
  Model2 <- Model
  for (i in length(Groups):2){
       Model2<-Model2[-c(innits[i,1]:(innits[i,2]),innits[i,4]:innits[i,7])]
       }
  Model2 <- Model2[-c(innits[1,4]:innits[1,7])]
  Model2 <- gsub('NO.', '02', Model2)
  writeLines(Model2, paste(FileOut,"Model2.inp", sep="/"))
  #Model 3 
  Model3 <- Model
  for (i in length(Groups):2){
       Model3<- Model3[-c(innits[i,1]:(innits[i,2]-1), innits[i,4]:(innits[i,6]-1), innits[i,7])]
       }
  Model3 <- Model3[-c(innits[1,3]:(innits[1,6]-1), innits[1,7])]
  Model3 <- gsub('NO.', '03', Model3)
  writeLines(Model3, paste(FileOut,"Model3.inp", sep="/"))
  #Model 4 
  Model4 <- Model
  for (i in length(Groups):2){
       Model4<- Model4[-c(innits[i,1]:(innits[i,2]-1),innits[i,6]:innits[i,7])]
       }
  Model4 <- Model4[-c(innits[1,6]:innits[1,7])]       
  Model4 <- gsub('NO.', '04', Model4)
  writeLines(Model4, paste(FileOut,"Model4.inp", sep="/"))
  #Model 5 
  Model5 <- Model
  for (i in length(Groups):2){
      Model5 <- Model5[-c(innits[i,1]:innits[i,7])]
      }
  Model5 <- Model5[-c(innits[1,2]:(innits[1,3]-1), innits[1,4]:innits[1,7])]       
  Model5 <- gsub('NO.', '05', Model5)
  writeLines(Model5, paste(FileOut,"Model5.inp", sep="/"))
  #Model 6 
  Model6<- Model
  for (i in length(Groups):2){
      Model6 <- Model6[-c(innits[i,1]:(innits[i,2]-1), innits[i,7])]
      }
  Model6 <- Model6[-c(innits[1,7])]       
  Model6 <- gsub('NO.', '06', Model6)
  writeLines(Model6, paste(FileOut,"Model6.inp", sep="/"))
  #Model 7 
  Model7 <- Model
  for (i in length(Groups):2){
      Model7 <- Model7[-c(innits[i,1]:(innits[i,6]-1), innits[i,7])]
      }
  Model7 <- Model7[-c(innits[1,2]:(innits[1,3]-1), innits[1,4]:(innits[1,6]-1), innits[1,7])]       
  Model7 <- gsub('NO.', '07', Model7)
  writeLines(Model7, paste(FileOut,"Model7.inp", sep="/"))
  #Model 8 
  Model8 <- Model
  for (i in length(Groups):2){
      Model8 <- Model8[-c(innits[i,1]:(innits[i,4]-1), innits[i,6]:innits[i,7])]
      }
  Model8 <- Model8[-c(innits[1,2]:(innits[1,3]-1), innits[1,6]:innits[1,7])]       
  Model8 <- gsub('NO.', '08', Model8)
  writeLines(Model8, paste(FileOut,"Model8.inp", sep="/"))
  #Model 9 
  Model9 <- Model
  for (i in length(Groups):2){
      Model9 <- Model9[-c(innits[i,1]:(innits[i,4]-1), innits[i,7])]
      }
  Model9 <- Model9[-c(innits[1,2]:(innits[1,3]-1), innits[1,7])]       
  Model9 <- gsub('NO.', '09', Model9)
  writeLines(Model9, paste(FileOut,"Model9.inp", sep="/"))
  #Model 10 
  Model10 <- Model
  for (i in length(Groups):2){
      Model10 <- Model10[-c(innits[i,1]:(innits[i,2]-1), innits[i,3]:innits[i,7])]
      }
  Model10 <- Model10[-c(innits[1,4]:innits[1,7])]       
  Model10 <- gsub('NO.', '10', Model10)
  writeLines(Model10, paste(FileOut,"Model10.inp", sep="/"))
  #Model 11 
  Model11 <- Model
  for (i in length(Groups):2){
      Model11 <- Model11[-c(innits[i,1]:(innits[i,2]-1), innits[i,3]:(innits[i,6]-1), innits[i,7])]
      }
  Model11 <- Model11[-c(innits[1,4]:(innits[1,6]-1), innits[1,7])]       
  Model11 <- gsub('NO.', '11', Model11)
  writeLines(Model11, paste(FileOut,"Model11.inp", sep="/"))
  #Model 12 
  Model12 <- Model
  for (i in length(Groups):2){
      Model12 <- Model12[-c(innits[i,1]:(innits[i,2]-1), innits[i,3]:(innits[i,4]-1), innits[i,6]:innits[i,7])]
      }
  Model12 <- Model12[-c(innits[1,6]:innits[1,7])]       
  Model12 <- gsub('NO.', '12', Model12)
  writeLines(Model12, paste(FileOut,"Model12.inp", sep="/"))
  #Model 13 
  Model13 <- Model
  for (i in length(Groups):2){
      Model13 <- Model13[-c(innits[i,1]:(innits[i,2]-1), innits[i,3]:(innits[i,4]-1), innits[i,7])]
      }
  Model13 <- Model13[-c(innits[1,7])]       
  Model13 <- gsub('NO.', '13', Model13)
  writeLines(Model13, paste(FileOut,"Model13.inp", sep="/"))
  #Removes master file so it does not get used in Mplus Automation
  file.remove(paste0(FileOut,"/ModelTemp.inp"))
}

  
