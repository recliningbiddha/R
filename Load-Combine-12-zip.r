## Load last 12 files from .zip and combine into "data"
## Number of column and names may vary in each file.
## Routine matches colnames and if needed adds columns

## Improve function by returning data.frame

Combine12Zip<-function()
  {

##Need to get directory and .zip file names in directory  
## For now assume wd is set before function invoked.
  zipfile <- dir(pattern=".zip$")
  if (length(zipfile) >1) stop("More than ONE .zip file found.")

  ## Get list of files in .zip 
  filenames<- unzip(zipfile,list=TRUE)
    
  ## Convert dataframe to vector
  filenames<-filenames[,1]
  
  ## Select last 12 filenames
  filenames <- filenames[(length(filenames)-11):length(filenames)]

  ##Combine 1st two files into var "data"
  ## Number of column and names may vary in each file.
  ## Routine matches colnames and if needed adds columns
  x<-read.csv(unz(zipfile,filenames[1]))
  y<-read.csv(unz(zipfile,filenames[2]))
  x.diff<-setdiff(colnames(x),colnames(y))
  y.diff<-setdiff(colnames(y),colnames(x))
  x[,c(as.character(y.diff))]<-NA
  y[,c(as.character(x.diff))]<-NA
  data<-rbind(x,y)
  
  ##now loop through remaining file names adding each to data
  for (filename in filenames[3:length(filenames)]) { ##loop for filename from 3rd to end
    ## compared to above:
    ## x is new data from file, y is from var "data"
    x<- read.csv(unz(zipfile,filename))
    
    ## Number of column and names may vary in each file.
    ## Routine matches colnames and if needed adds columns
    x.diff<-setdiff(colnames(x),colnames(data))
    y.diff<-setdiff(colnames(data),colnames(x))
    x[,c(as.character(y.diff))]<-NA
    data[,c(as.character(x.diff))]<-NA
    data<-rbind(x,data)
    
  } ## end for filenames loop
 
   ## 12 last files in zip now combined into var "data"
   
   ##Close Connections
   closeAllConnections()
   
   ## Convert dates
   data$Operation.Date<-as.Date(data$Operation.Date, "%d-%b-%Y")
   
   ##Sort by start time (date & time field)
   data <- data[with(data, order(In.OR.At)),]
 
## Return variable with combines data set and clear working variables.
 return(data)
  
} ##End of function
