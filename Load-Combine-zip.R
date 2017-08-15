## Load files from .zip and Combines into "data"

## Improve this funcion by:
## 1) returning a data.frame
## 2) pass number of months of data to read to function

CombineAllZip<-function()
  {

##Need to get directory and .zip file names in directory  
## For now assume wd is set before function invoked.
  zipfile <- dir(pattern=".zip$")
  if (length(zipfile) >1) stop("More than ONE .zip file found.")

  ## Get list of files in .zip 
  filenames<- unzip(zipfile,list=TRUE)
    
  ## Convert dataframe to vector
  filenames<-filenames[,1]

  ##Combine 1st two files in .zip into var "data"
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
 
   ## All files in wd now combined into var "data"

## ?? add in some manipulations like convert dates to as.Date(x$Operation.Date, "%d-%b-%Y")
  
## Return variable with combines data set and clear working variables.
 closeAllConnections()
 return(data)
  
} ##End of function