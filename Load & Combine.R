## Load and Combine Files in .csv format

CombineAll<-function()
  {

##Need to get directory and xlsx file names in directory  
## For now assume wd is set before function invoked.
  filenames <- dir(pattern=".csv$")

  ##Combine 1st two files in directory into data
  ## Number of column and names may vary in each file.
  ## Routine matches colnames and if needed adds columns
  x<-as.data.frame(read.csv(filenames[1]), header=TRUE)
  y<-as.data.frame(read.csv(filenames[2]), header=TRUE)
  x.diff<-setdiff(colnames(x),colnames(y))
  y.diff<-setdiff(colnames(y),colnames(x))
  x[,c(as.character(y.diff))]<-NA
  y[,c(as.character(x.diff))]<-NA
  data<-rbind(x,y)
  
  ##now loop through remaining file names adding each to data
  for (filename in filenames[3:length(filenames)]) { ##loop for filename from 3rd to end
    ## compared to above:
    ## x is new data from file, y is from var "data"
    x<- as.data.frame(read.csv(filename), header=TRUE)
    
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
 return(data)
  
} ##End of function