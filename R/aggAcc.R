aggAcc <- function(path){
  
  myfilenames <- list.files(path = path)
  newpath <- paste(path,"/aggregate",sep="")
  dir.create(newpath,showWarnings='FALSE')
  
  mylist <- list() #create an empty list
  
  for(i in 1:length(myfilenames)){
    mynchar <- nchar(myfilenames[i])
    mystr <- substr(myfilenames[i],mynchar-5,mynchar)
    
    if(mystr == ".Rdata"){
      file.out <- paste(path,"/",myfilenames[i],sep="")
      load(file.out)
      if(nrow(summary)>1){
        mylist[[i]] <- as.matrix(summary)}
      rm(summary)
    }
  }
  
  aggregate <- do.call("rbind",mylist) #View(databind)
  file.out <- paste(newpath,"/","aggregate.Rdata",sep="")
  save(noquote(aggregate), file = file.out) 
}