readRawBatch <- function(path){
  
  myfilenames <- list.files(path = path)
  newpath <- paste(path,"/readfiles",sep="")
  dir.create(newpath,showWarnings='FALSE')
  
  for(i in 1:length(myfilenames)){
    mynchar <- nchar(myfilenames[i])
    mystr <- substr(myfilenames[i],mynchar-3,mynchar)
    
    if(mystr == ".dat"){
      infilename <- paste(path,"/",myfilenames[i],sep="")
      myID <- substr(myfilenames[i], 1, mynchar-4)
      counts <- readRaw(infilename)
      file.out <- paste(newpath,"/",myID,".Rdata",sep="")
      save(counts, file = file.out) 
    }
  }
}