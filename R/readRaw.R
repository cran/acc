

readRaw <- function(filename){
  
  Tfile <- file(filename, "r")
  if(isOpen(Tfile, "r")) #  TRUE
  {
    seek(Tfile, 0, rw="r") # reset to beginning
    lines = readLines(Tfile)
    close(Tfile)
  }
  
  skipPos = grep("-----", lines)[2]  #number of skip lines
  startTPos = grep("Start Time", lines)  #start time
  
  startEPos = grep("Epoch Period", lines) 
  startDPos = grep("------------ Data File Created By ActiGraph ", lines) 
  startSPos = grep("Serial Number: ", lines) 
  
  #get start date
  startTime = gsub("Start Time ", "", lines[startTPos])
  startTime = gsub("[[:blank:]]", "", startTime)
  startDatePos = grep("Start Date ", lines)  #startdate
  startDate = gsub("Start Date ", "", lines[startDatePos])
  startDate = gsub("[[:blank:]]", "", startDate)
  startDate = strsplit(startDate, "/")[[1]]
  if(nchar(startDate[1]) == 1){
    startDate[1] = paste("0", startDate[1], sep ="")
  }
  if(nchar(startDate[2]) == 1){
    startDate[2] = paste("0", startDate[2], sep ="")
  }
  startDate = paste(startDate[3], startDate[1], startDate[2], sep = "-")
  #end of getting startdate
  rawTimeStamp1  = paste(startDate, startTime, sep = " ")
  
  epochTime = gsub("Epoch Period (hh:mm:ss) ", "", lines[startEPos])
  epochTime = substr(epochTime, nchar(epochTime)-7, nchar(epochTime))
  ep = as.numeric(as.difftime(c(epochTime),units = "secs"))
  
  deviceName = gsub("------------ Data File Created By ActiGraph ", "", lines[startDPos])
  deviceName = substr(deviceName, 1,4)
  
  serialNumber = gsub("Serial Number: ", "", lines[startSPos])
  type <- NA
  if(deviceName == "GT1M"){type <- "uni-axial"}
  if(deviceName == "GT3X"){type <- "tri-axial"}
  
  cat(noquote(paste("Raw data read for ",deviceName," device. This is a ",type," device.",sep=""))) 
  cat("\n")
  cat(noquote(paste("Serial number: ",serialNumber,".",sep="")))
  cat("\n")
  cat(noquote(paste("Start date is ",startDate," and epoch is ",ep," seconds.",sep="")))
  cat("\n")
  
  startline = skipPos+1
  endline = length(lines)
  
  rawdata = c()
  timeline = c()
  for(i in startline: endline){
    temp0 = gsub("[[:blank:]]+", " ",lines[i])
    temp = strsplit(temp0, " ")[[1]]
    temp = temp[temp != ""]
    rawdata = c(rawdata, temp)
  }
  
  counts = as.numeric(as.vector(rawdata))
  
  if(type=="uni-axial"){
    timeline = (0:as.integer(length(rawdata)-1)*ep)
    rawTimeStamp = rep(rawTimeStamp1, length(rawdata))
    rst = gsub(" GMT", "", as.POSIXlt(rawTimeStamp, tz = "GMT")+ timeline)
    data = data.frame(TimeStamp = as.vector(rst), counts = counts)
  }
  
  if(type=="tri-axial"){
    n <- length(rawdata)/3
    x = counts[seq(1, n, 3)] 
    y = counts[seq(2, n, 3)] 
    z = counts[seq(3, n, 3)] 
    timeline = (0:as.integer((length(rawdata))/3-1)*ep)
    rawTimeStamp = rep(rawTimeStamp1, length(rawdata)/3)
    rst = gsub(" GMT", "", as.POSIXlt(rawTimeStamp, tz = "GMT")+ timeline)
    data = data.frame(TimeStamp = as.vector(rst), x = x, y = y, z = z)
  }
  
  data
  
}
