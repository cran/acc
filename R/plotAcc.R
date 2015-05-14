



plotAcc <- function(object,date='NA'){
  
  if(names(object[1])=="totalDates"){
    counts <- object$sedentary$counts
    time <- object$sedentary$TimeStamp
    sedState <- object$sedentary$inSedentary
    mvpaState <- object$MVPA$inMVPA
    nonwearState <- object$sedentary$nonwear
    inboutSedentary <- object$sedentary$inboutSedentary
    inboutMVPA <- object$MVPA$inboutMVPA
    inbout <- inboutSedentary + inboutMVPA
    par(mar=c(6, 4, 3, 2)) # bottom, left, top and right
    plot(counts,type="l",ylim=c((min(counts)-30),(max(counts)+300)),axes=FALSE, ann=FALSE)
    mytime <- strptime(time,format='%Y-%m-%d %H:%M:%S')
    timeaxis <- format(mytime, format="%Y-%m-%d")
    min <- format(mytime, format="%M")
    hour <- format(mytime, format="%H")
    
    if(length(time)<=120){
      hourmark <- ifelse(( min=="00" | min=="15" | min=="30" | min=="45"  | min=="60"),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(120<length(time) & length(time)<=600){
      hourmark <- ifelse(( min=="00" | min=="30" | min=="60"),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(600<length(time) & length(time)<=1440){
      hourmark <- ifelse(min=="00",1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(length(time)>1440 & length(time)<=2880){
      hourmark <- ifelse(min=="00"  & ((hour=="00")|(hour=="06")|(hour=="12")|(hour=="18")),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(length(time)>2880 & length(time)<=4320){
      hourmark <- ifelse(min=="00"  & ((hour=="00")|(hour=="12")),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(length(time)>4320 & length(time)<=7200){
      hourmark <- ifelse(min=="00"  & ((hour=="00")),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(length(time)>7200){
      hourmark <- ifelse(min=="00"  & (hour=="00"),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    title(main="Plot of accelerometer data",cex.main=1.5)
    title(ylab="Acceleration counts")
    title(xlab="Time",line=5)
    cols <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
    t <- rep(1:length(counts))
    sedStatebind <- data.frame(cbind(t,sedState))  
    sedStatebind$col[sedStatebind$sedState == 1] <- "thistle3"
    rect(xleft =sedStatebind$t-1, xright = sedStatebind$t, 
         ybottom=min(counts), ytop=max(counts), 
         col=sedStatebind$col, border=NA )
    mvpaStatebind <- data.frame(cbind(t,mvpaState)) 
    mvpaStatebind$col[mvpaStatebind$mvpaState == 1] <- "springgreen"
    rect(xleft =mvpaStatebind$t-1, xright = mvpaStatebind$t, 
         ybottom=min(counts), ytop=max(counts), 
         col=mvpaStatebind$col, border=NA )
    nonwearStatebind <- data.frame(cbind(t,nonwearState))  
    nonwearStatebind$col[nonwearStatebind$nonwearState == 1] <- "khaki"
    rect(xleft =nonwearStatebind$t-1, xright = nonwearStatebind$t, 
         ybottom=min(counts), ytop=max(counts), 
         col=nonwearStatebind$col, border=NA )     # cols[5] "maroon1" "khaki1"
    lines(counts,type="l",ylim=c((min(counts)-30),(max(counts)+30)))
    
    barcols <- c("white","sienna1")
    for(ii in 0:length(inbout)){
      rect(xleft   = ii,
           ybottom = (min(counts)-max(counts)*.01),
           xright  = ii+1, 
           ytop    = (min(counts)-max(counts)*.04),
           col = barcols[inbout[ii]+1], border = 0)
    }              
    # Create a legend 
    legend(1*length(counts)/10, (max(counts))*1.08, 
           c(expression(italic("In bout"))), 
           text.col = "blue", cex=0.6, horiz = TRUE, #xpd=TRUE, 
           bty = "n", fill=c("sienna1"), xjust = 0.01)
    
    legend(3*length(counts)/10, (max(counts))*1.08, 
           c(expression(italic("Sedentary"))), 
           text.col = "blue", cex=0.6, horiz = TRUE, #xpd=TRUE, 
           bty = "n", fill=c("thistle3"), xjust = 0.01)
    
    legend(5*length(counts)/10, (max(counts))*1.08, 
           c(expression(italic("MVPA"))), 
           text.col = "blue", cex=0.6, horiz = TRUE, #xpd=TRUE, 
           bty = "n", fill=c("springgreen"), xjust = 0.01)
    
    legend(7*length(counts)/10, (max(counts))*1.08, 
           c(expression(italic("Non-wear time"))), 
           text.col = "blue", cex=0.6, horiz = TRUE, #xpd=TRUE, 
           bty = "n", fill=c("khaki"), xjust = 0.01)
  }
  
  
  # object <- myfile   
  if(names(object[1])!="totalDates"){
    
    mytime <- strptime(object$TimeStamp,format='%Y-%m-%d %H:%M:%S')
    timeaxis <- format(mytime, format="%Y-%m-%d")
    min <- format(mytime, format="%M")
    hour <- format(mytime, format="%H")
    
    if(names(object[2])=="counts"){
      par(mar=c(6, 4, 3, 2)) # bottom, left, top and right
      plot(object$counts,type="l",ylim=c((min(object$counts)-30),
                                         (max(object$counts)+500)),
           axes=FALSE, ann=FALSE, col ="#1b9e77")
      
      if(date!='NA'){
        datelist <- format(mytime, format="%Y-%m-%d")
        mydate <- which(datelist == date)[1]
        lines(x=rep(mydate,(max(object$counts)+1)),0:max(object$counts),
              lty=2,lwd=3,col="gray70")
      }
    }
    
    if(names(object[2])!="counts"){
      par(mar=c(6, 4, 3, 2)) # bottom, left, top and right
      plot(object$x,type="l",ylim=c((min(object$x,object$y,object$z)-30),
                                    (max(object$x,object$y,object$z)+500)),
           axes=FALSE, ann=FALSE, col ="#1b9e77")
      lines(object$y, col ="#d95f02")
      lines(object$z, col ="#7570b3")
      
      if(date!='NA'){
        datelist <- format(mytime, format="%Y-%m-%d")
        mydate <- which(datelist == date)[1]
        lines(x=rep(mydate,(max(object$x,object$y,object$z)+1)),0:max(object$x,object$y,object$z),
              lty=2,lwd=3,col="gray70")
      }
    }
    
    
    if(length(object$TimeStamp)<=120){
      hourmark <- ifelse(( min=="00" | min=="15" | min=="30" | min=="45"  | min=="60"),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(object$TimeStamp)))
      axis(2)
    }
    
    if(120<length(object$TimeStamp) & length(object$TimeStamp)<=600){
      hourmark <- ifelse(( min=="00" | min=="30" | min=="60"),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(object$TimeStamp)))
      axis(2)
    }
    
    if(600<length(object$TimeStamp) & length(object$TimeStamp)<=1440){
      hourmark <- ifelse(min=="00",1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(object$TimeStamp)))
      axis(2)
    }
    
    if(length(object$TimeStamp)>1440 & length(object$TimeStamp)<=2880){
      hourmark <- ifelse(min=="00"  & ((hour=="00")|(hour=="06")|(hour=="12")|(hour=="18")),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(object$TimeStamp)))
      axis(2)
    }
    
    if(length(object$TimeStamp)>2880 & length(object$TimeStamp)<=4320){
      hourmark <- ifelse(min=="00"  & ((hour=="00")|(hour=="12")),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(object$TimeStamp)))
      axis(2)
    }
    
    if(length(object$TimeStamp)>4320 & length(object$TimeStamp)<=7200){
      hourmark <- ifelse(min=="00"  & ((hour=="00")),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(object$TimeStamp)))
      axis(2)
    }
    
    if(length(object$TimeStamp)>7200){
      hourmark <- ifelse(min=="00"  & (hour=="00"),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(object$TimeStamp)))
      axis(2)
    }
    
    title(main="Plot of accelerometer data",cex.main=1.5)
    title(ylab="Acceleration counts")
    title(xlab="Time",line=5)
    
    # Create a legend 
    
    if(names(object[2])!="counts"){
      legend(1*length(object$TimeStamp)/10, max(object$x,object$y,object$z)+670, 
             c(expression(italic("x axis"))), lty=1, bty = "n", col = "#1b9e77",
             text.col = "#1b9e77", cex=0.8, horiz = TRUE, #xpd=TRUE, 
             , xjust = 0.01)
      
      legend(4*length(object$TimeStamp)/10, max(object$x,object$y,object$z)+670, 
             c(expression(italic("y axis"))), lty=1, bty = "n", col = "#d95f02",
             text.col = "#d95f02", cex=0.8, horiz = TRUE, #xpd=TRUE, 
             , xjust = 0.01)
      
      legend(7*length(object$TimeStamp)/10, max(object$x,object$y,object$z)+670, 
             c(expression(italic("z axis"))), lty=1, bty = "n", col = "#7570b3",
             text.col = "#7570b3", cex=0.8, horiz = TRUE, #xpd=TRUE, 
             , xjust = 0.01)
    }
    
  }
}