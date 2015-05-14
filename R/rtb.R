

rtb <- function(data, type, tri='FALSE', spuriousDef=20, nonwearDef=60, 
                boutsizeSed=10, toleranceSed='FALSE',  
                mvpa='Freedson', boutsizeMVPA=10, toleranceMVPA='TRUE'){
  
  
  if(tri=='TRUE'){data$counts <- sqrt(data[,2]^2+data[,3]^2+data[,4]^2)}
  
  if(is.numeric(mvpa)){mvpaCP <- mvpa}
  if(mvpa=='Freedson'){mvpaCP <- 1952}
  if(mvpa=='Swartz'){mvpaCP <- 574}
  if(mvpa=='Copland'){mvpaCP <- 1041}
  if(mvpa=='Hall'){mvpaCP <- 809}
  
  if(toleranceSed=='TRUE'){toleranceSed <- 2}
  if(toleranceMVPA=='TRUE'){toleranceMVPA <- 2}
  if(toleranceSed=='FALSE'){toleranceSed <- 0}
  if(toleranceMVPA=='FALSE'){toleranceMVPA <- 0}
  
  value  <- rep(rle(as.numeric(data$counts))$values, 
                rle(as.numeric(data$counts))$lengths)
  length <- rep(rle(as.numeric(data$counts))$lengths, 
                rle(as.numeric(data$counts))$lengths)
  d1.lag <- cbind(data,value,length,
                  head=head(c(0,length),-1),
                  tail=tail(c(length,0),-1),
                  dif1=head(c(0,length),-1)-length,
                  dif2=tail(c(length,0),-1)-length,
                  actb=head(c(0,data$counts),-1),
                  acta=tail(c(data$counts,0),-1))
  spDef <- spuriousDef -1
  d1s <- cbind(d1.lag,spurious=ifelse(d1.lag$dif1>=spDef & d1.lag$dif2>=spDef & 
                                        d1.lag$actb==0 & d1.lag$acta==0,1,0))
  d1s$counts2 <- ifelse(d1s$spurious==1, 0, d1s$counts) 
  d2 <- data.frame(TimeStamp=d1s$TimeStamp,counts=d1s$counts2)
  value2  <- rep(rle(as.numeric(d2$counts))$values, 
                 rle(as.numeric(d2$counts))$lengths)
  length2 <- rep(rle(as.numeric(d2$counts))$lengths, 
                 rle(as.numeric(d2$counts))$lengths)
  d2w <- cbind(d2,value2,length2)
  nonwear <- ifelse(d2w$value2 == 0 & d2w$length > nonwearDef, 1, 0)
  d2nw <- cbind(d2w,nonwear)
  d3 <- data.frame(TimeStamp = d2nw$TimeStamp, counts = d2nw$counts, nonwear = d2nw$nonwear)
  d3$countsWear <- ifelse(d3$nonwear == 1, NA, d3$counts)
  d3$sedentary <- ifelse(d3$countsWear < 100, 1, 0)
  d3$sedentary2 <- ifelse(is.na(d3$countsWear), 0, d3$sedentary)
  d3$inMVPA <- ifelse(d3$countsWear >= mvpaCP, 1, 0)
  d3$inMVPA2 <- ifelse(is.na(d3$countsWear), 0, d3$inMVPA)
  d3$mydates <- as.factor(as.numeric(strptime(d3$TimeStamp,format='%Y-%m-%d')))
  uniqueDates <- unique(strptime(d3$TimeStamp,format='%Y-%m-%d'))
  
  myRollSum<- function(x, k) { 
    rs <- rollsum(x, k)
    rsp <- c(rs,rep(NA,k-1))
    return(rsp)
  }
  
  myLag<- function(x, k) {
    c(rep(NA, k), x)[1 : length(x)] 
  }
  
  myLagUp<- function(x, k) {
    c(x[(k+1): (length(x))],rep(NA,k))
  }
  
  mybSed <- boutsizeSed - toleranceSed
  mybMVPA <- boutsizeMVPA - toleranceMVPA
  
  
  ##
  ## Get bout calculations: sedentary time
  ##
  
  
  if(type=='sedentary' | type=='sed' | type=='Sedentary'){
    dsid <- data.frame(TimeStamp = d3[,1], counts = d3[,2], nonwear = d3[,3], sedentary2 = d3[,6])
    dsid$sedB <- myRollSum(dsid$sedentary2, boutsizeSed) 
    dsid$sB <- ifelse(dsid$sedB >= mybSed, 1, 0)
    
    #make a lag matrix
    suppressWarnings(rm(bm))
    bm <- matrix(NA, nrow = nrow(dsid), ncol = (boutsizeSed+1))
    bm[,1] <- dsid$sB
    
    for(i in 1:(boutsizeSed-1)){
      bm[,(i+1)] <- myLag(dsid$sB,i)
    }
    
    bm[,ncol(bm)] <- rowSums(bm[,1:(ncol(bm)-1)], na.rm=TRUE)    
    dsid$inbout <- ifelse(bm[,ncol(bm)]>=1, 1, 0)    
    
    dsid$inboutLagb1 <- myLag(dsid$inbout,1)
    dsid$inboutLagb2 <- myLag(dsid$inbout,2)
    dsid$sedLagb1 <- myLag(dsid$sedentary2,1)
    dsid$inboutUpLagb1 <- myLagUp(dsid$inbout,1)
    dsid$inboutUpLagb2 <- myLagUp(dsid$inbout,2)
    dsid$sedLagUpb1 <- myLagUp(dsid$sedentary2,1)
    
    dsid$inbout[dsid$inbout==1 & dsid$inboutLagb1==0 & dsid$sedentary2==0] <- 0    
    dsid$inbout[dsid$inbout==1 & dsid$inboutLagb2==0 & dsid$sedLagb1==0 & dsid$sedentary2==0] <- 0 
    
    dsid$inbout[dsid$inbout==1 & dsid$inboutUpLagb1==0 & dsid$sedentary2==0] <- 0    
    dsid$inbout[dsid$inbout==1 & dsid$inboutUpLagb2==0 & dsid$sedLagUpb1==0 & dsid$sedentary2==0] <- 0 
    
    dsid$value  <- rep(rle(as.numeric(dsid$inbout))$values, 
                       rle(as.numeric(dsid$inbout))$lengths)
    dsid$length <- rep(rle(as.numeric(dsid$inbout))$lengths, 
                       rle(as.numeric(dsid$inbout))$lengths)
    
    dsid$valueLag1 <- myLag(dsid$value,1)
    dsid$lengthLag1 <- myLag(dsid$length,1)
    dsid$first <- ifelse(dsid$value == dsid$valueLag1 & dsid$length == dsid$lengthLag1 , 0, 1)                          
    dsid$first[1] <- ifelse(dsid$inbout[1] == 1, 1, 0)
    
    dsid$valueLag2 <- myLag(dsid$value,2)
    dsid$lengthLag2 <- myLag(dsid$length,2)
    dsid$second <- ifelse(dsid$valueLag1 == dsid$valueLag2 & dsid$lengthLag1 == dsid$lengthLag2 , 0, 1)                          
    dsid$second[2] <- ifelse(dsid$inbout[2] == 1, 1, 0)
    
    dsid$valueLag3 <- myLag(dsid$value,3)
    dsid$lengthLag3 <- myLag(dsid$length,3)
    dsid$third <- ifelse(dsid$valueLag2 == dsid$valueLag3 & dsid$lengthLag2 == dsid$lengthLag3 , 0, 1)                          
    dsid$third[3] <- ifelse(dsid$inbout[3] == 1, 1, 0)
    
    dsid$wear <- ifelse(dsid$nonwear==0,1,0)
    
    dsid$firstBout <- ifelse(dsid$first == 1& dsid$inbout == 1 & dsid$wear == 1  & dsid$length>=boutsizeSed, 1, 0)  # View(dsid)
    
    
    rtdSed <- dsid[ which(dsid$firstBout==1), ] # View(rtdSed) #length
    
    summary <- data.frame(TimeStamp = rtdSed$TimeStamp, 
                          Minutes = rtdSed$length) # View(rtdSedentary)
  }
  
  ##
  ## Get bout calculations: MVPA time
  ##
  
  if(type=='mvpa' | type=='MVPA' | type == 'Mvpa'){
    dsiMVPA <- d3
    dsidMVPA <- data.frame(TimeStamp = dsiMVPA[,1], counts = dsiMVPA[,2], nonwear = dsiMVPA[,3], inMVPA2 = dsiMVPA[,8])
    dsidMVPA$mvpaB <- myRollSum(dsidMVPA$inMVPA2, boutsizeMVPA)
    dsidMVPA$mvB <- ifelse(dsidMVPA$mvpaB >= mybMVPA, 1, 0)
    
    suppressWarnings(rm(bm))
    bm <- matrix(NA, nrow = nrow(dsidMVPA), ncol = (boutsizeMVPA+1))
    bm[,1] <- dsidMVPA$mvB
    
    for(i in 1:(boutsizeMVPA-1)){
      bm[,(i+1)] <- myLag(dsidMVPA$mvB,i)
    }
    
    bm[,ncol(bm)] <- rowSums(bm[,1:(ncol(bm)-1)], na.rm=TRUE)  
    
    dsidMVPA$inbout <- ifelse(bm[,ncol(bm)]>=1, 1, 0)   
    
    dsidMVPA$inboutLagb1 <- myLag(dsidMVPA$inbout,1)
    dsidMVPA$inboutLagb2 <- myLag(dsidMVPA$inbout,2)
    dsidMVPA$MVPALagb1 <- myLag(dsidMVPA$inMVPA2,1)
    dsidMVPA$inboutUpLagb1 <- myLagUp(dsidMVPA$inbout,1)
    dsidMVPA$inboutUpLagb2 <- myLagUp(dsidMVPA$inbout,2)
    dsidMVPA$MVPALagUpb1 <- myLagUp(dsidMVPA$inMVPA2,1)
    
    dsidMVPA$inbout[dsidMVPA$inbout==1 & dsidMVPA$inboutLagb1==0 & dsidMVPA$inMVPA2==0] <- 0    
    dsidMVPA$inbout[dsidMVPA$inbout==1 & dsidMVPA$inboutLagb2==0 & dsidMVPA$MVPALagb1==0 & dsidMVPA$inMVPA2==0] <- 0 
    
    dsidMVPA$inbout[dsidMVPA$inbout==1 & dsidMVPA$inboutUpLagb1==0 & dsidMVPA$inMVPA2==0] <- 0    
    dsidMVPA$inbout[dsidMVPA$inbout==1 & dsidMVPA$inboutUpLagb2==0 & dsidMVPA$MVPALagUpb1==0 & dsidMVPA$inMVPA2==0] <- 0 
    
    dsidMVPA$value  <- rep(rle(as.numeric(dsidMVPA$inbout))$values, 
                           rle(as.numeric(dsidMVPA$inbout))$lengths)
    dsidMVPA$length <- rep(rle(as.numeric(dsidMVPA$inbout))$lengths, 
                           rle(as.numeric(dsidMVPA$inbout))$lengths)
    dsidMVPA$valueLag <- myLag(dsidMVPA$value)
    dsidMVPA$lengthLag <- myLag(dsidMVPA$length)
    dsidMVPA$first <- ifelse(dsidMVPA$value == dsidMVPA$valueLag & dsidMVPA$length == dsidMVPA$lengthLag , 0, 1)
    dsidMVPA$first[1] <- ifelse(dsidMVPA$inbout[1] == 1, 1, 0)
    dsidMVPA$wear <- ifelse(dsidMVPA$nonwear==0,1,0)
    dsidMVPA$firstBout <- ifelse(dsidMVPA$first == 1& dsidMVPA$inbout == 1 & dsidMVPA$wear == 1 & dsidMVPA$length>=boutsizeMVPA, 1, 0)
    
    
    rtdMVPA <- dsidMVPA[ which(dsidMVPA$firstBout==1), ] # View(rtdMVPA) #length
    
    summary <- data.frame(TimeStamp = rtdMVPA$TimeStamp, 
                          Minutes = rtdMVPA$length) # View(rtdMVPA)
  }
  
  summary
}

