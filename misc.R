PriceIndByOut <- function(data1, PrV, Prods, plot=FALSE){
        
        usedate <- rownames(PrV)
        outInd <- list()
        
        u1s <- c(1.397349, 1.62, 1.218344, 1.333677)
        d1s <- c(0.5536494, 0.56,0.52, 0.4192478)
        for(i in 1:ncol(PrV)){
                tmp <- data1[grepl(Prods[i],data1[,"品种"]), ]
                prInd <- tmp[,"金额"]/(tmp[,"数量"]/1000)
                nasub <- is.na(PrV[match(as.character(tmp[,"日期"]), usedate),i])
                tmpsub <- PrV[match(as.character(tmp[,"日期"]), usedate),i]
                prInd[!nasub] <- prInd[!nasub]/tmpsub[!nasub]
                prInd[nasub] <- mean(prInd[!nasub])
                NumPTA <- tmp[,"数量"]/1000
                
                if(plot){
                        plot(density(prInd))
                        #print(cor(prInd, NumPTA))
                        #print(cor.test(prInd, NumPTA))
                        plot(prInd,type="l",col=i, main=Prods[i])
                        if(i==3) plot(prInd[prInd<4],type="l",col=i, main=Prods[i])
                        print(sum(nasub)/nrow(tmp))
                }
                
                if(i==1){
                        pcut <- 0.001
                        m2 <- norMixMLE(prInd,m=2,trace = 1)
                        xx <- seq(min(prInd),max(prInd),0.0001)
                        res <- dpnorMix(xx,m2)$p
                        
                        u1 <- xx[min(which(res > 1-pcut))]
                        d1 <- xx[max(which(res < pcut))]
                }else{
                        pcut <- 0.0001
                        u1 <- u1s[i] #quantile(prInd[prInd>0], 1-pcut) #mean(prInd) + 6*sd(prInd)
                        d1 <- d1s[i] #quantile(prInd[prInd>0], pcut)  #mean(prInd) - 6*sd(prInd)      
                }
                
                if(plot){
                        abline(h=d1,lty=2,col=2)
                        abline(h=u1,lty=2,col=2)
                }
                
                subind <- which(prInd > u1 | prInd < d1)
                tmpOut <- tmp[subind, ]
                prOut <- tmpOut[,"金额"]/(tmpOut[,"数量"]/1000)
                indOut <- prInd[subind]
                
                write.table(cbind(tmpOut, prOut, indOut, subind),file=paste("PriceOutlier/OutlierOrders_", Prods[i],".txt",sep=""), quote=FALSE, row.names=FALSE, sep="\t" )
                
                outInd[[i]] <- prInd
        }
        
        outInd
        
}

PriceIndByIn <- function(data0, dkehuList, Prods, plot=FALSE){
        
        kday <- 7
        ntop <- 5
        inInd <- list()
        for(i in 1:length(Prods)){
                print(i)
                oneD <- dkehuList[[i]]
                diag(oneD) <- Inf #oneK <- setdiff(oneK, tmp[kk,"客户"])
                tmp <- data0[grepl(Prods[i],data0[,"品种"]), ]
                kehus <- colnames(oneD)
                useDates <- as.Date(tmp[,"日期"])
                
                #kehusL <- lapply(1:length(kehus), function(kk) kehus[oneD[kk, ] <= kdis[i]]) ##v1
                kehusL <- lapply(1:length(kehus), function(kk) kehus[rank(oneD[kk, ]) <= ntop]) ##v2
                
                
                uniDays <- unique(as.character(useDates))
                daysL <- sapply(1:length(uniDays), function(kk) as.character(seq(as.Date(uniDays[kk]) - kday, as.Date(uniDays[kk]) + kday, by="day")))
                colnames(daysL) <- uniDays
                
                prInd <- sapply(1:nrow(tmp), function(kk){
                        onesub <- (tmp[,"日期"] %in% as.character(daysL[,tmp[kk,"日期"]])) & (tmp[,"客户"] %in% kehusL[[ which(tmp[kk,"客户"]==kehus) ]])
                        (tmp[kk,"金额"]/tmp[kk,"数量"])/(mean(tmp[onesub,"金额"]/tmp[onesub,"数量"]))
                })
                
                if(plot){
                        plot(prInd,type="l",col=i, main=Prods[i])
                        plot(density(prInd),col=i)
                }
                
                prInd[is.na(prInd)] <- 1
                u1 <- mean(prInd) + 3*sd(prInd)
                d1 <- mean(prInd) - 3*sd(prInd)     
                subind <- which(prInd > u1 | prInd < d1)
                tmpOut <- tmp[subind, ]
                prOut <- tmpOut[,"金额"]/(tmpOut[,"数量"]/1000)
                indOut <- prInd[subind]
                
                write.table(cbind(tmpOut, prOut, indOut, subind),file=paste("PriceOutlier/OutlierOrders_In_", Prods[i],".txt",sep=""), quote=FALSE, row.names=FALSE, sep="\t" )
                
                inInd[[i]] <- prInd
        }
        
        #save(inInd,file="inInd_V2")       
        inInd
}

GetCustomerDisByProduct <- function(data0, Prods){
        
        data0 <- data0[data0[,"金额"] > 0, ]  ## del zero money orders
        data0[,"金额"] <- log(data0[,"金额"])
        
        dkehuList <- list()
        for(i in 1:length(Prods)){
                tmp <- data0[grepl(Prods[i],data0[,"品种"]), ]
                kehus <- unique(tmp[,"客户"])
                useDates <- as.Date(tmp[,"日期"])
                useMon <- paste(year(useDates),month(useDates),sep="-")
                rowMon  <- unique(useMon)
                #print(length(kehus)) 
                
                feaKehu <- sapply(1:length(kehus), function(kk){
                        onesub <- tmp[,"客户"]==kehus[kk]
                        oneV <- aggregate(tmp[onesub,"金额"],list(useMon[onesub]),sum)
                        oneV[match(rowMon,oneV[,1]),2]
                })
                
                colnames(feaKehu) <- kehus
                rownames(feaKehu) <- rowMon
                print(sum(is.na(feaKehu))/(length(feaKehu)))
                feaKehu[is.na(feaKehu)] <- 0
                
                dkehu <- as.matrix(dist(t(feaKehu)))
                
                dkehuList[[i]] <- dkehu
        }
        
        #save(dkehuList,file="distanceCustomer_V2")  
        dkehuList
        
}

delOutliers <- function(data0, filename="AllinOneNew.txt"){
        
        data0 <- data0[data0[,"金额"] > 0, ]
        
        kehu <- read.delim("D:/data/恒逸ERP数据/data/kehu.txt")
        sefsubs <- grepl("恒逸", kehu[,2]) | grepl("逸盛", kehu[,2])
        sefCode <- kehu[sefsubs, ]
        data0 <- data0[!(data0[,"客户"] %in% sefCode[,1]), ]
        
        filenames <- c("PriceOutlier/OutlierOrders_PTA.txt","PriceOutlier/OutlierOrders_FDY.txt","PriceOutlier/OutlierOrders_DTY.txt","PriceOutlier/OutlierOrders_POY.txt")
        badLab <- c()
        for(i in 1:length(filenames)){
                out1 <- read.delim(filenames[i])
                badLab <- c(badLab, paste(out1[,"客户"], out1[,"订单"],sep="_"))
        }
        aa1 <- paste(data0[,"客户"], data0[,"订单"],sep="_")
        data0 <- data0[!(aa1 %in% badLab), ]
        
        write.table(data0,file=filename,sep="\t",row.names = FALSE,quote = FALSE)
        
}

productLevel <- function(data0){
        tmp <- sapply(1:nrow(data0), function(ti) unlist(strsplit(data0[ti,"物料描述"],"-"))[4])
        ## adjust to A, AA, B, C
        tmp[grepl("壹等",tmp)] <- "A"
        tmp[grepl("优等",tmp)] <- "AA"
        tmp[grepl("合格",tmp)] <- "B"
        tmp[grepl("等外",tmp)] <- "C"
        ## there is no ranks information for PTA
        tmp[is.na(tmp)] <- "PTA"
        tmp <- gsub("\\d","",tmp)
        tmp <- gsub("\\.","",tmp)
        names(tmp) <- "产品等级" 
        
        tmp
}

GetFeatureM <- function(data0, Prods, di=1){
        ## di: 0 week; 1 month; 2 season
        
        useDates <- as.Date(data0[,"日期"])
        if(di==0){
                useMon <- paste(year(useDates),week(useDates),sep="-")
        }else if(di==1){
                useMon <- paste(year(useDates),month(useDates),sep="-")
        }else if(di==2){
                useMon <- paste(year(useDates),quarter(useDates),sep="-")        
        }
        rowMon  <- unique(useMon)
        
        data0[,"金额"] <- log(data0[,"金额"])
        
        kehua <- unique(data0[,"客户"])
        nm <- length(rowMon)
        feaM  <- matrix(0,length(kehua), length(Prods) * nm)
        rownames(feaM) <- kehua
        
        for(i in 1:length(Prods)){
                tmp <- data0[grepl(Prods[i],data0[,"品种"]), ]
                kehus <- unique(tmp[,"客户"])
                useDates <- as.Date(tmp[,"日期"])
                
                if(di==0){
                        useMon <- paste(year(useDates),week(useDates),sep="-")
                }else if(di==1){
                        useMon <- paste(year(useDates),month(useDates),sep="-")
                }else if(di==2){
                        useMon <- paste(year(useDates),quarter(useDates),sep="-")        
                }
                
                feaKehu <- sapply(1:length(kehus), function(kk){
                        onesub <- tmp[,"客户"]==kehus[kk]
                        oneV <- aggregate(tmp[onesub,"金额"],list(useMon[onesub]),sum)
                        oneV[match(rowMon,oneV[,1]),2]
                })
                
                colnames(feaKehu) <- kehus
                rownames(feaKehu) <- rowMon
                print(sum(is.na(feaKehu))/(length(feaKehu)))
                feaKehu[is.na(feaKehu)] <- 0
                
                feaM[as.character(kehus), ((i-1)*nm+1):(i*nm)] <- t(feaKehu)
        } 
        
        feaM
}

GetDistanceM <- function(data0, Prods, di=1){
      
        feaM <- GetFeatureM(data0, Prods, di=di)
        oneD <- dist(feaM)
        #disM <- as.matrix(oneD)
        #save(oneD,file = "dist_10927")
        
        oneD
}

HierCluster <- function(oneD, nc=10){
        
        disM <- as.matrix(oneD)
        h1 <- hclust(oneD)
        labs <- cutree(h1,k=nc)   
        cluS <- cbind(rownames(disM),labs)
        
        # hierScore <- read.delim("D:/data/恒逸ERP数据/data/hengyi_diaoyan1316_rating_composite.txt")[ ,c(2,9)]
        # kehu <- read.delim("D:/data/恒逸ERP数据/data/kehu.txt")
        # 
        # hierOrder <- read.delim("D:/data/恒逸ERP数据/data/hengyi_diaoyan1316_rating_composite.txt")[ ,c(2,8,9)]
        # hierOrder <- hierOrder[match(rownames(disM),hierOrder[,1]), ]
        # tmp <- sort(hierOrder[,2],decreasing = TRUE, index.return=TRUE)
        # o2 <- tmp$ix
        # o1 <- h1$order
        # wilcox.test(o1,o2)
        cluS
}

quantileCluster <- function(feaM,nc=5){
        
        n.col <- ncol(feaM)
        cCenter <- matrix(0,nc,n.col)
        for(i in 1:n.col){
                x <- feaM[,i]
                x <- x[x>0]
                cCenter[,i] <- quantile(x,probs = seq(0,1,length.out = nc+1)[-1])
        }
        
        n.row <- nrow(feaM)
        disM <- sapply(1:n.row, function(ii) sapply(1:nc, function(ii1) dist(rbind(feaM[ii,],cCenter[ii1, ]))[1]))
        labs <- sapply(1:n.row, function(ii) which.min(disM[, ii]))
        cluS1 <- cbind(rownames(feaM), labs)
        
        cluS1
}

PriceIndSumUp <- function(data1, PrV, Prods, plot=FALSE){
        
        usedate <- rownames(PrV)
        outIndDay <- list()
        for(i in 1:ncol(PrV)){
                tmp <- data1[grepl(Prods[i],data1[,"品种"]), ]
                prInd <- tmp[,"金额"]/(tmp[,"数量"]/1000)
                nasub <- is.na(PrV[match(as.character(tmp[,"日期"]), usedate),i])
                tmpsub <- PrV[match(as.character(tmp[,"日期"]), usedate),i]
                prInd[!nasub] <- prInd[!nasub]/tmpsub[!nasub]
                prInd[nasub] <- mean(prInd[!nasub])
                
                useDates <- as.Date(tmp[,"日期"])
                useDay <- paste(year(useDates),week(useDates),sep="-") #useDates
                
                oneV <- aggregate(prInd,list(useDay), mean)
                rownames(oneV) <- useDates[match(oneV[,1], useDay)]
                oneV <- oneV[order(as.Date(rownames(oneV))), ]
                outIndDay[[i]] <- oneV
                
                if(plot){
                        plot(oneV[,2],type="l",col=i, main=Prods[i])
                        abline(h=mean(oneV[,2]),lty=2,col=2)
                        
                        twoV <- aggregate(tmp[,"数量"],list(useDay), sum)
                        R2y(1:nrow(oneV),oneV[,2],twoV[,2]*oneV[,2],led="bottomleft",legend=c("PriceInd","amount"),type="l",abl=TRUE)
                        print(cor.test(oneV[,2],twoV[,2]))
                }
        }        
        
        outIndDay
}

BestPriceInd <- function(data1, Prod, oneV, nweek=100, plot=FALSE){
        
        tmp <- data1[grepl(Prod,data1[,"品种"]), ]
        useDates <- as.Date(tmp[,"日期"])
        useDay <- paste(year(useDates),week(useDates),sep="-") #useDates
        twoV <- aggregate(tmp[,"金额"],list(useDay), sum)
        rownames(twoV) <- useDates[match(twoV[,1], useDay)]
        twoV <- twoV[order(as.Date(rownames(twoV))), ]
        
        a1 <- oneV[max(1, nrow(oneV)-nweek+1):nrow(oneV),2] 
        b1 <- twoV[max(1, nrow(twoV)-nweek+1):nrow(twoV),2]
        
        a2 <- diff(a1)  ## delta price index
        b2 <- diff(log(b1)) ## delta log sale-money
        y <- b2/abs(a2)
        asub <- sort(a1[-length(a1)],index.return=TRUE)$ix
        
        if(plot) plot(a1[asub],y[asub],xlab="Price Index",ylab="Amount_d/Index_d",type="l",col=2)
        
        x1 <- a1[asub]
        y1 <- y[asub]
        chpi <- x1[which.max(y1)]
        sdi <- sd(x1)
        ui <- chpi + sdi
        di <- chpi - sdi
        b0 <- mean(x1)
        
        c(chpi,ui,di,b0)
}


## useful functions
write2Database <- function(tdata,tname,db="mathes_version3"){
        library(DBI)
        library(RMySQL)
        conn <- dbConnect(MySQL(), user="etlmathes", password="yAUJ4c", host="172.16.2.244", dbname=db)
        #dbSendQuery(conn,'SET NAMES GBK')
        #dbRemoveTable(conn,"hengyi_erp")
        dbWriteTable(conn, tname,  as.data.frame(tdata), row.names=FALSE)
        dbDisconnect(conn)
}

write2TableFile <- function(tmp,filename,path="D:/data/恒逸ERP数据/data/ExportTables/"){
        write.table(tmp,file = paste(path,filename,sep=""), row.names = FALSE, sep="\t", quote =  FALSE)   
}

trans2group <- function(data0,k=0, useDates, f=1){
        
        mode(data0) <- "numeric"
        useDates <- as.Date(useDates)
        
        if(k==0){
                gs <- useDates
        }else if(k==1){
                gs <- paste(year(useDates),week(useDates),sep="-")
        }else if(k==2){
                gs <- paste(year(useDates),month(useDates),sep="-")
        }else if(k==3){
                gs <- paste(year(useDates),quarter(useDates),sep="-")       
        }
        ws <- unique(gs)
        
        if(f==1) tmpdata <- sapply(1:ncol(data0), function(i)  sapply(ws, function(ix) sum(data0[gs==ix,i])) )
        
        if(f==2) tmpdata <- sapply(1:ncol(data0), function(i)  sapply(ws, function(ix) mean(data0[gs==ix,i])) )
        
        if(f==3)  tmpdata <- sapply(1:ncol(data0), function(i)  sapply(ws, function(ix) length(unique(data0[gs==ix,i]))) )
        
        colnames(tmpdata) <- colnames(data0)
        rownames(tmpdata) <- as.character(ws)
        
        tmpdata
}  

plot2Time <- function(data0,d1="公司",d2="客户",plot=TRUE,k=2){
        gongsi <- unique(data0[,d1])
        n.row <- length(gongsi)
        useDate <- as.Date(data0[,"日期"])
        
        if(k==1) DateMon <- paste(year(useDate), week(useDate), sep="-")
        if(k==2) DateMon <- paste(year(useDate), month(useDate), sep="-")
        if(k==3) DateMon <- paste(year(useDate), quarter(useDate), sep="-")
        
        shijian <- unique(DateMon)
        n.col <- length(shijian)
        
        kehuNum <- matrix(0,n.row,n.col,dimnames=list(gongsi,shijian))
        for(i in 1:n.row){
                for(j in 1:n.col){
                        if(d2=="客户"){
                                kehuNames <- unique(data0[data0[,d1]==gongsi[i]  & DateMon==shijian[j],d2])
                                kehuNum[i,j] <- length(unique(kehuNames))
                        }
                        if(d2 %in% c("数量","金额")){
                                kehuNum[i,j] <- sum(as.numeric(data0[data0[,d1]==gongsi[i]  & DateMon==shijian[j],d2]))
                                #kehuNum[i,j] <- log(kehuNum[i,j])
                        }
                }
        }
        
        #if(max(kehuNum[n.row, ]) * 10 < max(kehuNum)) kehuNum[n.row, ] <- kehuNum[n.row, ] * 15
        
        if(plot){
                ymax <- 1.2*max(kehuNum)
                ymin <- 0.9*min(kehuNum)
                for(i in 1:n.row){
                        if(i==1) plot(kehuNum[i, ], type="b", col=i, ylim = c(ymin, ymax) , xaxt="n", xlab="",ylab="")
                        if(i>1) lines(kehuNum[i,],type="b", col=i)
                }
                legend("topleft",legend = gongsi, col = 1:n.row, lwd=2, lty=1)
                text(x=c(seq(6,n.col-8,length.out = 3), n.col-1), par("usr")[3]-0.051*(ymax-ymin), labels = 2013:2016, srt = 0, pos = 1, xpd = TRUE)
        }
        
        kehuNum
}

MarketPr <- function(){
        
        usedate <- seq(as.Date("2013/1/1"), as.Date("2016/3/1"), "day")
        usedate <- as.character(usedate)
        
        PTAPr <- read.delim("D:/data/恒逸ERP数据/data/PTA价格/PTA市场价.txt",sep="\t")
        PTAPr <- PTAPr[-(1:5), ]
        others <- read.delim("D:/data/恒逸ERP数据/data/Choice_DTY_POY_FDY.txt",sep="\t")
        others <- others[-(1:5), ]
        
        PrV <- matrix(NA,length(usedate),4,dimnames = list(usedate,c("PTA","DTY","FDY","POY")))
        PrV[,1] <- as.numeric(PTAPr[match(usedate,PTAPr[,1]),2])
        PrV[,2] <- as.numeric(others[match(usedate,others[,1]),2])
        PrV[,3] <- as.numeric(others[match(usedate,others[,1]),5])
        PrV[,4] <- as.numeric(others[match(usedate,others[,1]),8])
        
        nasub <- which(is.na(PrV),arr.ind = TRUE)
        nasub <- nasub[nasub[,1] > 1, ]
        nasub <- nasub[order(nasub[,1]), ]
        for(i in 1:nrow(nasub)) PrV[nasub[i,1],nasub[i,2]] <- PrV[nasub[i,1]-1,nasub[i,2]]
        #PrV[cbind(nasub[,1],nasub[,2])] <- PrV[cbind(nasub[,1]-1,nasub[,2])]
        #rownames(PrV) <- usedate
        #PrV <- as.numeric(PrV)
        #PrV[is.na(PrV)] <- PrV[!is.na(PrV)][1]
        
        PrV
}

topOrders <- function(sefCode,ntop=100){
        filenames <- list.files(path="D:/data/恒逸ERP数据/data/数据表文件/",pattern = ".txt", full.names = TRUE)
        
        res <- list()
        for(i in 1:length(filenames)){
                tmp <- read.delim(filenames[i],sep="\t",strip.white=TRUE)
                tmp <- tmp[!is.na(tmp[,"公司"]), ]
                tmp <- tmp[order(- tmp[,"数量"]), ]
                aa <- sum(tmp[1:ntop,"客户"] %in% sefCode)
                
                if(aa>1){
                        useMon <- sapply(1:nrow(tmp), function(kk) paste(unlist(strsplit(tmp[kk,"日期"],"\\."))[1:2],sep="",collapse = "-"))
                        tmp <- cbind(tmp,useMon)
                        
                        MonAmount <- aggregate(tmp[,"数量"],list(useMon),sum)
                        tmp1 <- tmp[1:ntop, ]
                        tmp1 <- tmp1[tmp1[,"客户"] %in% sefCode, ]
                        tmpr <- 1:aa
                        for(j in 1:aa){
                                tmpr[j] <- tmp1[j,"数量"]/MonAmount[match(tmp1[j,ncol(tmp1)],MonAmount[,1]),2]           
                        }
                        res[[i]] <- cbind(tmp1[,"客户"],tmpr)
                }else{res[[i]] <- c(0,0)}
        }
        
        res
}

R2y <- function(x,y1,y2,led="topright",legend=c("y1","y2"),type="b",xlab="",main="",abl=FALSE, y1lim=NULL, y2lim=NULL){
        
        if(is.null(y1lim)) y1lim <- c(0.9*min(y1), 1.1*max(y1))
        if(is.null(y2lim)) y2lim <- c(0.9*min(y2), 1.1*max(y2))
        
        par(mar=c(5,4,4,5)+.1)
        plot(x,y1,type=type,col="red",ylab=legend[1],xlab=xlab,main=main,ylim = y1lim)
        if(abl) abline(h=mean(y1),lty=2,col=2,lwd=2)
        par(new=TRUE)
        plot(x,y2,type=type,col="blue",xaxt="n",yaxt="n",xlab="",ylab="", ylim=y2lim)
        axis(4)
        mtext(legend[2],side=4,line=3)
        legend(led,col=c("red","blue"),lty=1,legend=legend)   

}

stackedBarplot <- function(barData){
        
        ## Stacked Bar Plot with Colors and Legend
        g1 <- list()
        g1[[1]] <- 1:7
        g1[[2]] <- 8:11
        g1[[3]] <- 12:16
        g2 <- list()
        g2[[1]] <- 1:2
        g2[[2]] <- 3:4
        
        Prods <- c("DTY","FDY", "POY")
        di <- c("number", "amount")
        for(i in 1:3){
                for(j in 1:2){
                        barplot(t(barData[g1[[i]],g2[[j]]]), main=paste("立即付款 or not (",Prods[i], di[j],")",sep=" "), xlab="", col=c("darkblue","red"), legend = c("Yes", "No"), args.legend = list(x="bottomleft"))  
                }
        }

}


