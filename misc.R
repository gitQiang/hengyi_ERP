
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
        
        colnames(tmpdata) <- colnames(data0)
        rownames(tmpdata) <- as.character(ws)
        
        tmpdata
}  

plot2Time <- function(data0,d1="公司",d2="客户"){
        gongsi <- unique(data0[,d1])
        n.row <- length(gongsi)
        useDate <- as.Date(data0[,"日期"])
        DateMon <- paste(year(useDate), month(useDate), sep="-")
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
                                kehuNum[i,j] <- sum(data0[data0[,d1]==gongsi[i]  & DateMon==shijian[j],d2])
                                kehuNum[i,j] <- log(kehuNum[i,j])
                        }
                }
        }
        if(max(kehuNum[n.row, ]) * 10 < max(kehuNum)) kehuNum[n.row, ] <- kehuNum[n.row, ] * 15
        
        ymax <- 1.2*max(kehuNum)
        ymin <- 0.9*min(kehuNum)
        for(i in 1:n.row){
                if(i==1) plot(kehuNum[i, ], type="b", col=i, ylim = c(ymin, ymax) , xaxt="n", xlab="",ylab="")
                if(i>1) lines(kehuNum[i,],type="b", col=i)
        }
        legend("topleft",legend = gongsi, col = 1:n.row, lwd=2, lty=1)
        text(x=c(seq(6,n.col-8,length.out = 3), n.col-1), par("usr")[3]-0.051*(ymax-ymin), labels = 2013:2016, srt = 0, pos = 1, xpd = TRUE)        
        
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

R2y <- function(x,y1,y2,led="topright",legend=c("y1","y2"),type="b"){
        
        par(mar=c(5,4,4,5)+.1)
        plot(x,y1,type=type,col="red")
        par(new=TRUE)
        plot(x, y2,type=type,col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
        axis(4)
        mtext("y2",side=4,line=3)
        legend(led,col=c("red","blue"),lty=1,legend=legend)   
        
        
}

delOutliers <- function(){
        data0 <- read.delim("AllinOne.txt")
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
        
        write.table(data0,file="AllinOneNew.txt",sep="\t",row.names = FALSE,quote = FALSE)
}
