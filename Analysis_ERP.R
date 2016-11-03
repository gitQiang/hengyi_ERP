setwd("D:/data/恒逸ERP数据/data/ExportTables")
source("D:/code/ERP_dataANA/misc.R")
library(lubridate)
library(nor1mix)
library(zoo)

## kehu number =======
data0 <- read.delim("AllinOne.txt")

plot2Time(data0,d1="公司",d2="客户")

plot2Time(data0,d1="公司",d2="金额")

## tong chanpin, tong dengji, liangda shifou jiage pianyi ======
#data1 <- read.delim("hengyi_erp_chanpin.txt")
data1 <- read.delim("AllinOne.txt")
#data1 <- data1[data1[,"品种"]=="逸盛PTA", ]

PrV <- MarketPr()
usedate <- rownames(PrV)
Prods <- colnames(PrV)
outInd <- list()

for(i in 1:ncol(PrV)){
        tmp <- data1[grepl(Prods[i],data1[,"品种"]), ]
        prInd <- tmp[,"金额"]/(tmp[,"数量"]/1000)
        nasub <- is.na(PrV[match(as.character(tmp[,"日期"]), usedate),i])
        tmpsub <- PrV[match(as.character(tmp[,"日期"]), usedate),i]
        prInd[!nasub] <- prInd[!nasub]/tmpsub[!nasub]
        prInd[nasub] <- NA #mean(prInd[!nasub])
        prInd <- na.approx(prInd,maxgap = length(prInd))
        prInd[is.na(prInd)] <- mean(prInd[!is.na(prInd)])
        NumPTA <- tmp[,"数量"]/1000
        
        #prInd <- prInd[!nasub]
        #NumPTA <- NumPTA[!nasub] 
        
        #print(cor(prInd, NumPTA))
        #print(cor.test(prInd, NumPTA))
        plot(prInd,type="l",col=i, main=Prods[i])
        if(i==3) plot(prInd[prInd<4],type="l",col=i, main=Prods[i])
        print(sum(nasub)/nrow(tmp))
        
        plot(density(prInd))
        if(i==1){
                m2 <- norMixMLE(prInd,m=2,trace = 1)
                xx <- seq(min(prInd),max(prInd),0.0001)
                res <- dpnorMix(xx,m2)$p
                pcut <- 0.001
        
                u1 <- xx[min(which(res > 1-pcut))]
                d1 <- xx[max(which(res < pcut))]
        }else{
                u1 <- mean(prInd) + 3*sd(prInd)
                d1 <- mean(prInd) - 3*sd(prInd)      
        }
        
        subind <- which(prInd > u1 | prInd < d1)
        tmpOut <- tmp[subind, ]
        prOut <- tmpOut[,"金额"]/(tmpOut[,"数量"]/1000)
        indOut <- prInd[subind]
        
        write.table(cbind(tmpOut, prOut, indOut, subind),file=paste("PriceOutlier/OutlierOrders_", Prods[i],".txt",sep=""), quote=FALSE, row.names=FALSE, sep="\t" )
        
        outInd[[i]] <- prInd
}
save(outInd,file="outInd")

## fukuanfangshi biLi, butong chanpin ==========
data0 <- read.delim("AllinOne.txt")
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

uniProd <- paste(data0[,"品种"],tmp,sep="_")
tmpsub <- data0[,"付款方式"]=="立即付款"
tmp1 <- data0[tmpsub, ]
tmp2 <- data0[!tmpsub, ]

## whole ratio without time information=======
oneb1 <- aggregate(tmp1[,"数量"],list(uniProd[tmpsub]),sum)
oneb2 <- aggregate(tmp1[,"金额"],list(uniProd[tmpsub]),sum)
oneb3 <- aggregate(tmp2[,"数量"],list(uniProd[!tmpsub]),sum)
oneb4 <- aggregate(tmp2[,"金额"],list(uniProd[!tmpsub]),sum)
barData <- matrix(0,length(unique(uniProd)),4)
rownames(barData) <- oneb4[,1]
barData[,1] <- oneb1[match(oneb4[,1],oneb1[,1]),2]
barData[,2] <- oneb3[match(oneb4[,1],oneb3[,1]),2]
barData[,3] <- oneb2[match(oneb4[,1],oneb2[,1]),2]
barData[,4] <- oneb4[,2]
barData[is.na(barData)] <- 0
#barData <- log(barData)
for(i in 1:nrow(barData)){
        barData[i,1:2] <- barData[i,1:2]/sum(barData[i,1:2])
        barData[i,3:4] <- barData[i,3:4]/sum(barData[i,3:4])
}

# Stacked Bar Plot with Colors and Legend ========
barplot(t(barData[1:7,1:2]), main="立即付款 or not (DTY Number)",
        xlab="", col=c("darkblue","red"),
        legend = c("Yes", "No"))

barplot(t(barData[8:11,1:2]), main="立即付款 or not (FDY Number)",
        xlab="", col=c("darkblue","red"),
        legend = c("Yes", "No"))


barplot(t(barData[12:16,1:2]), main="立即付款 or not (POY Number)",
        xlab="", col=c("darkblue","red"),
        legend = c("Yes", "No"))
### jin e

barplot(t(barData[1:7,3:4]), main="立即付款 or not (DTY amount)",
        xlab="", col=c("darkblue","red"),
        legend = c("Yes", "No"))

barplot(t(barData[8:11,3:4]), main="立即付款 or not (FDY amount)",
        xlab="", col=c("darkblue","red"),
        legend = c("Yes", "No"))

barplot(t(barData[12:16,3:4]), main="立即付款 or not (POY amount)",
        xlab="", col=c("darkblue","red"),
        legend = c("Yes", "No"))

## lijiFukuan zhanbi Bodong =======
ProdsLevs <- unique(uniProd)
Prods1 <- uniProd[tmpsub]
for(i in 1:length(ProdsLevs)){
        prodsub <- Prods1 == ProdsLevs[i]
        if(sum(prodsub)>0){
                print(i)
                oneprod <- as.matrix(tmp1[prodsub, c("数量","金额")])
                amProd <- trans2group(oneprod,k=2, tmp1[prodsub, "日期"])
                tosub <- uniProd == ProdsLevs[i]
                onetoprod <- as.matrix(data0[tosub, c("数量","金额")])
                toProd <- trans2group(onetoprod,k=2, data0[tosub, "日期"])
                inter <- intersect(rownames(amProd), rownames(toProd))
                plot(amProd[inter, 2]/toProd[inter, 2],col=1,main=ProdsLevs[i], type="b", ylab="立即付款 ratio", xlab="", xaxt="n", ylim=c(0,1))
                text(x=1:length(inter), par("usr")[3]-0.05, labels = inter, srt = 90, pos = 1, xpd = TRUE) 
        }
}

#### top orders in themselves, and ratios ========
kehu <- read.delim("../kehu.txt")
sefsubs <- grepl("恒逸", kehu[,2]) | grepl("逸盛", kehu[,2])
sefCode <- kehu[sefsubs, ]

res <- topOrders(sefCode[,1],ntop=100)

plot(res[[4]][,2],type="p", xlab="", ylab="Ratio")

## price index without outside data=========
data0 <- read.delim("AllinOne.txt")
data0 <- data0[data0[,"金额"] > 0, ]
data0[,"金额"] <- log(data0[,"金额"])

Prods <- c("PTA","DTY","FDY","POY")
dkehuList <- list()
for(i in 1:length(Prods)){
        tmp <- data0[grepl(Prods[i],data0[,"品种"]), ]
        kehus <- unique(tmp[,"客户"])
        useDates <- as.Date(tmp[,"日期"])
        useMon <- paste(year(useDates),month(useDates),sep="-")
        rowMon  <- unique(useMon)
        print(length(kehus)) 
               
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

save(dkehuList,file="distanceCustomer")

## get price index =======
library(MASS)
data0 <- read.delim("AllinOne.txt")
data0[data0[,"金额"] <= 0,"金额"] <- 1
data0[,"金额"] <- log(data0[,"金额"])

Prods <- c("PTA","DTY","FDY","POY")
load("distanceCustomer")

### only one order for the same quantify
for(i in 1:length(Prods)){
        oneD <- dkehuList[[i]]
        kehus <- colnames(oneD)
        
        aa <- which(oneD==0,arr.ind = TRUE)
        aa <- aa[aa[,1]>aa[,2], ,drop=FALSE]
        onetmp <- data0[grepl(Prods[i],data0[,"品种"]),  ]
        
        if(nrow(aa) > 0){
                for(j in 1:nrow(aa)){
                        oneSame <- onetmp[onetmp[,"客户"] %in% c(kehus[aa[j,1]],kehus[aa[j,2]]), ]
                        write.table(oneSame, file=paste("SameCustomers/",Prods[i],"_",kehus[aa[j,1]],"_",kehus[aa[j,1]],".txt",sep=""), sep="\t", row.names = FALSE, quote=FALSE)
                }
        }
}

kdis <- 1:length(Prods)
for(i in 1:length(Prods)){
        oneD <- dkehuList[[i]]
        oneD <- as.vector(oneD[upper.tri(oneD, diag = FALSE)])
        fit <- fitdistr(oneD[oneD>0],densfun = "lognormal")
        #plot(density(oneD))
        print(quantile(oneD[oneD>0],0.5))
        kdis[i] <- qlnorm(0.95, fit$estimate[1], fit$estimate[2])
}

kday <- 7
for(i in 1:length(Prods)){
        print(i)
        oneD <- dkehuList[[i]]
        tmp <- data0[grepl(Prods[i],data0[,"品种"]), ]
        kehus <- colnames(oneD)
        useDates <- as.Date(tmp[,"日期"])
        
        kehusL <- lapply(1:length(kehus), function(kk) kehus[oneD[kk, ] <= kdis[i]])
        
        uniDays <- unique(as.character(useDates))
        daysL <- sapply(1:length(uniDays), function(kk) as.character(seq(as.Date(uniDays[kk]) - kday, as.Date(uniDays[kk]) + kday, by="day")))
        colnames(daysL) <- uniDays
        
        prInd <- sapply(1:nrow(tmp), function(kk){
                #oneK <- setdiff(oneK, tmp[kk,"客户"])
                onesub <- (tmp[,"日期"] %in% as.character(daysL[,tmp[kk,"日期"]])) & (tmp[,"客户"] %in% kehusL[[ which(tmp[kk,"客户"]==kehus) ]])
                (tmp[kk,"金额"]/tmp[kk,"数量"])/(mean(tmp[onesub,"金额"]/tmp[onesub,"数量"]))
                })
        
        plot(prInd,type="l",col=i, main=Prods[i])
}
