setwd("D:/data/恒逸ERP数据/data/ExportTables")
options(stringsAsFactors = FALSE)
source("D:/code/hengyi_ERP/misc.R")
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
        
        plot(density(prInd))
        #print(cor(prInd, NumPTA))
        #print(cor.test(prInd, NumPTA))
        plot(prInd,type="l",col=i, main=Prods[i])
        if(i==3) plot(prInd[prInd<4],type="l",col=i, main=Prods[i])
        print(sum(nasub)/nrow(tmp))
        
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
        abline(h=d1,lty=2,col=2)
        abline(h=u1,lty=2,col=2)
        
        subind <- which(prInd > u1 | prInd < d1)
        tmpOut <- tmp[subind, ]
        prOut <- tmpOut[,"金额"]/(tmpOut[,"数量"]/1000)
        indOut <- prInd[subind]
        
        write.table(cbind(tmpOut, prOut, indOut, subind),file=paste("PriceOutlier/OutlierOrders_", Prods[i],".txt",sep=""), quote=FALSE, row.names=FALSE, sep="\t" )
        
        outInd[[i]] <- prInd
}
save(outInd,file="outInd")

## time distributions of PTA two normal price index distributions =======
data1 <- read.delim("AllinOne.txt")
PrV <- MarketPr()
usedate <- rownames(PrV)
Prods <- colnames(PrV)

i=1
tmp <- data1[grepl(Prods[i],data1[,"品种"]), ]
prInd <- tmp[,"金额"]/(tmp[,"数量"]/1000)
nasub <- is.na(PrV[match(as.character(tmp[,"日期"]), usedate),i])
tmpsub <- PrV[match(as.character(tmp[,"日期"]), usedate),i]
prInd[!nasub] <- prInd[!nasub]/tmpsub[!nasub]
prInd[nasub] <- mean(prInd[!nasub])

# aa <- prInd
# aa <- aa[aa>0.7 & aa<1.2]
# m2 <- norMixMLE(aa,m=3,trace = 1)
# plot(density(aa))
# abline(v=m2[1,1])
# abline(v=m2[2,1],col=2)
# abline(v=m2[3,1],col=3)

useDates <- as.Date(tmp[,"日期"])
useDay <- paste(year(useDates),quarter(useDates),sep="-") #useDates
oneV <- aggregate(prInd,list(useDay), mean)

plot(oneV[,2],type="l",col=i, main=Prods[i])

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

## Stacked Bar Plot with Colors and Legend ========
barplot(t(barData[1:7,1:2]), main="立即付款 or not (DTY Number)",
        xlab="", col=c("darkblue","red"),
        legend = c("Yes", "No"), args.legend = list(x="bottomleft"))

barplot(t(barData[8:11,1:2]), main="立即付款 or not (FDY Number)",
        xlab="", col=c("darkblue","red"),
        legend = c("Yes", "No"), args.legend = list(x="bottomleft"))


barplot(t(barData[12:16,1:2]), main="立即付款 or not (POY Number)",
        xlab="", col=c("darkblue","red"),
        legend = c("Yes", "No"), args.legend = list(x="bottomleft"))
### jin e

barplot(t(barData[1:7,3:4]), main="立即付款 or not (DTY amount)",
        xlab="", col=c("darkblue","red"),
        legend = c("Yes", "No"), args.legend = list(x="bottomleft"))

barplot(t(barData[8:11,3:4]), main="立即付款 or not (FDY amount)",
        xlab="", col=c("darkblue","red"),
        legend = c("Yes", "No"), args.legend = list(x="bottomleft"))

barplot(t(barData[12:16,3:4]), main="立即付款 or not (POY amount)",
        xlab="", col=c("darkblue","red"),
        legend = c("Yes", "No"), args.legend = list(x="bottomleft"))

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
                inter <- setdiff(rownames(toProd), rownames(amProd))
                aa <- matrix(0,length(inter),2, dimnames = list(inter,colnames(amProd)))
                amProd <- rbind(amProd,aa)
                ti <- rownames(toProd)
                plot(amProd[ti, 1]/toProd[, 1],col=1,main=ProdsLevs[i], type="b", ylab="立即付款 ratio", xlab="", xaxt="n", ylim=c(0,1))
                text(x=1:nrow(toProd), par("usr")[3]-0.1, labels = rownames(toProd), srt = 90, pos = 1, xpd = TRUE) 
        }
}

#### top orders in themselves, and ratios ========
kehu <- read.delim("../kehu.txt")
sefsubs <- grepl("恒逸", kehu[,2]) | grepl("逸盛", kehu[,2])
sefCode <- kehu[sefsubs, ]

res <- topOrders(sefCode[,1],ntop=100)

plot(res[[4]][,2],type="p", xlab="", ylab="Ratio")

## price index without outside data=========
#data0 <- read.delim("AllinOne.txt")
data0 <- read.delim("AllinOneNew.txt")
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

save(dkehuList,file="distanceCustomer_V2")

## get price index =======
library(MASS)
data0 <- read.delim("AllinOne.txt")
#data0[data0[,"金额"] <= 0,"金额"] <- 1
#data0[,"金额"] <- log(data0[,"金额"])

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
        
        plot(prInd,type="l",col=i, main=Prods[i])
        #plot(density(prInd),col=i)
        
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
save(inInd,file="inInd_V2")
        

### kehu clusters =======
data0 <- read.delim("AllinOneNew.txt")
data0[,"金额"] <- log(data0[,"金额"])
useDates <- as.Date(data0[,"日期"])
useMon <- paste(year(useDates),quarter(useDates),sep="-")
rowMon  <- unique(useMon)

Prods <- c("PTA","DTY","FDY","POY")
kehua <- unique(data0[,"客户"])
nm <- length(rowMon)
feaM  <- matrix(0,length(kehua), length(Prods) * nm)
rownames(feaM) <- kehua

for(i in 1:length(Prods)){
        tmp <- data0[grepl(Prods[i],data0[,"品种"]), ]
        kehus <- unique(tmp[,"客户"])
        useDates <- as.Date(tmp[,"日期"])
        useMon <- paste(year(useDates),month(useDates),sep="-")
       
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

oneD <- dist(feaM)
disM <- as.matrix(oneD)
save(oneD,file = "dist_10927")


###  
load("dist_10927")
disM <- as.matrix(oneD)
nc <- 10
h1 <- hclust(oneD)
labs <- cutree(h1,k=nc)   
hierScore <- read.delim("D:/data/恒逸ERP数据/data/hengyi_diaoyan1316_rating_composite.txt")[ ,c(2,9)] #read.delim("../征信评级.txt",header = FALSE, skip=2)[,c(1,3)]
kehu <- read.delim("D:/data/恒逸ERP数据/data/kehu.txt")
cluS <- cbind(rownames(disM),labs)

inNum <- matrix(0,nc,5)
labC <- c("A","B","C","D","E")
for(i in 1:nc){
        for(j in 1:5){
                #tmp <- kehu[match(hierScore[hierScore[,2]==labC[j],1],kehu[,2]),1]
                #inNum[i,j] <- length(intersect(tmp,cluS[labs==as.character(i),1]))
                inNum[i,j] <- length(intersect(hierScore[hierScore[,2]==labC[j],1],cluS[labs==as.character(i),1]))/length(union(hierScore[hierScore[,2]==labC[j],1],cluS[labs==as.character(i),1]))     
        }
}

aa <- table(hierScore[,2])
bb <- rownames(disM)[h1$order]
k=1
inv <- 1:5
for(i in 1:5){
        n1 <- round(length(bb)*aa[i]/nrow(hierScore))
        inv[i] <- length(intersect(bb[k:(k+n1-1)],hierScore[hierScore[,2]==labC[i],1] ))
        k <- k+n1
}
cor(inv,aa) # 0.9227714

hierOrder <- read.delim("D:/data/恒逸ERP数据/data/hengyi_diaoyan1316_rating_composite.txt")[ ,c(2,8,9)]
hierOrder <- hierOrder[match(rownames(disM),hierOrder[,1]), ]
tmp <- sort(hierOrder[,2],decreasing = TRUE, index.return=TRUE)
o2 <- tmp$ix
o1 <- h1$order
wilcox.test(o1,o2)

#### PTA number and amount, Price ========
data0 <- read.delim("AllinOneNew.txt")
ptaD <- data0[grepl("PTA",data0[,"品种"]), ]

###
ptaD <- cbind(ptaD, ptaD[,"金额"]/(ptaD[,"数量"]/1000))
ptaD <- as.matrix(ptaD)

y1 <- trans2group(ptaD[,8, drop=FALSE],k=2, useDates = ptaD[,2])
y1 <- y1/1000
y2 <- trans2group(ptaD[,14, drop=FALSE],k=2, useDates = ptaD[,2], f=2)
x <- 1:length(y1)
R2y(x,y1,y2,"bottomright",legend=c("销量","价格"))
abline(v=26,col=2,lwd=2,lty=2)

y1 <- trans2group(ptaD[,10, drop=FALSE],k=2, useDates = ptaD[,2])
R2y(x,y1,y2,"bottomright",legend=c("金额","价格"))
abline(v=26,col=2,lwd=2,lty=2)
## find the change point
chp <- which.min(y1[1:36])
rownames(y1)[chp]

### compare two stages of amount on time, what causes the changes? ========
y1 <- trans2group(ptaD[,"客户", drop=FALSE],k=2, useDates = ptaD[,2],f=3)
y2 <- trans2group(ptaD[,"订单", drop=FALSE],k=2, useDates = ptaD[,2],f=3)
x <- 1:length(y1)
R2y(x,y1,y2,"bottomleft",legend=c("客户数","订单数"))
abline(v=chp,col=2,lwd=2,lty=2)

useDates <- as.Date(ptaD[,"日期"])
useDay <- paste(year(useDates),month(useDates),sep="-") 
uniDay <- unique(useDay)
nm <- length(uniDay)
nd <- 1:(nm-1)
for(i in 2:nm){
        tmp1 <- unique(ptaD[useDay==uniDay[i-1],"客户"])
        tmp2 <- unique(ptaD[useDay==uniDay[i],"客户"])
        nd[i-1] <- length(setdiff(tmp2,tmp1))
}
y3 <- c(0,nd)
R2y(x,y1,y3,"bottomleft",legend=c("客户数","不同客户数"))
abline(v=chp,col=2,lwd=2,lty=2)

## new customer and old customers =========
chp1 <- 24
set0 <- c()
for(i in 1:chp1){
        tmp1 <- unique(ptaD[useDay==uniDay[i],"客户"])
        set0 <- union(set0,tmp1)
}
set1 <- c()
for(i in (chp1+1):nm){
        tmp1 <- unique(ptaD[useDay==uniDay[i],"客户"])
        set1 <- union(set1,tmp1)
}
set1 <- setdiff(set1,set0)

labK <- 1:nrow(ptaD)
labK[ptaD[,"客户"] %in% set0] <- 1
labK[ptaD[,"客户"] %in% set1] <- 2

y1 <- aggregate(as.numeric(ptaD[labK==1,"数量"]), list(useDay[labK==1]), sum)[,2]
y2 <- aggregate(as.numeric(ptaD[labK==2,"数量"]), list(useDay[labK==2]), sum)[,2]
y2 <- c(rep(0,length(y1)-length(y2)), y2)
x <- 1:length(y1)
R2y(x,y1,y2,"bottomleft",legend=c("旧客户","新客户"))
abline(v=chp,col=2,lwd=2,lty=2)

y1 <- aggregate(as.numeric(ptaD[labK==1,"金额"]), list(useDay[labK==1]), sum)[,2]
y2 <- aggregate(as.numeric(ptaD[labK==2,"金额"]), list(useDay[labK==2]), sum)[,2]
y2 <- c(rep(0,length(y1)-length(y2)), y2)
x <- 1:length(y1)
yr <- y1/(y1+y2)

R2y(x,yr,1-yr,"bottomleft",legend=c("旧客户","新客户"))
abline(v=chp,col=2,lwd=2,lty=2)

barplot(t(cbind(yr,1-yr)), main="旧客户 VS 新客户",
        xlab="", col=c("darkblue","red"),
        legend = c("Old", "New"), args.legend = list(x="bottomleft"))

## customers mean and variance distritbuions across three years========
tmpM <- plot2Time(ptaD,d1="客户",d2="金额",plot=FALSE,k=2)
y1 <- apply(tmpM,1,mean)
y2 <- apply(tmpM,1,sd)
hist(y1,main="mean distribution of customers")
hist(y2,main="variance distribution of customers")
hist(y2/y1,main="Index of dispersion of customers")

tmpM <- plot2Time(ptaD,d1="客户",d2="金额",plot=FALSE,k=2)
plot(tmpM[5,],type="b",col=1,xlab="时间", ylab="金额", main="浙江双兔新材料有限公司
")

## scores in fengcengfenji and scores in zhengxinpingji=======
kehu <- read.delim("D:/data/恒逸ERP数据/data/kehu.txt")
score1 <- read.delim("D:/data/恒逸ERP数据/data/hengyi_diaoyan1316_rating_composite.txt")
score2 <- read.delim("D:/data/恒逸ERP数据/data/征信评级.txt",skip=2,header = FALSE)

sv1 <- score1[,c(2,8)]
sv2 <- cbind(as.character(kehu[match(score2[,1],kehu[,2]),1]), score2[,2])

csv <- intersect(sv1[,1],sv2[,1])
cor.test(as.numeric(sv1[match(csv,sv1[,1]),2]), as.numeric(sv2[match(csv,sv2[,1]),2]))

table(score1[score1[,"customer"] %in% set1, 9])

table(score2[score2[ ,1] %in% kehu[match(set1,kehu[,1]), 2], 3])

### price index in days ======
data1 <- read.delim("AllinOneNew.txt")
data1 <- data1[data1[,"金额"] > 0, ]

PrV <- MarketPr()
usedate <- rownames(PrV)
Prods <- colnames(PrV)
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
        outIndDay[[i]] <- oneV
        
        plot(oneV[,2],type="l",col=i, main=Prods[i])
        abline(h=mean(oneV[,2]),lty=2,col=2)
        
        twoV <- aggregate(tmp[,"数量"],list(useDay), sum)
        R2y(1:nrow(oneV),oneV[,2],twoV[,2]*oneV[,2],led="bottomleft",legend=c("PriceInd","amount"),type="l",abl=TRUE)
        print(cor.test(oneV[,2],twoV[,2]))
}

### best prind for hengyi and one month yujing ========
data1 <- read.delim("AllinOneNew.txt")
data1 <- data1[data1[,"金额"] > 0, ]

PrV <- MarketPr()
usedate <- rownames(PrV)
Prods <- colnames(PrV)
i=1

tmp <- data1[grepl(Prods[i],data1[,"品种"]), ]
prInd <- tmp[,"金额"]/(tmp[,"数量"]/1000)
nasub <- is.na(PrV[match(as.character(tmp[,"日期"]), usedate),i])
tmpsub <- PrV[match(as.character(tmp[,"日期"]), usedate),i]
prInd[!nasub] <- prInd[!nasub]/tmpsub[!nasub]
prInd[nasub] <- mean(prInd[!nasub])

useDates <- as.Date(tmp[,"日期"])
useDay <- paste(year(useDates),week(useDates),sep="-") #useDates

oneV <- aggregate(prInd,list(useDay), mean)
b0 <- mean(oneV[,2])
twoV <- aggregate(tmp[,"金额"],list(useDay), sum)

a1 <- oneV[,2] #sapply( 1:(nrow(oneV)-4), function(i) mean(oneV[i:(i+4),2]))
b1 <- twoV[,2] #sapply( 1:(nrow(twoV)-4), function(i) mean(twoV[i:(i+4),2]))

a2 <- diff(a1)
b2 <- diff(log(b1))
y <- b2/abs(a2)
asub <- sort(a1[-length(a1)],index.return=TRUE)$ix
plot(a1[asub],y[asub],xlab="Price Index",ylab="Amount",type="l",col=2)

x1 <- a1[asub]
y1 <- y[asub]
chpi <- x1[which.max(y1)]
sdi <- sd(oneV[,2])
ui <- chpi + sdi
di <- chpi - sdi

0.951678
0.8751118

## gou mai zhouqi estimate ======
data0 <- read.delim("AllinOneNew.txt")
ptaD <- data0[grepl("PTA",data0[,"品种"]), ]
ptaD <- as.matrix(ptaD)
kehupta <- unique(ptaD[,"客户"])
tmp <- sapply(kehupta, function(i){ 
        #print(i)
        a <- ptaD[ptaD[,"客户"]==i, ,drop=FALSE]
        a <- a[!duplicated(a[,"日期"]), ,drop=FALSE]
        mean(diff(as.Date(a[,"日期"])))
             })
plot(density(tmp[!is.na(tmp)]))
hist(tmp[!is.na(tmp)],20)
median(tmp[!is.na(tmp)])


