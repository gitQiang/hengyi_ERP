setwd("D:/data/恒逸ERP数据/data/ExportTables")
options(stringsAsFactors = FALSE)
source("D:/code/hengyi_ERP/misc.R")
source("D:/code/hengyi_ERP/GM11.R")
library(lubridate)
library(nor1mix)
library(zoo)
library(MASS)

## get outside prices
PrV <- MarketPr()
Prods <- colnames(PrV)

## Step 1: abnormal orders ======

# s1: Price index by outside price
data1 <- read.delim("AllinOne.txt")
outInd <- PriceIndByOut(data1, PrV, Prods)

# s2: Price index by inisde price 
data1 <- read.delim("AllinOne.txt")
#data1 <- read.delim("AllinOneNew.txt")

dkehuList <- GetCustomerDisByProduct(data1, Prods)$dkehuList
inInd <- PriceIndByIn(data1, dkehuList, Prods)

# s3: delete outliers based on outside price index with zero money orders and company in themselves
delOutliers(data1, filename="AllinOneNew.txt")

## Step 2: customer clustering ======
data0 <- read.delim("AllinOneNew.txt")
Prods <- c("PTA","DTY","FDY","POY")
nc <- 5

oneD <- GetDistanceM(data0, Prods)
cluS <- HierCluster(oneD, nc=nc)

feaM <- GetFeatureM(data0, Prods)
cluS1 <- quantileCluster(feaM, nc=nc)

hierScore <- read.delim("D:/data/恒逸ERP数据/data/hengyi_diaoyan1316_rating_composite.txt")[ ,c(2,9)] 
labh <- c("A","B","C","D","E")
labFea <- seq(nc,1,-1)
foldR <- 1:nc
v1 <- 1:nc
v2 <- 1:nc
for(i in 1:nc){
        tmp1 <- cluS1[cluS1[,2]==as.character(labFea[i]), 1]
        tmp2 <- hierScore[hierScore[,2]==labh[i], 1]
        v1[i] <- length(tmp1)
        v2[i] <- length(tmp2)
        foldR[i] <- (length(intersect(tmp1,tmp2))/v1[i])/(v1[i]/nrow(cluS1))
}

r1 <- cor(v1,v2) 
rdis <- sapply(1:1000, function(ii) cor(runif(5,0,1),runif(5,0,1)) )
pcor <- sum(rdis > r1)

## Step 3: customer value access =====

## Step 4: Price strategy based on history price index trends ======
data1 <- read.delim("AllinOneNew.txt")
data1 <- data1[data1[,"金额"] > 0, ]

dis=c("day","week","month","season")
for(i in 1:4){ ## time dimonsion
        di <- dis[i]
        outIndDay <- PriceIndSumUp(data1, PrV, Prods, di=di, plot=FALSE)
        for(j in 1:4){ ## product
                oneV <- outIndDay[[j]]
                ### best history data to predict price index in future
                nx <- Lbesterr(oneV[,2])$nx
                res1 <- BestPriceInd(data1, Prods[j], oneV, di=di, nx=nx, plot=TRUE)
                res2 <- Pbesterr(oneV[,2], nx, plot=TRUE)
        }
}

## Step 5: Pay methods =======
data0 <- read.delim("AllinOne.txt")

## Product levels 
tmp <- productLevel(data0)
uniProd <- paste(data0[,"品种"],tmp,sep="_")
tmpsub <- data0[,"付款方式"]=="立即付款"
tmp1 <- data0[tmpsub, ]
tmp2 <- data0[!tmpsub, ]

## Ratios of pay methods without time information 
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

stackedBarplot(barData)


## pay at once time series 
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

## Step 6: top orders in themselves, and ratios ========
kehu <- read.delim("../kehu.txt")
sefsubs <- grepl("恒逸", kehu[,2]) | grepl("逸盛", kehu[,2])
sefCode <- kehu[sefsubs, ]
res <- topOrders(sefCode[,1],ntop=100)
plot(res[[4]][,2],type="p", xlab="", ylab="Ratio")

## Step 7: cusotomer buy period estimate ======

## Step 8: 产品销量变化、 占比、波动率变化 ==========
data0 <- read.delim("AllinOneNew.txt")
ptaD <- data0[grepl("PTA",data0[,"品种"]), ]
ptaD <- as.matrix(ptaD)

useDates <- as.Date(ptaD[,"日期"])
useDay <- paste(year(useDates),month(useDates),sep="-") 
tmp <- aggregate(as.numeric(ptaD[,"数量"]), list(useDay), sum)
plot(1:nrow(tmp),tmp[,2],type="b",ylab="Number",xlab="Month",main="")
abline(v=c(12,24,36),col=2,lty=2,lwd=2)

useDates <- as.Date(ptaD[,"日期"])
useDay <- paste(year(useDates),quarter(useDates),sep="-") 
tmp <- aggregate(as.numeric(ptaD[,"数量"]), list(useDay), sum)
plot(1:nrow(tmp),tmp[,2],type="b",ylab="Number",xlab="Season",main="")
abline(v=c(4,8,12),col=2,lty=2,lwd=2)


top10pta <- unlist(read.table("top10PTA.txt"))

useDates <- as.Date(ptaD[,"日期"])
useDay <- paste(year(useDates),month(useDates),sep="-") 

for(i in 1:length(top10pta)){
        onesub <- which(ptaD[,"客户"]==top10pta[i])
        tmp <- aggregate(as.numeric(ptaD[onesub,"数量"]), list(useDay[onesub]), sum)
        #plot(1:nrow(tmp),tmp[,2],type="b",ylab="Number",xlab="Month",main="")
        #abline(v=c(12,24,36),col=2,lty=2,lwd=2)
        x<-as.vector(tmp[,2])/1000
        #GM11(x,length(x)+2)
        stsr <- HoltWinters(ts(x,frequency = 12),gamma=FALSE)
        plot(1:length(x), x, type="b", col=1,ylab="Number",xlab="Month",main="")
        lines(1:length(x), c(x[1:2], stsr$fitted[,"xhat"]), type="b", col=2)
        abline(v=c(12,24,36),col=2,lty=2,lwd=2)
}

kehu <- read.delim("../kehu.txt")
top10kehu <- kehu[match(top10pta,kehu[,1]), ]

## Step 9: key changes in PTA sale =====
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

R2y(x,yr,1-yr,"bottomleft",legend=c("旧客户","新客户"),y1lim=c(0,1), y2lim=c(0,1))
abline(v=chp,col=2,lwd=2,lty=2)

barplot(t(cbind(yr,1-yr)), main="旧客户 VS 新客户",
        xlab="", col=c("darkblue","red"),
        legend = c("Old", "New"), args.legend = list(x="bottomleft"))


## Step 10: customer distribution =======
data0 <- read.delim("AllinOneNew.txt")
Prods <- c("PTA","DTY","FDY","POY")
feaM <- GetCustomerDisByProduct(data0, Prods)$fkehuList

for(i in 1:length(feaM)){
        
        tmp <- exp(feaM[[i]])
        tmp[feaM[[i]]==0] <- 0
        aa <- largeCusAna(tmp)
        nk <- ncol(tmp)
        bb <- CustomerClu(tmp, aa$namesk,ntop=round(nk*0.35), plot=TRUE)
        print(bb)
        
        # numtmp <- aa$numAuck
        # for(j in 5:8){
        #         x <- numtmp[j, ]
        #         plot((1:ncol(numtmp))/ncol(numtmp),x, type="l", main="Accumlation Sale", xlab="Number of customers", ylab="Fractions")
        #         abline(h=0.9,lwd=2,lty=2,col=2)
        #         abline(v=which(x>0.9)[1]/ncol(numtmp),lwd=2,lty=2,col=2)
        #         print(which(x>0.9)[1])
        # }
}

onek <- "321419"
onesub <- which(colnames(tmp)==onek)
plot(tmp[,onesub],type="b",main=onek, xlab="",ylab="", xaxt="n")
ymax <- max(tmp[,onesub])
ymin <- min(tmp[,onesub])
text(x=c(6.5,18.5,30.5,37.5), par("usr")[3]-0.035*(ymax-ymin), labels = 2013:2016, srt = 0, pos = 1, xpd = TRUE)
