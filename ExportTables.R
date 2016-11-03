setwd("D:/data/恒逸ERP数据/data/ExportTables/")
source("D:/code/ERP_dataANA/misc.R")
 
# Step 1: Data cleaning ============
library(lubridate)
filenames <- list.files(path="D:/data/恒逸ERP数据/data/数据表文件/",pattern = ".txt", full.names = TRUE)

data0 <- c()
for(i in 1:length(filenames)){
        tmp <- read.delim(filenames[i],sep="\t",strip.white=TRUE)
        data0 <- rbind(data0,tmp)
}
data0[data0==""] <- NA
data0[,"日期"] <- as.Date(gsub("\\.","-",data0[,"日期"]))
data0 <- data0[!is.na(data0[,1]), ]
#anyNA(data0)

## trans USD --> RMB
#table(data0[,"币种"])

rate <- read.delim("D:/data/恒逸ERP数据/data/ExchangeRate.txt",header = FALSE, sep="\t", strip.white = TRUE)
colnames(rate) <- rate[3,]
rate <- rate[-(1:9), ]
rate <- rate[!(grepl("数据来源",rate[,1]) | rate[,1]==""), ]
dayAll <- seq(as.Date("2012/12/31"), as.Date("2016/9/1"), "day")
rate1 <- rate[match(as.character(dayAll),rate[,1]), 2]
for(ti in 2:length(rate1)){
        if(is.na(rate1[ti])) rate1[ti] <- rate1[ti-1]
}

subDa <- which(data0[,"币种"]=="USD")
t1 <- data0[subDa, "日期"]
data0[subDa,"金额"] <- as.numeric(data0[subDa,"金额"] ) * as.numeric(rate1[match(t1,dayAll)])
data0[subDa,"币种"] <- "RMB"

### trans TO --> KG
subDa <- which(data0[,"单位"]=="TO")
data0[subDa,"数量"] <- as.numeric(data0[subDa,"数量"] ) * 1000
data0[subDa,"单位"] <- "KG"

### write to database
#write2Database(data0,tname="hengyi_erp")
write2TableFile(data0,filename="AllinOne.txt")


### Step 2: export tables ============
data1 <- data0[,c("日期","公司","金额")]
#write2Database(data1,tname="hengyi_erp_gongsi")
write2TableFile(data1,filename="hengyi_erp_gongsi.txt")


data2 <- data0[,c("日期","客户","金额","付款方式")]
#write2Database(data2,tname="hengyi_erp_kehu")
write2TableFile(data2,filename="hengyi_erp_kehu.txt")

data3 <- data0[,c("日期","品种","数量","金额")]
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
#table(tmp)
data3 <- cbind(data3,tmp)
colnames(data3) <- c("日期","品种","数量","金额","产品等级")
write2TableFile(data3,filename="hengyi_erp_chanpin.txt")


data4 <- data0[,c("日期","公司","客户","金额","付款方式")]
write2TableFile(data4,filename="hengyi_erp_gongsi-kehu.txt")


data5 <- cbind(data0[,c("日期","客户","品种","数量","金额","付款方式")],tmp)
write2TableFile(data5,filename="hengyi_erp_kehu-chanpin.txt")


data6 <- cbind(data0[,c("日期","公司","品种","数量","金额")],tmp)
write2TableFile(data6,filename="hengyi_erp_gongsi-chanpin.txt")




