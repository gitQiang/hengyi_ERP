setwd("D:/data/����ERP����/data/ExportTables/")
source("D:/code/ERP_dataANA/misc.R")
 
# Step 1: Data cleaning ============
library(lubridate)
filenames <- list.files(path="D:/data/����ERP����/data/���ݱ��ļ�/",pattern = ".txt", full.names = TRUE)

data0 <- c()
for(i in 1:length(filenames)){
        tmp <- read.delim(filenames[i],sep="\t",strip.white=TRUE)
        data0 <- rbind(data0,tmp)
}
data0[data0==""] <- NA
data0[,"����"] <- as.Date(gsub("\\.","-",data0[,"����"]))
data0 <- data0[!is.na(data0[,1]), ]
#anyNA(data0)

## trans USD --> RMB
#table(data0[,"����"])

rate <- read.delim("D:/data/����ERP����/data/ExchangeRate.txt",header = FALSE, sep="\t", strip.white = TRUE)
colnames(rate) <- rate[3,]
rate <- rate[-(1:9), ]
rate <- rate[!(grepl("������Դ",rate[,1]) | rate[,1]==""), ]
dayAll <- seq(as.Date("2012/12/31"), as.Date("2016/9/1"), "day")
rate1 <- rate[match(as.character(dayAll),rate[,1]), 2]
for(ti in 2:length(rate1)){
        if(is.na(rate1[ti])) rate1[ti] <- rate1[ti-1]
}

subDa <- which(data0[,"����"]=="USD")
t1 <- data0[subDa, "����"]
data0[subDa,"���"] <- as.numeric(data0[subDa,"���"] ) * as.numeric(rate1[match(t1,dayAll)])
data0[subDa,"����"] <- "RMB"

### trans TO --> KG
subDa <- which(data0[,"��λ"]=="TO")
data0[subDa,"����"] <- as.numeric(data0[subDa,"����"] ) * 1000
data0[subDa,"��λ"] <- "KG"

### write to database
#write2Database(data0,tname="hengyi_erp")
write2TableFile(data0,filename="AllinOne.txt")


### Step 2: export tables ============
data1 <- data0[,c("����","��˾","���")]
#write2Database(data1,tname="hengyi_erp_gongsi")
write2TableFile(data1,filename="hengyi_erp_gongsi.txt")


data2 <- data0[,c("����","�ͻ�","���","���ʽ")]
#write2Database(data2,tname="hengyi_erp_kehu")
write2TableFile(data2,filename="hengyi_erp_kehu.txt")

data3 <- data0[,c("����","Ʒ��","����","���")]
tmp <- sapply(1:nrow(data0), function(ti) unlist(strsplit(data0[ti,"��������"],"-"))[4])
## adjust to A, AA, B, C
tmp[grepl("Ҽ��",tmp)] <- "A"
tmp[grepl("�ŵ�",tmp)] <- "AA"
tmp[grepl("�ϸ�",tmp)] <- "B"
tmp[grepl("����",tmp)] <- "C"
## there is no ranks information for PTA
tmp[is.na(tmp)] <- "PTA"
tmp <- gsub("\\d","",tmp)
tmp <- gsub("\\.","",tmp)
names(tmp) <- "��Ʒ�ȼ�"
#table(tmp)
data3 <- cbind(data3,tmp)
colnames(data3) <- c("����","Ʒ��","����","���","��Ʒ�ȼ�")
write2TableFile(data3,filename="hengyi_erp_chanpin.txt")


data4 <- data0[,c("����","��˾","�ͻ�","���","���ʽ")]
write2TableFile(data4,filename="hengyi_erp_gongsi-kehu.txt")


data5 <- cbind(data0[,c("����","�ͻ�","Ʒ��","����","���","���ʽ")],tmp)
write2TableFile(data5,filename="hengyi_erp_kehu-chanpin.txt")


data6 <- cbind(data0[,c("����","��˾","Ʒ��","����","���")],tmp)
write2TableFile(data6,filename="hengyi_erp_gongsi-chanpin.txt")



