#######################  
# Script: Scrape elections 2014 & 2018


library(readxl)
library(utils)
library(stringr)
library(pdftools)
library(data.table)
library(dplyr)

# get gemeindenummer

gemeinden <- read_excel("gemeinden.xlsx")

# reduce to Verwaltungskreis Seeland & Verwaltungskreis Biel/Bienne

numbers <- gemeinden %>%
  filter(GDEBZNA == "Verwaltungskreis Biel/Bienne" | GDEBZNA == "Verwaltungskreis Seeland") %>%
  select(GDENR)


gnr <- numbers$GDENR[numbers$GDENR!=389] # gemeindenummern


for(m in gnr){
#### datacleaning, per gemeinde

next2 <- data.frame(readLines(paste("2018_Resultate/reportCsvGemeindeResultat-",m,".csv", sep="")))

t <- str_split(next2[,1],";")

te <- plyr::ldply(t, rbind)

colnames(te)[2] <- "pa"

gemeinde <- str_remove(te[3,1], "Gemeinde/Commune: ")
wahlberechtigte <- te[33,2]
wahlzettel <- te[34,2]
leer <- te[35,2]
ungultig <- te[36,2]
gultig <- te[37,2]
beteiligung <- te[38,2]
te$pa <- str_replace(te$pa, "�", "u")
te$pa <- str_replace(te$pa, "�", "u")

  
Parteien <- te$pa[6:31]

# erste Partei (BDP)

for(i in Parteien[1:24]){
  
##### Gemeindedaten
container <- data.frame(matrix(nrow = 24, ncol = 8))

for(j in 1:26){
  container[j,] <- if(te[which(te$pa == i)[2] + (5+j),1] == j){
    te[which(te$pa == i)[2] + (5+j),]
  } else {
    rep(NA, 8)
  }
  
}

container$gemeinde <- str_remove(te[3,1], "Gemeinde/Commune: ")
container$wahlberechtigte <- te[33,2]
container$wahlzettel <- te[34,2]
container$leer <- te[35,2]
container$ungultig <- te[36,2]
container$gultig <- te[37,2]
container$beteiligung <- te[38,2]

##### Listendaten
container$Listenstimmen <- te[te$pa %in% c(i), ][1,3]
container$Prozent <- te[te$pa %in% c(i), ][1,4]
container$Kandidatenstimmen  <- te$pa[(which(te$pa == i)[2]+1)]
container$Zusatzstimmen <- te$pa[(which(te$pa == i)[2]+2)]
container$Parteistimmen <- te$pa[(which(te$pa == i)[2]+3)]
container$Partei <- i

assign(paste("container",i, sep="_"), container)

}



####### spezialfall SD & Piraten (länge der Liste....)

for(i in Parteien[25:26]){
  
  ##### Gemeindedaten
container <- data.frame(matrix(nrow = 24, ncol = 8))
  
for(j in 1:8){
    container[j,] <- if(te[which(te$pa == i)[2] + (5+j),1] == j){
      te[which(te$pa == i)[2] + (5+j),]
    } else {
      rep(NA, 8)
    }
    
  }
  
  container$gemeinde <- str_remove(te[3,1], "Gemeinde/Commune: ")
  container$wahlberechtigte <- te[33,2]
  container$wahlzettel <- te[34,2]
  container$leer <- te[35,2]
  container$ungultig <- te[36,2]
  container$gultig <- te[37,2]
  container$beteiligung <- te[38,2]
  
  ##### Listendaten
  container$Listenstimmen <- te[te$pa %in% c(i), ][1,3]
  container$Prozent <- te[te$pa %in% c(i), ][1,4]
  container$Kandidatenstimmen  <- te$pa[(which(te$pa == i)[2]+1)]
  container$Zusatzstimmen <- te$pa[(which(te$pa == i)[2]+2)]
  container$Parteistimmen <- te$pa[(which(te$pa == i)[2]+3)]
  container$Partei <- i
  
  assign(paste("container",i, sep="_"), container)

}

l <- ls(pattern = "container_")
x.list <- lapply(l, get)
d <- do.call(rbind, x.list)

### df cleaner machen

d <- d[,c(2:21)]
colnames(d)[1:7] <- c("Familienname", "Vorname", "Jahrgang", "Beruf", "Heimat", "Stimmen", "bisher")

d$Stimmen <- as.numeric(gsub("'","" , d$Stimmen ,ignore.case = TRUE))
d$Parteistimmen <- as.numeric(gsub("'","" , d$Parteistimmen ,ignore.case = TRUE))
d$Kandidatenstimmen <- as.numeric(gsub("'","" , d$Kandidatenstimmen ,ignore.case = TRUE))
d$Zusatzstimmen <- as.numeric(gsub("'","" , d$Zusatzstimmen ,ignore.case = TRUE))
d$Listenstimmen <- as.numeric(gsub("'","" , d$Listenstimmen ,ignore.case = TRUE))
d <- d[!is.na(d$Stimmen), ]

assign(paste("new_dataframe",unique(d$gemeinde), sep="_"), d)

townname <- str_replace_all(unique(d$gemeinde), "[^[:alnum:]]", "")

saveRDS(assign(paste("new_dataframe",
                     unique(d$gemeinde), 
                     sep="_"), d), 
        file = paste("2018_grosserrat",townname,"data",".rds", sep = ""))

}

# merge 2018data
rm(list = ls())

temp = list.files(pattern="2018_grosserrat")

data <- readRDS(file = temp[1])

for(i in 2:length(temp)){
  
prov <- readRDS(file = temp[i])

data <- rbind(data, prov)


}

data$Jahr <- "2018"

saveRDS(data, "Results18.rds")
