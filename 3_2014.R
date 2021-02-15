#######################  
# Script: Scrape elections 2014 & 2018

# get gemeindenummer

gemeinden <- read_excel("gemeinden.xlsx")

# reduce to Verwaltungskreis Seeland & Verwaltungskreis Biel/Bienne

numbers <- gemeinden %>%
  filter(GDEBZNA == "Verwaltungskreis Biel/Bienne" | GDEBZNA == "Verwaltungskreis Seeland") %>%
  select(GDENR)


gnr <- numbers$GDENR[numbers$GDENR!=389]

for(m in gnr){
  #### datacleaning, per gemeinde
  
  next2 <- data.frame(readLines(paste("/Users/mathisschnell/Desktop/SP/Data/SP_Analysen/2014_resultate/waehleranteileGemeinde",m,".csv", sep="")))
  
  t <- str_split(next2[,1],";")
  
  te <- plyr::ldply(t, rbind)
  
  colnames(te)[2] <- "pa"
  
  gemeinde <- str_remove(te[3,2], "Gemeinde/Commune: ")
  wahlberechtigte <- te[26,2]
  wahlzettel <- te[27,2]
  leer <- te[28,2]
  ungultig <- te[29,2]
  gultig <- te[30,2]
  
  beteiligung <- te[31,2]
  te$pa <- str_replace(te$pa, "�", "u")
  te$pa <- str_replace(te$pa, "�", "u")
  
  
  Parteien <- te$pa[6:21]


  for(i in Parteien[1:15]){
    
    ##### Gemeindedaten
    container <- data.frame(matrix(nrow = 24, ncol = 8))
    
    for(j in 1:26){
      container[j,] <- if(te[which(te$pa == i)[2] + (5+j),1] == j){
        te[which(te$pa == i)[2] + (5+j),]
      } else {
        rep(NA, 8)
      }
      
    }
    
    container$gemeinde <- str_remove(te[3,2], "Gemeinde/Commune: ")
    container$wahlberechtigte <- te[26,2]
    container$wahlzettel <- te[27,2]
    
    container$leer <- te[28,2]
    container$ungultig <- te[29,2]
    container$gultig <- te[30,2]
    
    container$beteiligung <- te[31,2]
    
    ##### Listendaten
    
    container$Listenstimmen <- te[te$pa %in% c(i), ][1,3]
    container$Prozent <- te[te$pa %in% c(i), ][1,4]
    container$Kandidatenstimmen  <- te$pa[(which(te$pa == i)[2]+1)]
    container$Zusatzstimmen <- te$pa[(which(te$pa == i)[2]+2)]
    container$Parteistimmen <- te$pa[(which(te$pa == i)[2]+3)]
    container$Partei <- i
    
    assign(paste("container",i, sep="_"), container)
    
  }
  
  
  
  ####### spezialfall SD &  (länge der Liste....)
  

    ##### Gemeindedaten
    container <- data.frame(matrix(nrow = 10, ncol = 8))
    
    
    container[1,] <- te[which(te$pa == i)[2] + (6),]
    
    container$gemeinde <- str_remove(te[3,2], "Gemeinde/Commune: ")
    container$wahlberechtigte <- te[26,2]
    container$wahlzettel <- te[27,2]
    container$leer <- te[28,2]
    container$ungultig <- te[29,2]
    container$gultig <- te[30,2]
    container$beteiligung <- te[31,2]
    
    ##### Listendaten
    container$Listenstimmen <- te[te$pa %in% c(i), ][1,3]
    container$Prozent <- te[te$pa %in% c(i), ][1,4]
    container$Kandidatenstimmen  <- te$pa[(which(te$pa == i)[2]+1)]
    container$Zusatzstimmen <- te$pa[(which(te$pa == i)[2]+2)]
    container$Parteistimmen <- te$pa[(which(te$pa == i)[2]+3)]
    container$Partei <- i
    
    assign(paste("container",i, sep="_"), container)
    

  
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
  
  name <- gsub("[[:space:]]", "", unique(d$gemeinde))

  assign(paste("new_dataframe",unique(d$gemeinde), sep="_"), d)
  
  townname <- str_replace_all(unique(d$gemeinde), "[^[:alnum:]]", "")
  
  saveRDS(assign(paste("new_dataframe",
                       unique(d$gemeinde), 
                       sep="_"), d), 
          file = paste("2014_grosserrat",townname,"data",".rds", sep = ""))
  
}

# merge 2018data
rm(list = ls())


temp = list.files(pattern="2014_grosserrat")

data <- readRDS(file = temp[1])


for(i in 2:length(temp)){
  
  prov <- readRDS(file = temp[i])
  print(i)
  data <- rbind(data, prov)
  
  
}

data$Jahr <- "2014"

saveRDS(data, "Results14.rds")


