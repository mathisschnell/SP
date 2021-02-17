###############  data nach generierung

rm(list = ls())

older <- readRDS(file = "Results10.rds")
older$year <- as.factor("2010")
old <- readRDS(file = "Results14.rds")
old$year <- as.factor("2014")
new <- readRDS(file = "Results18.rds")
new$year <- as.factor("2018")


data_grw <- rbind(old, new)

data_grw <- rbind(data_grw, older)
colnames(older)
saveRDS(data_grw, "data_grw.rds")
data_grw <- readRDS(file = "data_grw.rds")
data_grw$wahlzettel <- as.numeric(gsub("'","" , data_grw$wahlzettel ,ignore.case = TRUE))
data_grw$ungultig <- as.numeric(gsub("'","" , data_grw$ungultig ,ignore.case = TRUE))
data_grw$wahlberechtigte <- as.numeric(gsub("'","" , data_grw$wahlberechtigte ,ignore.case = TRUE))
data_grw$beteiligung <- as.numeric(gsub("%","" , data_grw$beteiligung ,ignore.case = TRUE))

data_grw$Prozent <- as.numeric(gsub("%","" , data_grw$Prozent ,ignore.case = TRUE))

########## Parteinamen angleichen
levels(as.factor(data_grw$Partei))

data_grw <- data_grw %>%
  mutate(Partei=recode(Partei, 
                          "SP-Frauen"="SP Frauen",
                          "SP Munner"="SP-Munner",
                          "Grune Biel / Les Verts Bienne / Grune Biel / Les Verts Bienne" = "Grune Biel / Verts Bienne",
                          "Grune Seeland / Les Verts Seeland" = "Grune Seeland / Verts Seeland",
                          "BDP / PBD" = "BDP Biel-Seeland / PBD Bienne-Seeland"))



saveRDS(data_grw, "data_grw_final.rds")


data <- readRDS("data_grw_final.rds")

# gender ev nicht präzis - mal schauen...
# https://www.bfs.admin.ch/bfs/de/home/aktuell/neue-veroeffentlichungen.assetdetail.9127669.html

# read in all 4 years
df <- data.frame(matrix(ncol = 3, nrow = 1))
colnames(df) <- c("Vorname", "W",    "M")

data_names <- read_excel("data_names.xlsx", sheet = 1)

for(i in 1:4){
  
  data_names <- read_excel("data_names.xlsx", sheet = i)

  #   data_names <- data_names %>%
  #   mutate(W, recode(W, "*" = "0")) %>%
  #  mutate(M, recode(M, "*" = "0")) 
  
#  data_names <- data_names %>%
#    select(colnames(data_names)[c(1,4,5)]) 
  
  colnames(data_names) <- colnames(df)

  assign(paste("data_names_",i,sep=""),data_names)
  
  df <- rbind(df, data_names)
}


df$W[df$W == "*"] <- "0"
df$M[df$M == "*"] <- "0"
df[,2] <- as.numeric(df[,2])
df[,3] <- as.numeric(df[,3])

df <- ddply(df, .(Vorname), summarize,  M=sum(M), W=sum(W))
df$sex <- NA 

df$sex[df$M == 0] <- "f"
df$sex[df$W == 0] <- "m"

# hat noch na's  ev garnicht schlimmm

testmerge <- merge(data, df, by.x = "Vorname", all.x = T)


## manuelle korrekkturen

Frauen <- c("Andrea", "Am�lie", "Val�rie", "Tania Paola", "Am\xe9lie",
            "Anna Ursula", "Dana", "Daphn\xe9", "Dominique D\xe9sir\xe9e",  "Elisabeth Odette",
            "Val\xe9rie", "Elisabeth Odette", "Am\xe9lie", "Am�lie", "B�atrice", "Sina", "Sabrija",
            ds$Vorname[1])

Maenner <- c("Adrian M.", "Pierre-Andr�", "Nicola", "	Ren�", "J�rgen", "Luca", "Pierre-Andr�",
             "Willy", "Yannick", "Rudolf Friedrich","Kai", "Martin Daniel", "Hans Rudolf", "Franz-Dominik",
             "Jan", "Andr�", "Matthias Daniel",  "Rolf Eduard", "Stefan Markus", "Hans Peter", "Mathis", "Francis", 
             "Marc-Andr\xe9", "C\xe9dric", "Markus Erwin", "Francis", "Mohammed Hassan", "Patrick Michael", 
             "Rainer Wolfgang", "Jos\xe9", "Beat Christoph", "Anton Werner", "Bernhard A.", "Christian Philippe")

ds <- testmerge[is.na(testmerge$sex)==T,]


testmerge$sex[testmerge$Vorname %in% Frauen] <- "f"
testmerge$sex[testmerge$Familienname == "Evard" & testmerge$Jahrgang == "1991"] <- "f"

testmerge$sex[testmerge$Familienname == "Sublet" & testmerge$Jahrgang == "1994"] <- "f"

testmerge$sex[testmerge$Beruf == "Juristin MLaw, Immobilienberaterin MAS REM"] 	  <- "f"

testmerge$sex[testmerge$Vorname %in% Maenner] <- "m"

testmerge$sex[testmerge$Familienname == "de Montmollin" & testmerge$Jahrgang == "1958"] <- "m"



saveRDS(testmerge, "data_grw_final.rds")

data_grw <- readRDS("data_grw_final.rds")

################ Datensatz aggregieren nach Jahr, Liiste und Gemeiinde

data_grw_agg <- data_grw %>%
  select(Partei, year, gemeinde, Stimmen, wahlberechtigte, 
         wahlzettel, Listenstimmen, Kandidatenstimmen, Zusatzstimmen, Prozent, beteiligung)

for(i in 4:11){
  data_grw_agg[,i]<-as.numeric(data_grw_agg[,i])
}

for(i in 1:3){
  data_grw_agg[,i]<-as.factor(as.character(data_grw_agg[,i]))
}

RG <- c("Grune Biel / Verts Bienne","Grune Seeland / Verts Seeland" , 
        "JUSO JS", "PdA/POP", "PSR" , "SP Frauen", "SP-Munner")

data_grw_agg <- data_grw_agg %>%
  group_by(Partei, year, gemeinde) %>% 
  summarise_each(funs(mean)) %>%
  filter(Partei %in% RG) 

data_grw_agg[,c(2:10)] %>%
  group_by(year, gemeinde) %>% 
  summarise_each(funs(mean)) 
  
saveRDS(data_grw_agg, "data_grw_final.rds")


