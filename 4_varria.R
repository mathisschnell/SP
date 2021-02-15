###############  data nach generierung

rm(list = ls())

old <- readRDS(file = "Results14.rds")
old$year <- as.factor("2014")
new <- readRDS(file = "Results18.rds")
new$year <- as.factor("2018")

data_grw <- rbind(old, new)
saveRDS(data_grw, "data_grw.rds")

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
                       


################ Datensatz aggregieren nach Jahr, Liiste und Gemeiinde
data_grw$Zusatzstimmen
data_grw_agg <- data_grw %>%
  select(Partei, year, gemeinde, Stimmen, wahlberechtigte, 
         wahlzettel, Listenstimmen, Kandidatenstimmen, Zusatzstimmen, Prozent, beteiligung)

for(i in 4:11){
  data_grw_agg[,i]<-as.numeric(data_grw_agg[,i])
}

for(i in 1:3){
  data_grw_agg[,i]<-as.factor(as.character(data_grw_agg[,i]))
}
levels(data_grw_agg$Partei)

RG <- c("Grune Biel / Verts Bienne","Grune Seeland / Verts Seeland" , 
        "JUSO JS", "PdA/POP", "PSR" , "SP Frauen", "SP-Munner")

data_grw_agg <- data_grw_agg %>%
  group_by(Partei, year, gemeinde) %>% 
  summarise_each(funs(mean)) %>%
  filter(Partei %in% RG) 
  

data_grw_agg[,c(2:10)] %>%
  group_by(year, gemeinde) %>% 
  summarise_each(funs(mean)) 
  


                       