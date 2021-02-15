#######################  
# Script: Scrape elections 2014 & 2018

library(readxl)
library(utils)
library(stringr)
library(pdftools)
library(data.table)
library(dplyr)

<<<<<<< HEAD

=======
>>>>>>> e80cf58ecee6a77853d964104971ebd6c4660c36
# get gemeindenummer

gemeinden <- read_excel("gemeinden.xlsx")

# reduce to Verwaltungskreis Seeland & Verwaltungskreis Biel/Bienne

numbers <- gemeinden %>%
  filter(GDEBZNA == "Verwaltungskreis Biel/Bienne" | GDEBZNA == "Verwaltungskreis Seeland") %>%
  select(GDENR)


# 2014 (eine gemeinde fehlt --> Büren & Meienried wurden zusammengefasst)
for(i in numbers$GDENR) {
  
  browseURL(paste("https://www.wahlarchiv.sites.be.ch/wahlen2014/www.growa.apps.be.ch/growa/generated/waehleranteileGemeinde",
                  i,
                  ".csv", sep =""),
            browser = getOption("browser"),
            encodeIfNeeded = TRUE)
  
}


# 2018

for(i in numbers$GDENR) {
  
  browseURL(paste("https://www.bewas.sites.be.ch/2018/2018-03-25/WAHL_GROSSRAT/reportCsvGemeindeResultat-",
                  i,
                  ".csv", sep =""),
            browser = getOption("browser"), 
            encodeIfNeeded = TRUE)
  
}



#### ich habe es manuell in den ordner verschoben

# test


<<<<<<< HEAD
# varia
=======
>>>>>>> e80cf58ecee6a77853d964104971ebd6c4660c36

