# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("Data/data.rds")

### Check each column, replace NA with 0 when appropriate

### VIS


## Fix some VDs

Data <- Data %>% 
  mutate(maladiechronique_genou = ifelse(is.na(maladiechronique_genou),
                                         0, maladiechronique_genou),
         maladiechronique_estomac = ifelse(is.na(maladiechronique_estomac),
                                         0, maladiechronique_estomac),
         maladiechronique_neuro = ifelse(is.na(maladiechronique_neuro),
                                           0, maladiechronique_neuro),
         maladiechronique_yeux = ifelse(is.na(maladiechronique_yeux),
                                           0, maladiechronique_yeux),
         maladiechronique_dos = ifelse(is.na(maladiechronique_dos),
                                           0, maladiechronique_dos),
         maladiechronique_sciatique = ifelse(is.na(maladiechronique_sciatique),
                                           0, maladiechronique_sciatique),
         maladiechronique_rhumatisme = ifelse(is.na(maladiechronique_rhumatisme),
                                           0, maladiechronique_rhumatisme),
         maladiechronique_silicose = ifelse(is.na(maladiechronique_silicose),
                                           0, maladiechronique_silicose),
         maladiechronique_hemorroide = ifelse(is.na(maladiechronique_hemorroide),
                                           0, maladiechronique_hemorroide),
         maladiechronique_kyste = ifelse(is.na(maladiechronique_kyste),
                                           0, maladiechronique_kyste),
         maladierespiratoire_yeux = ifelse(is.na(maladierespiratoire_yeux),
                                           0, maladierespiratoire_yeux),
         problemepeau_allergie = ifelse(is.na(problemepeau_allergie),
                                           0, problemepeau_allergie),
         problemepeau_autre = ifelse(is.na(problemepeau_autre),
                                           0, problemepeau_autre),
         problemepeau_eruptions = ifelse(is.na(problemepeau_eruptions),
                                           0, problemepeau_eruptions),
         )


### VDS
vds <- names(Data %>%
               select(santegenerale, santementale, consommationalcool,
                      tabac, douleursmusculaires, douleursarticulaires,
                      troublesommeil,
                      starts_with(c("maladiecardio", "maladiechronique", "maladierespiratoire",
                                    "problemepeau", "problemeneuro", "problemedigestif",
                                    "blessurerecentes"))))

for (i in vds){
  message(i)
  message(sum(table(Data[[i]])))
  print(table(Data[[i]]))
  message("")
  message("")
}

