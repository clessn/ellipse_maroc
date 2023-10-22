# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("Data/data.rds")

### Check each column, replace NA with 0 when appropriate

### VIS


## Fix some VDs

Data <- Data %>% 
  mutate(
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

