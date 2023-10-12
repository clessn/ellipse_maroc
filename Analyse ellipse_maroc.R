
##### Load packages #####
library(tidyverse)


### Load Data ####
data <- readRDS("Data/hommes.rds")


# Analyse -----------------------------------------------------------------


## logement ----------------------------------------------------------------



### reparation_majeure --------------------------------------------------------------
table(data$reparationmajeure)
table(data$ses_revenu)

graph <- data %>% 
  group_by(ses_revenu) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            nrepmajeure = sum(reparationmajeure, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  mutate(prop = nrepmajeure/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()

ggplot(graph, aes(x = ses_revenu, y = prop * 100)) +
  geom_bar(stat = "identity") +
  clessnverse::theme_clean_light()

### reparation mineur -------------------------------------------------------
table(data$reparationmajeure)
table(data$ses_revenu)

graph <- data %>% 
  group_by(ses_revenu) %>% 
  summarise(ngroup = n(),
            nrepmajeure = sum(reparationmajeure, na.rm = TRUE)) %>% 
  mutate(prop = nrepmajeure/ngroup) %>% 
  drop_na()

ggplot(graph, aes(x = ses_revenu, y = prop)) +
  geom_bar(stat = "identity")


## service -----------------------------------------------------------------


## sante -------------------------------------------------------------------


## perception --------------------------------------------------------------


