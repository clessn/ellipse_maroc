##### Load packages #####
library(tidyverse)
library(RColorBrewer)

### Load Data ####
data <- readRDS("Data/data.rds")

# Marges d'erreur ---------------------------------------------------------

z <- 2.58
samples <- table(data$ses_lieu, data$ses_sex)
pops <- matrix(data = c(70, 60, 300, 250),
               nrow = 2, ncol = 2,
               byrow = TRUE,
               dimnames = list(c("Ahouli", "Mibladen"),
                               c("female", "male")))
samples["Ahouli","male"]
pops["Ahouli","male"]

adjust <- function(lieu,
                   sex,
                   ns = samples,
                   bigns = pops){
  n <-ns[lieu, sex]
  bign <- bigns[lieu, sex]
  coef <- sqrt((bign - n) / (bign - 1))
  return(coef)
}

sds_matrix <- function(vd,
                          d = data){
  sds_table <- d %>%
    group_by(ses_lieu, ses_sex) %>%
    summarise(sd = sd(eval(parse(text = vd)), na.rm = TRUE)) %>%
    drop_na() %>%
    pivot_wider(id_cols = "ses_lieu",
                values_from = "sd",
                names_from = "ses_sex")
  # Convertir le dataframe en matrice
  sds_matrix <- as.matrix(sds_table[,-1])
  rownames(sds_matrix) <- c("Ahouli", "Mibladen")
  colnames(sds_matrix) <- c("female", "male")
  return(sds_matrix)
}

calculate_me <- function(vd,
                         lieu,
                         sex,
                         d = data,
                         z = 2.58,
                         ns = samples,
                         bigns = pops){
  ## get sd of strat
  sd <- sds_matrix(vd)[lieu, sex]
  n <- ns[lieu, sex]
  me <- z * (sd / sqrt(n)) * adjust(lieu, sex)
  return(me) 
}

calculate_me(vd = "santegenerale",
             lieu = "Mibladen",
             sex = "female")


### sante generale ----------------------------------------------------------
table(data$santegenerale)
table(data$ses_sex)


### Code dplyr classique
graph <- data %>% 
  group_by(ses_lieu, ses_sex, santegenerale) %>% # prépare les prochaines données #
  summarise(nvalue = n(), .groups = "drop_last") %>% # nb de personnes qui ont mis 1 dans la case #
  ungroup() %>% 
  complete(ses_lieu, ses_sex, santegenerale, fill = list(nvalue = 0)) %>%
  group_by(ses_lieu, ses_sex) %>% 
  mutate(ngroup = sum(nvalue),
         prop = nvalue/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()


### Ajouter la marge d'erreur à la dataframe
graph$me <- NA
for (i in 1:nrow(graph)){
  graph$me[i] <- calculate_me(vd = "santegenerale", ### ici, changer la vd
                              lieu = graph$ses_lieu[i],
                              sex = graph$ses_sex[i])
}

#### Soustraire et additionner la marge d'erreur à la proportion
graph <- graph %>% 
  mutate(low = prop - me,
         high = prop + me)

ggplot(graph, aes(x = santegenerale, y = prop)) +
  facet_grid(rows = vars(ses_lieu),
             cols = vars(ses_sex),
             switch = "y") +
  geom_bar(stat = "identity") +
  geom_point() +
  geom_segment(aes(yend = high, y = low, xend = santegenerale))
  theme(strip.placement = "outside")
