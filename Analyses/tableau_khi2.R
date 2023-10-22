# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("Data/data.rds")

#https://bookdown.org/tomholbrook12/bookdown-demo/hypothesis-testing-with-crosstabs.html#hypothesis-testing-with-crosstabs-1

### Check each column, replace NA with 0 when appropriate

### VIS
vis <- c("ses_sex", "ses_lieu", "ses_revenu",
         "revenuprincipal_mines", "ses_age", "niveauetude",
         "ses_lang_fr", "ses_lang_amaz",
         "ses_lang_darija", "ses_lang_arabe",
         "ses_lang_ang", "nbpersonnelogement")

### VDS
vds <- names(Data %>%
               select(santegenerale, santementale, consommationalcool,
                      tabac, douleursmusculaires, douleursarticulaires,
                      troublesommeil,
                      starts_with(c("maladiecardio", "maladiechronique", "maladierespiratoire",
                                    "problemepeau", "problemeneuro", "problemedigestif",
                                    "blessurerecentes")),
                      -maladiechronique_hemorroide, -maladiechronique_kyste))

for (i in vds){
  message(i)
  message(sum(table(Data[[i]])))
  print(table(Data[[i]]))
  message("")
  message("")
}

### Fix NAs in VDs
# Boucle sur chaque variable dans la liste "vds"
for (v in vds) {
  # Vérifie si la somme des valeurs non-NA de la variable courante "v" est inférieure à 150
  if (sum(!is.na(Data[[v]]), na.rm = TRUE) < 150) {
    # Si la condition est vraie, remplace toutes les valeurs NA de la variable "v" par 0
    Data[[v]][is.na(Data[[v]])] <- 0
  }
}


# Loop for khi2 -----------------------------------------------------------

# coefficient de cramer
#https://www.ibm.com/docs/fr/cognos-analytics/11.2.0?topic=terms-cramrs-v

for (i in 1:length(vis)){
  vi <- vis[i]
  print(vi)
  for (j in 1:length(vds)){
    vd <- vds[j]
    table <- table(Data[[vi]], Data[[vd]])
    ## how many degrees of freedom?
    df <- (nrow(table) - 1) * (ncol(table) - 1)
    ## critical chisq value at 0.01 level
    cv <- qchisq(.01, df, lower.tail=F)
    ## chisq test
    test <- chisq.test(Data[[vi]], Data[[vd]])
    stat <- test$statistic
    ## cramer
    n <- sum(test$observed)
    k <- ncol(test$observed)
    r <- nrow(test$observed)
    v_cramer <- sqrt(stat / (n * (min(k, r) - 1)))
    if (j == 1){
      datai <- data.frame(
        vi = vi,
        vd = vd,
        cv = cv,
        chi2 = stat,
        v_cramer = v_cramer
      )
    } else {
      dataj <- data.frame(
        vi = vi,
        vd = vd,
        cv = cv,
        chi2 = stat,
        v_cramer = v_cramer
      )
      datai <- rbind(datai, dataj)
    }
    print(paste0("             ---", vd))
  }
  if (i == 1){
    data <- datai
  } else {
    data <- rbind(data, datai)
  }
}




