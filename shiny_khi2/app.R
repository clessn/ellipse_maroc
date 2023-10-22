library(shiny)
library(DT)
library(tidyr)
library(rsconnect)

ui <- fluidPage(
  titlePanel("Relations bivariées"),
  h2("V de Cramer"),
  a("Pour en savoir plus sur le coefficient de Cramer", href = "https://towardsdatascience.com/contingency-tables-chi-squared-and-cramers-v-ada4f93ec3fd", target = "_blank"),
  p("Le coefficient de Cramer est une mesure statistique qui indique la force de la relation entre deux variables catégorielles. Les valeurs du coefficient de Cramer varient de 0 à 1, où 0 indique qu'il n'y a pas de relation entre les variables, et 1 indique une relation parfaite. Il est important de noter que le coefficient de Cramer ne donne pas d'informations sur la direction de la relation, il indique simplement sa force."),
  br(),
  br(),
  DTOutput("table")
)

data <- as.data.frame(readRDS("khi2.rds") %>%
                        pivot_wider(
                          id_cols = "vd",
                          values_from = "v_cramer",
                          names_from = "vi"
                        ) %>% 
                        na.omit())

names(data) <- gsub("ses_sex", "Sexe", names(data))
names(data) <- gsub("ses_lieu", "Lieu", names(data))
names(data) <- gsub("ses_revenu", "Revenu", names(data))
names(data) <- gsub("revenuprincipal_mines", "Revenu principal\nmines", names(data))
names(data) <- gsub("ses_age", "Âge", names(data))
names(data) <- gsub("niveauetude", "Niveau\nd'étude", names(data))
names(data) <- gsub("ses_lang_fr", "Langue\nfrançaise", names(data))
names(data) <- gsub("ses_lang_amaz", "Langue\namazighe", names(data))
names(data) <- gsub("ses_lang_darija", "Langue\ndarija", names(data))
names(data) <- gsub("ses_lang_arabe", "Langue\narabe", names(data))
names(data) <- gsub("ses_lang_ang", "Langue\nanglaise", names(data))
names(data) <- gsub("nbpersonnelogement", "Nombre de\npersonnes dans\nle logement", names(data))


vd_names <- c(
  santegenerale = "Santé générale",
  santementale = "Santé mentale",
  consommationalcool = "Consommation d'alcool",
  tabac = "Tabac",
  douleursmusculaires = "Douleurs musculaires",
  douleursarticulaires = "Douleurs articulaires",
  troublesommeil = "Troubles du sommeil",
  maladiecardiovasculaire = "Maladie cardiovasculaire",
  maladiecardiovasculaire_hypertension = "Maladie cardiovasculaire - Hypertension",
  maladiecardiovasculaire_coronarienne = "Maladie cardiovasculaire - Coronarienne",
  maladiecardiovasculaire_fatigue = "Maladie cardiovasculaire - Fatigue",
  maladiechronique = "Maladie chronique",
  maladiechronique_colon = "Maladie chronique - Colon",
  maladiechronique_diabete = "Maladie chronique - Diabète",
  maladiechronique_rein = "Maladie chronique - Rein",
  maladiechronique_respiratoire = "Maladie chronique - Respiratoire",
  maladiechronique_genou = "Maladie chronique - Genou",
  maladiechronique_estomac = "Maladie chronique - Estomac",
  maladiechronique_neuro = "Maladie chronique - Neuro",
  maladiechronique_yeux = "Maladie chronique - Yeux",
  maladiechronique_dos = "Maladie chronique - Dos",
  maladiechronique_sciatique = "Maladie chronique - Sciatique",
  maladiechronique_rhumatisme = "Maladie chronique - Rhumatisme",
  maladiechronique_silicose = "Maladie chronique - Silicose",
  maladierespiratoire = "Maladie respiratoire",
  maladierespiratoire_asthme = "Maladie respiratoire - Asthme",
  maladierespiratoire_bronchite = "Maladie respiratoire - Bronchite",
  maladierespiratoire_rhume = "Maladie respiratoire - Rhume",
  maladierespiratoire_amygdalite = "Maladie respiratoire - Amygdalite",
  maladierespiratoire_yeux = "Maladie respiratoire - Yeux",
  problemepeau = "Problème de peau",
  problemepeau_demangeaisons = "Problème de peau - Démangeaisons",
  problemepeau_eruptions = "Problème de peau - Éruptions",
  problemepeau_allergie = "Problème de peau - Allergie",
  problemepeau_autre = "Problème de peau - Autre",
  problemepeau_squelette = "Problème de peau - Squelette",
  problemepeau_kyste = "Problème de peau - Kyste",
  problemeneuro = "Problème neuro",
  problemeneuro_maux_de_tete = "Problème neuro - Maux de tête",
  problemeneuro_vertige = "Problème neuro - Vertige",
  problemeneuro_parkinson = "Problème neuro - Parkinson",
  problemeneuro_oreille = "Problème neuro - Oreille",
  problemeneuro_syncope = "Problème neuro - Syncope",
  problemedigestif = "Problème digestif",
  problemedigestif_douleurabdo = "Problème digestif - Douleur abdominale",
  problemedigestif_nausees = "Problème digestif - Nausées",
  problemedigestif_vomissements = "Problème digestif - Vomissements",
  blessurerecentes = "Blessures récentes",
  blessurerecentes_main = "Blessures récentes - Main",
  blessurerecentes_jambe = "Blessures récentes - Jambe",
  blessurerecentes_epaule = "Blessures récentes - Épaule",
  blessurerecentes_dos = "Blessures récentes - Dos",
  blessurerecentes_yeux = "Blessures récentes - Yeux",
  blessurerecentes_oreille = "Blessures récentes - Oreille"
)

data$vd <- vd_names[data$vd]


server <- function(input, output) {
  print(head(data))
  output$table <- renderDT({
    datatable(data, options = list(
      dom = 'lfrtip',
      order = list(),
      rownames = FALSE,
      language = list(
        search = "Chercher",
        lengthMenu = "Afficher _MENU_ entrées",
        info = "Affichage de _START_ à _END_ de _TOTAL_ entrées"
      ),
      pageLength = nrow(data),
      rowCallback = JS("
        function(row, data) {
          $('td', row).each(function(i, td) {
            if (i === 0) return;
            var value = parseFloat(data[i]);
            if (!isNaN(value) && value > 0) {
              var color = 'rgb(' + (255 - Math.round(value * 255)) + ', ' + (255 - Math.round(value * 255)) + ', ' + (255) + ')';
              $(td).css('background-color', color);
            }
          });
        }
      ")
    ))
  })
}

shinyApp(ui, server)
