library(dplyr)
h <- readRDS("Data/hommes.rds") %>% 
  mutate(ses_sex = "male")
f <- readRDS("Data/femmes.rds") %>% 
  mutate(ses_sex = "female")

data <- merge(h, f, all = TRUE)

saveRDS(data, "Data/data.rds")
writexl::write_xlsx(data, "Data/data.xlsx")
