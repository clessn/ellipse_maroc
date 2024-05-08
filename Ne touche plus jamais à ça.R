library(foreign)
library(haven)

Morocco <- read_sav("Data/Morocco.sav")


table(wouf_wouf$ses_etatcivil1)
table(Morocco$ses_etatcivil)

Morocco$statutTravail <- data$ses_statutTravail
Morocco$ses_etatcivil <- data$ses_etatcivil


write.foreign(wouf_wouf, file = "Morocco_cleaned", var.attr = attr)
write_sav(Morocco, "Moroccocleaned.sav")

view(wouf_wouf)
