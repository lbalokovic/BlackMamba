# A. Vanjske baze ####
library("eurostat") # eurostat()
library("ecb") # get_data()
library("imfr") 
library("OECD")
library("fredr") # FED, fredr()

# B. Ucitavanje podataka ####
library("readr")
library("readxl") # Učitavanje iz Excel-a

# C. Manipuliranje podacima ####
library("tidyverse") # (dplyr,tidyr,ggplot2,purrr); pipeline ( %>% ), filter(),mutate()
library("tibbletime") # rollify()

# D. Graficko prikazivanje ####
library("ggplot2")
library("scales") # kreiranje vlastitih skala, percent_format(), date_breaks()
library("patchwork") # plot_annotation()
library("devEMF") # spremanje slika, emf(), dev.off()
library("gganimate") # izrada jednostavnih animacija
library("rworldmap") # za izradu mapa

# E. Ciscenje i uredivanje podataka ####
library("tidyxl")

# F. Rad sa datumima ####
library("lubridate") # celling_date(), floor_date()

# G. Koristenje SQL-a ####
library("sqldf") # korištenje sql jezika u skripti
library("odbc") # spajanje na bazu

# H. Rad sa Excelom u R-u ####
library("clipr") # write_clip()
library("XLConnect")
library("r2excel")
library("rJava")
library("xlsx")
library("openxlsx")

# I. Rad sa web podacima ####
library("jsonlite") # fromJSON()
library("httr")
library("rjson")

# J. Ostali paketi ####
library("HyperbolicDist") # is.wholenumber()
library("rio")
library("writexl")
library("Rtools")
library("devtools")
library("PerformanceAnalytics")
library("Hmisc")
library("ggpubr")

devtools::install_github("kassambara/r2excel")

# K. Instalacija paketa za podatke sa ECB-a i IMF-a ####
## nema ih više na CRAN-u, pa ih se mora skinuti sa Git-a 
install.packages("devtools")
library(devtools)
install_github("expersso/ecb")
install_github("christophergandrud/imfr")
