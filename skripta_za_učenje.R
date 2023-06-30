# Da ti ne ispisuje znanstveni format broja ####
options(scipen = 999)
# Funkcija koja prepoznaje da li je broj cijeli ili decimalan ####
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
# Rolane funkcije ####
sum_rol <- rollify(~sum(.x,na.rm=T),window = 3)
# Odabire jedinstvene vrijednosti jednog ili kombinacije više stupaca nekog data set-a ####
distinct(ime_stupca) # kada koristim "pipe"
unique(tablica$stupac) # za korištenje u konzoli
# Sortiranje ####
arrange(desc(stupac)) # Kada želiš "obrnuti" redoslijed
# Filtriranje redaka uz uvjet da u određenom stupcu/stupcima (ne) bude NA-ova (is.na) ####
pom2 <- pom1 %>% filter(!(is.na(stupac1) | is.na(strupac2)))
# Filtriranje redaka tako da se nigdje ne pojavi NA (stats::na.omit) ####
pom2 <- pom1 %>% na.omit()
# Filtriranje redaka u kojima se pojavljuje NA ####
pom2 <- pom1 %>% filter(rowSums(is.na(pom1))>0) 
# Selektiranje stupaca u kojima nema NA vrijednosti (dplyr::select_if) ####
pom2 <- pom1 %>% select_if(~!any(is.na(.)))
# Zamjena NA-ova u određenom stupcu tablice sa nulom (tidyr::replace_na) #### 
pom2 <- pom1 %>% replace_na(list(stupac_xyz=0))
# Zamjena svih NA-ova u tablici sa nulom (replace + is.na) ####
pom2 <- pom1 %>% replace(is.na(.),0)
# Zamjena svih NA-ova u tablici sa nulom (is.na) ####
pom[is.na(pom)]=0
# Kako se riješiti ekstrema ####
maksimum <- quantile(tablica$stupac, probs=.99,na.rm=T)
minimum <- quantile(tablica$stupac,probs=.01,na.rm=T)
pom2 <- pom1 %>% filter(stupac<maksimum & stupac>minimum)
# Rad sa string-ovima: Prva dva slova nekog string-a ####
a <- substr("stupac",1,2)
# Filtriranje datuma: PAZI, ovo ne funkcionira! ####
pom2 <- pom1 %>% filter(datum %in% c("2022-02-28","2021-02-28","2020-02-29"))
# Formatiranje brojeva (za slike) ####
hr_format <- function(x) format(x,big.mark=".",decimal.mark=",")
hr_forat_decim <- function(x) format(x,big.mark=".",decimal.mark=",",digits=1,nsmall=1)
hr_format_postotak <- function(x) paste(format(x,big.mark=".",decimal.mark=",",digits=1,nsmall=1),"%")
# Definiranje teme slika (dizajna) ####
moj_dizajn <- theme_minimal() + theme(panel.background = element_rect(fill="#e7eaf6",linetype = 0),plot.background = element_rect(fill="#e7eaf6",linetype = 0),legend.box.background = element_rect(fill="#e7eaf6",linetype = 0),text = element_text(colour = "#000000"),plot.caption = element_text(hjust = 0),legend.position = "top",legend.title = element_blank(),panel.border = element_blank(),axis.line = element_blank(),panel.grid.major = element_line(size = 0.5, linetype = "dotted",colour = "#233142"))
theme_set(moj_dizajn)
# Namještanje poretka legendi ####
ggplot() + guides(color = guide_legend(order = 0),fill  = guide_legend(order = 1))
# Primjeri izrade funkcija iz postojećih funkcija (custom-iziranje postojećih funkcija): ####
## Najjednostavniji primjer prilagođavanja već postojeće funkcije (scales::percent_format())
moj_format_za_postotke <- percent_format(accuracy = .01, big.mark = ".", decimal.mark = ",")
## Prilagođavanje postojeće funkcije, ali uz obvezu navođenja "function(x)" (base::format())
moj_format <- function(x) format(x,big.mark = ".",decimal.mark = ",")
# Figuriranje vremenskih postavki ####
Sys.setlocale("LC_TIME", "Croatian") # /"English"
# WARNING MESSAGE: In grid.newpage() : Multiple pages not available for EMF device ####
## Trebaš samo pokrenuti naredbu dev.off()