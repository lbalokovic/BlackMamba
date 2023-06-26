# 0. Predradnje ####
# Library
library(tidyverse)
library(lubridate)
library(clipr)
library(scales)
library(eurostat)
library(rjson)
library(jsonlite)
library(patchwork)
library(readxl)

# Vremenske postavke
Sys.setlocale("LC_TIME", "English")

# Da ti ne ispisuje znanstveni format broja
options(scipen=999)

# Formatiranje brojeva (za slike)
hr_format <- function(x) format(x,big.mark=".",decimal.mark=",")
hr_format_decim <- function(x) format(x,big.mark=".",decimal.mark=",",digits=1,nsmall=1)
hr_format_postotak <- function(x) paste(format(x*100,big.mark=".",decimal.mark=",",digits=1,nsmall=1),"%")

# Definiranje teme slika (dizajna)
dizajn <- theme_minimal() + theme(panel.background = element_rect(fill="#e7eaf6",linetype = 0),plot.background = element_rect(fill="#e7eaf6",linetype = 0),legend.box.background = element_rect(fill="#e7eaf6",linetype = 0),text = element_text(colour = "#000000"),plot.caption = element_text(hjust = 0),legend.position = "top",legend.title = element_blank(),panel.border = element_blank(),axis.line = element_blank(),panel.grid.major = element_line(size = 0.5, linetype = "dotted",colour = "#233142"))
theme_set(dizajn)

# Paleta boja
boje_fill <- scale_fill_manual(values = c("#155e63","#e84545","#25a55f","#ffc93c","#9b5d73","#ff7c38","#7e6752","#679186","#2e99b0","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314"))
boje_col <- scale_color_manual(values = c("#155e63","#e84545","#25a55f","#ffc93c","#9b5d73","#ff7c38","#7e6752","#679186","#2e99b0","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314"))

# Zemlje članice EU
reg = data.frame(geo=c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE","UK","LI","IS","NO"),ctry=c("AUT","BEL","BLG","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE","GBR","LIE","ISL","NOR"),country=c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Rep.","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom","Lichenstein","Iceland","Norway"),regija=c("Ostale zemlje EU-a","Ostale zemlje EU-a","Zemlje SIE","HR","Ostale zemlje EU-a","Zemlje SIE","Ostale zemlje EU-a","Zemlje SIE","Ostale zemlje EU-a","Ostale zemlje EU-a","Ostale zemlje EU-a","Ostale zemlje EU-a","Zemlje SIE","Ostale zemlje EU-a","Ostale zemlje EU-a","Zemlje SIE","Zemlje SIE","Ostale zemlje EU-a","Ostale zemlje EU-a","Ostale zemlje EU-a","Zemlje SIE","Ostale zemlje EU-a","Zemlje SIE","Zemlje SIE","Zemlje SIE","Ostale zemlje EU-a","Ostale zemlje EU-a","Ostale zemlje EU-a","EEA","EEA","EEA"))

# A. Ucitavanje podataka ####
load("Z:/DSR/dwh/NAV.Rda")
load("Z:/DSR/dwh/nav_opce.Rda")
load("Z:/DSR/dwh/imovina_S2.Rda")
load("Z:/DSR/dwh/cijene.Rda")


# 1. Nerizična kamatna stopa za eurske duznicke VP (eurostat) ####
# dohvat podataka
eurostat1 <- get_eurostat("irt_euryld_d",yld_curv=="SPOT_RT")
# sredjivanje podataka
pom1 <- eurostat1 %>% filter(yld_curv=="SPOT_RT" & (time=="2019-12-05" | time=="2023-05-31")) %>% select(-c(yld_curv,geo)) %>% separate(maturity,c("maturity","mjeseci"),"M") %>% mutate(maturity=str_remove(maturity,"Y"),maturity=str_remove(maturity,"_"),mjeseci=as.numeric(mjeseci),maturity=as.numeric(maturity)) %>% replace_na(list(maturity=0,mjeseci=0)) %>% mutate(rocnost=maturity*12+mjeseci) %>% select(datum=time,obveznice=bonds,rocnost,stope=values) %>% mutate(datum=as.character(datum),stope=stope/100)
# slika
ggplot(pom1,aes(rocnost,stope,col=datum)) + geom_line() + facet_wrap(~obveznice) + scale_y_continuous(labels=hr_format_postotak)
rm(pom1)

# 2. Prikaz odnosa cijene obveznica i trzisnog prinosa do dospijeca ####
# tablica
pom1 <- expand.grid(dospijece=c(1,5,10,15,20),prinos=seq(0.005,0.1,by=0.005)) %>% mutate(G=100,K=2.5,n=2) %>% mutate(cijena=K/(prinos/n)*(1-(1/(1+prinos/n)^(n*dospijece)))+G/(1+prinos/n)^(n*dospijece)) %>% mutate(dospijece=as.factor(dospijece))
# slika
ggplot(pom1,aes(x=prinos,y=cijena,col=dospijece)) + geom_line(size=2) + scale_x_continuous(labels=hr_format_postotak,breaks = seq(0,0.1,by=0.02)) + scale_y_continuous(breaks = seq(0,200,by=10)) + geom_vline(xintercept = 0.05,size=1.5,linetype=2) + boje_col + labs(subtitle=str_wrap("Prikaz odnosa cijene obveznice (y os) i tržišnog prinosa do dospijeća (x os)",45), x="",y="")
rm(pom1)

# 3. Tecajevi eura sa ostalim valutama (eurostat) ####
# dohvat podataka
eurostat_er <- get_eurostat("ert_bil_eur_d")
# filtriranje
pom1 <- eurostat_er %>% filter(currency %in% c("CHF","GBP","HUF","USD"))
# slika
ggplot(pom1,aes(time,values,col=currency)) + geom_line(size=1.3) + facet_wrap(~currency,scale="free")

# 4. BDP (eurostat) ####
# dohvat podataka
eurostat_bdp <- get_eurostat("namq_10_gdp")
# Sezonski i blagdanski prilagodjeni kvartalni bdp u tekućim cijenama i milijunima nacionalne valute (i neke zasebne komponente bdp-a)
pom1 <- eurostat_bdp %>% filter(unit=="CP_MNAC" & s_adj=="SCA" & na_item %in% c("B1GQ","P31_S13","P31_S14","P61","P62","P71","P72","D3") & geo %in% c("EU27_2020","HR")) %>% select(datum=time,geo,varijabla=na_item,iznos=values) %>% mutate(varijabla=case_when(varijabla=="B1GQ"~"bdp",varijabla=="P31_S13"~"drzavna potrosnja",varijabla=="P31_S14"~"potrosnja kucanstava",varijabla=="P61"~"izvoz dobara",varijabla=="P62"~"izvoz usluga",varijabla=="P71"~"uvoz dobara",varijabla=="P72"~"uvoz usluga",varijabla=="D3"~"Subvencije")) %>% arrange(datum) %>% group_by(geo,varijabla) %>% mutate(delta=(iznos/lag(iznos)-1)) %>% ungroup() %>% na.omit()
# slika
ggplot(pom1 %>% filter(geo=="HR"),aes(datum,iznos,col=varijabla)) + geom_line(linewidth=1) + scale_y_continuous(labels=hr_format)

# kvartalni nominalni BDP (BDV+porez)
bdp <- get_eurostat("namq_10_gdp") %>% filter(geo=="HR" & na_item %in% c("B1G","B1GQ","D21X31") & s_adj=="NSA" & unit=="CP_MNAC") %>% mutate(datum=ceiling_date(time,unit = "quarter")-1,razina=case_when(na_item=="B1G"~"bdv",na_item=="B1GQ"~"bdp",T~"bdp_porez")) %>% select(datum,razina,values) %>% spread(razina,values) %>% arrange(datum) %>% mutate(bdp_y=bdp+lag(bdp,1)+lag(bdp,2)+lag(bdp,3),bdv_y=bdv+lag(bdv,1)+lag(bdv,2)+lag(bdv,3))
bdp_realni <- get_eurostat("namq_10_gdp") %>% filter(geo=="HR" & na_item=="B1GQ" & s_adj=="NSA" & unit=="CLV_PCH_SM") %>% mutate(datum=ceiling_date(time,unit = "quarter")-1) %>% select(datum,dbdp_r=values) %>% arrange(datum)








# 5. Broj aktivnih društava za upravljanje i investicijskih fondova ####
pom <- nav_opce %>% mutate(mj=ceiling_date(datum,"month")-1) %>% filter(mj==datum & vrsta0=="Investicijski") %>% group_by(datum,drustvo,vrsta1) %>% summarise(broj=n())
temp <- pom %>% filter(vrsta1=="UCITS" & (datum=="2022-09-30" | datum=="2021-12-31"))
write_clip(temp)
# 6. Top društva za upravljanje po neto imovini pod upravljanjem ####
pom <- nav_opce %>% mutate(mj=ceiling_date(datum,"month")-1) %>% filter(mj==datum,vrsta0=="Investicijski") %>% group_by(datum,drustvo,vrsta1) %>% summarise(nav=sum(nav,na.rm=T)) %>% group_by(datum,vrsta1) %>% mutate(nav_uk=sum(nav)) %>% ungroup() %>% mutate(udio=nav/nav_uk)
# 7. Udio obveznica u portfelju ####
pom <- nav %>% filter(vrsta1=="UCITS" & razina1=="Imovina") %>% group_by(datum) %>% mutate(im_uk=sum(iznos,na.rm=T)) %>% group_by(datum,razina2) %>% summarise(iznos=sum(iznos,na.rm=T),im_uk=mean(im_uk)) %>% ungroup() %>% mutate(udio=iznos/im_uk) 
# 8. ... Od toga Hrvatskih ####
pom <- nav %>% filter(vrsta1=="UCITS" & razina2=="Obveznice") %>% mutate(rezidentnost=if_else(drzava=="HR","HR","non HR")) %>% group_by(datum) %>% mutate(obv_uk=sum(iznos,na.rm=T)) %>% group_by(datum,rezidentnost) %>% summarise(iznos=sum(iznos,na.rm=T),obv_uk=mean(obv_uk)) %>% ungroup() %>% mutate(udio=iznos/obv_uk) 
# 9. Duracija ####
# duracija obvezničkog portfelja
p1 <- ggplot(pom1,aes(x=datum,y=duracija)) + geom_line(size=1.5) + scale_x_date(breaks = date_breaks("1 year"),labels = date_format("%y.")) + boje_col + labs(x="",y="",subtitle = str_wrap("Prosječna duracija obveznica, u godinama",45)) + scale_y_continuous(labels = hr_format) + guides(col=guide_legend(nrow=2))

library(ggalluvial)
library(tidyverse)
library(lubridate)
library(scales)
library(devEMF)
library(eurostat)
library(wbstats)
library(ecb)
library(readxl)
library(ggpubr)
library(rjson)
library(jsonlite)
library(OECD)
library(imfr)
library(ggrepel)
library(patchwork)
library(fredr)
library(ggalt)
library(HyperbolicDist)
library(tibbletime)
library(jrvFinance)

# rizik refinanciranja
pom2 <- nav %>% filter(datum=="2022-06-30" & vrsta1=="UCITS" & razina2=="Obveznice") %>% mutate(varijabla=case_when(dospijece<="2023-12-31"~"Kraj 2023.",dospijece<="2027-12-31"~"Kraj 2027.",T~"Dulje ročnosti"),varijabla=factor(varijabla,levels=c("Kraj 2023.","Kraj 2027.","Dulje ročnosti"))) %>% group_by(varijabla) %>% summarise(kamata=weighted.mean(x = kamata,w = iznos,na.rm=T),iznos=sum(iznos,na.rm=T)) %>% ungroup() %>% mutate(ukupno=sum(iznos,na.rm=T)) %>% ungroup() %>% mutate(udio=iznos/ukupno, kamata=kamata/100) %>% select(varijabla,`Kamatna stopa`=kamata,`Udio u portfelju`=udio) %>% gather("indikator","iznos",-varijabla)
p2 <- ggplot(pom2,aes(x=varijabla,y=iznos,fill=varijabla)) + geom_col(position = "dodge") + facet_wrap(~indikator,scales="free") + boje_fill + scale_y_continuous(labels = hr_format_postotak1) + labs(x="",y="",subtitle = "Karakteristike obveznica prema dospijeću") + theme(axis.text.x=element_blank())
emf(file = "Z:/DSR/FinancijskaStabilnost/slike/66_kamatni_rizik_ucits.emf",width = 8,height = 6)
p1 + p2 + plot_annotation(title=str_wrap("Slika 66. U uvjetima rasta kamatnih stopa kratkoročno raste kamatni rizik, ali se istovremeno smanjuje rizik refinanciranja",90),caption=str_c(str_wrap("Napomena: Kraj 2023. označuje obveznice koje dospijevaju do 2023., kraj 2027. obveznice koje dospijevaju do kraja 2027., dok dulje ročnosti označavaju obveznice koje dospijevaju nakon 2027. godine.",130),"\nIzvor: Hanfa")) + plot_layout(guides = "collect")
dev.off()
rm(pom1,pom2,i,p1,p2)
# 10. Prinosi ####
## Prinosi (mjesečni) - UCITS po kategorijama
pom1 <- nav_opce %>% filter(vrsta1=="UCITS" & br_udjela>0) # Filtriramo samo UCITS fondove i izbacujemo observacije sa br_udjela=0
pom2 <- pom1 %>% mutate(cj_udjela_u_valuti=case_when(datum<"2014-01-01"~cj_udjela,T~cj_udjela_u_valuti),valuta=case_when(datum<"2014-01-01"~"HRK",T~valuta),tecaj=case_when(datum<"2014-01-01"~1,T~tecaj),mjesec=ceiling_date(datum,"month")-1) # Opcija denominacije cijene udjela u ino valuti očito je omogućena tek od 2014. godine, pa korigiramo prijašnje podatke i pripremamo novi stupac za filtriranje mjesečnih podataka
pom3 <- pom2 %>% filter(cj_udjela_u_valuti>0) # Eliminiramo observacije sa cj_udjela_u_valuti=0
pom4 <- pom3 %>% group_by(mjesec) %>% filter(datum==max(datum)) %>% ungroup() # Filtriramo mjesečne podatke
pom5 <- pom4 %>% group_by(subjekt,klasa) %>% arrange(datum) %>% mutate(promjena_valute=case_when(valuta==lag(valuta)~0,T~1),omjer=lag(cj_udjela_u_valuti)/cj_udjela_u_valuti) # Kreiramo pomoćne stupce: promjena_valute i omjer, koji će nam trebati u idućim koracima
pom6 <- pom5 %>% mutate(prinos1=case_when(promjena_valute==0 | subjekt %in% c("InterCapital SEE Equity","InterCapital Bond")~cj_udjela_u_valuti/lag(cj_udjela_u_valuti)-1,valuta %in% c("EUR","USD") & lag(valuta)=="HRK"~(cj_udjela_u_valuti*tecaj)/lag(cj_udjela_u_valuti)-1,valuta=="HRK" & lag(valuta) %in% c("EUR","USD")~(cj_udjela_u_valuti/lag(tecaj))/lag(cj_udjela_u_valuti)-1,(valuta!="HRK" & lag(valuta)!="HRK")~(cj_udjela_u_valuti*(tecaj/lag(tecaj)))/lag(cj_udjela_u_valuti)-1)) # Računamo prvu verziju prinosa po subjektu tako da uzimamo u obzir sljedeće: Ako nije bilo promjene valute, onda se prinos računa na klasičan način. Ako je valuta fonda promijenjena iz domaće u ino, onda prvo cj_udjela_u_valuti moramo pomnožiti sa tečajem. Ako je valuta fonda promijenjena iz ino u domaću, onda cj_udjela_u_valuti prvo moramo podijeliti sa lagiranom vrijednosti tečaja. Ako je valuta fonda promijenjena iz ino u ino, onda cj_udjela_u_valuti prvo moramo pomnožiti sa omjerom tečaja i lagirane vrijednosti tečaja. Također, vodimo računa o InterCapital-ovim fondovima SEE Equity i Bond koji su promjenu valute izveli totalno drugačije od ostalih.
pom7 <- pom6 %>% mutate(prinos2=case_when(promjena_valute==0 & omjer>=6.67 & omjer<=20~(cj_udjela_u_valuti*10)/lag(cj_udjela_u_valuti)-1,promjena_valute==0 & omjer>=66.67 & omjer<=200~(cj_udjela_u_valuti*100)/lag(cj_udjela_u_valuti)-1,promjena_valute==0 & omjer>=666.67 & omjer<=2000~(cj_udjela_u_valuti*1000)/lag(cj_udjela_u_valuti)-1,promjena_valute==0 & omjer>=0.067 & omjer<=0.2~cj_udjela_u_valuti/(lag(cj_udjela_u_valuti)*10)-1,promjena_valute==0 & omjer>=0.0067 & omjer<=0.02~cj_udjela_u_valuti/(lag(cj_udjela_u_valuti)*100)-1,promjena_valute==0 & omjer>=0.00067 & omjer<=0.002~cj_udjela_u_valuti/(lag(cj_udjela_u_valuti)*1000)-1,T~prinos1)) # Računamo drugu verziju prinosa i to tako što uzimamo u obzir činjenicu da često prilikom spajanja fondova cijena udjela može naglo skočiti ili pasti za neki okrugli broj (najčešće 1000, ali ovdje smo predvidjeli da može i za 10 i za 100), iako nije došlo do promjene valute fonda. Za detekciju takvih slučajeva koristit će nam stupac - omjer.
pom8 <- pom7 %>% ungroup() %>% filter(prinos2<0.5 | prinos2>=-0.5) # Obzirom na prethodne postupke, kojima smo pokušali obuhvatiti sve eventualne slučajeve pojave ekstremnih vrijednosti prinosa, uslijed nekih specifičnih događaja u nekom određenom mjesecu za neki određeni fond, kao što je recimo spajanje nekih fondova ili promjena valute, sada mičemo iz analize eventualno preostale ekstremne slučajeve, jer ih smatramo greškama koje bez grižnje savjesti možemo ukloniti.
pom9 <- pom8 %>% mutate(kategorija=case_when(vrsta4=="Novčani" | vrsta5=="Kratkoročni obveznički"~"Novčani i kratkoročni obveznički",vrsta4 %in% c("Ostali","Napajajući")~"Ostali",T~vrsta4)) # Kreiramo stupac - kategorija
yld_ucits_m <- pom9 %>% group_by(datum,kategorija) %>% summarise(yld=weighted.mean(x=prinos2,w=nav,na.rm=T)) %>% ungroup() %>% na.omit()
rm(pom1,pom2,pom3,pom4,pom5,pom6,pom7,pom8,pom9)

ggplot(yld_ucits_m,aes(datum,yld)) + geom_line() + facet_wrap(~kategorija,scales="free") + scale_y_continuous(label=hr_format_postotak)
pom1 <- nav_opce %>% filter(vrsta1=="UCITS") %>% arrange(datum) %>% group_by(subjekt,klasa) %>% mutate(prinos=(cj_udjela_u_valuti/lag(cj_udjela_u_valuti,365)-1)*100) %>% filter(!is.na(prinos))

ggplot(pom1,aes(x=prinos)) + geom_histogram(binwidth = 5) + geom_rug() + scale_x_continuous(limits = c(-100,100),breaks = seq(-100,100,by=10))

pom2 <- nav_opce %>% filter(vrsta1=="UCITS") %>% group_by(datum,subjekt,klasa) %>% summarise(br_udjela=sum(br_udjela,na.rm=T)) %>% group_by(subjekt,klasa) %>% arrange(datum) %>% mutate(delta=((br_udjela/lag(br_udjela))-1)*100) %>% ungroup() %>% filter(!is.na(delta) & delta!=0) %>% mutate(donja_granica=quantile(delta,probs=.005),gornja_granica=quantile(delta,probs=.995))




## Prinosi UCITS fondova - nominalni prinosi sa benchmarks-ima ####
# ucits-i
pom <- nav_opce %>% mutate(godina=ceiling_date(datum,unit = "year")-1,vrsta=if_else(vrsta4 %in% c("Obveznički","Dionički"),vrsta4,"Ostali")) %>% filter(vrsta1=="UCITS" & cj_udjela_u_valuti!=0 & !(subjekt %in% c("InterCapital Bond (HRICAMUCAON0)", "InterCapital SEE Equity  (HRICAMUCATW0)") & year(datum)==2016) & !(subjekt=="InterCapital Smart II   (HRVBINUVBSM7)" & year(datum)==2014) & (datum==godina | datum=="2022-06-30")) %>% select(datum,oib,vrsta,nav,cj_udjela_u_valuti) %>% ungroup() %>% arrange(datum) %>% group_by(oib,vrsta) %>% mutate(prinos=(cj_udjela_u_valuti/lag(cj_udjela_u_valuti,1)-1)*100, lnav=lag(nav,1)) %>% group_by(datum,vrsta) %>% summarise(p25=quantile(prinos,probs=0.25,na.rm=T),p75=quantile(prinos,probs=0.75,na.rm=T),medijan=median(prinos,na.rm=T),prinos=weighted.mean(prinos,w = lnav,na.rm = T)) %>% na.omit() %>% filter(datum>"2014-12-31") %>% mutate(datum_graf=floor_date(datum,"year")) %>% mutate(datum=if_else(datum=="2022-06-30",datum,datum_graf)) %>% filter(datum<="2022-06-30")
p1 <- ggplot(pom,aes(x=datum,y=medijan,fill=vrsta)) + geom_hline(aes(yintercept=0),color="red",size=1.8,alpha=0.3)+ geom_errorbar(inherit.aes=F,aes(x=datum,ymin=p25,ymax=p75,col=vrsta),size=1.5,alpha=0.7,show.legend=F) + geom_point(shape=18,size=4,show.legend=F) + facet_wrap(~vrsta,scales="free") + scale_x_date(date_breaks = "1 year",date_labels = "%y.") + boje_col + labs(x="",y="",subtitle=str_wrap("Medijalne vrijednosti nominalnih godišnjih prinosa UCITS fondova po vrsti fonda, u postotnim bodovima",150))

# PROVJERAVANJE
# stari kod (FS)
pom <- nav_opce %>% mutate(godina=ceiling_date(datum,unit = "year")-1,vrsta=if_else(vrsta4 %in% c("Obveznički","Dionički"),vrsta4,"Ostali")) %>% filter(vrsta1=="UCITS" & cj_udjela_u_valuti!=0 & !(subjekt %in% c("InterCapital Bond (HRICAMUCAON0)", "InterCapital SEE Equity  (HRICAMUCATW0)") & year(datum)==2016) & !(subjekt=="InterCapital Smart II   (HRVBINUVBSM7)" & year(datum)==2014) & (datum==godina | datum=="2022-06-30")) %>% select(datum,oib,vrsta,nav,cj_udjela_u_valuti) %>% ungroup() %>% arrange(datum) %>% group_by(oib,vrsta) %>% mutate(prinos=(cj_udjela_u_valuti/lag(cj_udjela_u_valuti,1)-1)*100, lnav=lag(nav,1)) %>% group_by(datum,vrsta) %>% summarise(p25=quantile(prinos,probs=0.25,na.rm=T),p75=quantile(prinos,probs=0.75,na.rm=T),medijan=median(prinos,na.rm=T),prinos=weighted.mean(prinos,w = lnav,na.rm = T)) %>% na.omit() %>% filter(datum>"2014-12-31") %>% mutate(datum_graf=floor_date(datum,"year")) %>% mutate(datum=if_else(datum=="2022-06-30",datum,datum_graf)) %>% filter(datum<="2022-06-30")
emf(file="prinosi_FS.emf",width=8,height=6)
ggplot(pom,aes(x=datum,y=medijan,fill=vrsta)) + geom_hline(aes(yintercept=0),color="red",size=1.8,alpha=0.3)+ geom_errorbar(inherit.aes=F,aes(x=datum,ymin=p25,ymax=p75,col=vrsta),size=1.5,alpha=0.7,show.legend=F) + geom_point(shape=18,size=4,show.legend=F) + facet_wrap(~vrsta,scales="free") + scale_x_date(date_breaks = "1 year",date_labels = "%y.") + boje_col + labs(x="",y="",subtitle=str_wrap("Medijalne vrijednosti nominalnih godišnjih prinosa UCITS fondova po vrsti fonda, u postotnim bodovima",150))
dev.off()
# novi kod
pom <- nav_opce %>% mutate(godina=ceiling_date(datum,unit = "year")-1,vrsta=if_else(vrsta4 %in% c("Obveznički","Dionički"),vrsta4,"Ostali")) %>% filter(vrsta1=="UCITS" & cj_udjela_u_valuti!=0 & !(subjekt %in% c("InterCapital Bond (HRICAMUCAON0)", "InterCapital SEE Equity  (HRICAMUCATW0)") & year(datum)==2016) & !(subjekt=="InterCapital Smart II   (HRVBINUVBSM7)" & year(datum)==2014) & (datum==godina | datum=="2022-06-30")) %>% select(datum,oib,subjekt,klasa,vrsta4,vrsta,drustvo,nav,cj_udjela_u_valuti,valuta,tecaj) %>% group_by(oib,subjekt,klasa,vrsta4,vrsta,drustvo) %>% arrange(datum) %>% mutate(promjena_valute=case_when(valuta==lag(valuta)~0,T~1),prinos=case_when(promjena_valute==0 | drustvo=="INTERCAPITAL ASSET MANAGEMENT d.o.o."~cj_udjela_u_valuti/lag(cj_udjela_u_valuti)-1,valuta %in% c("EUR","USD") & lag(valuta)=="HRK"~(cj_udjela_u_valuti*tecaj)/lag(cj_udjela_u_valuti)-1,valuta=="HRK" & lag(valuta) %in% c("EUR","USD")~(cj_udjela_u_valuti/lag(tecaj))/lag(cj_udjela_u_valuti)-1,(valuta!="HRK" & lag(valuta)!="HRK")~(cj_udjela_u_valuti*(tecaj/lag(tecaj)))/lag(cj_udjela_u_valuti)-1),lnav=lag(nav)) %>% group_by(datum,vrsta) %>% summarise(p25=quantile(prinos,probs=0.25,na.rm=T),p75=quantile(prinos,probs=0.75,na.rm=T),medijan=median(prinos,na.rm=T),prinos_mean=weighted.mean(prinos,w = lnav,na.rm = T)) %>% na.omit() %>% filter(datum>"2014-12-31") %>% mutate(datum_graf=floor_date(datum,"year")) %>% mutate(datum=if_else(datum=="2022-06-30",datum,datum_graf)) %>% filter(datum<="2022-06-30")
emf(file="prinosi_novo.emf",width=8,height=6)
ggplot(pom,aes(x=datum,y=medijan,fill=vrsta)) + geom_hline(aes(yintercept=0),color="red",size=1.8,alpha=0.3)+ geom_errorbar(inherit.aes=F,aes(x=datum,ymin=p25,ymax=p75,col=vrsta),size=1.5,alpha=0.7,show.legend=F) + geom_point(shape=18,size=4,show.legend=F) + facet_wrap(~vrsta,scales="free") + scale_x_date(date_breaks = "1 year",date_labels = "%y.") + boje_col + labs(x="",y="",subtitle=str_wrap("Medijalne vrijednosti nominalnih godišnjih prinosa UCITS fondova po vrsti fonda, u postotnim bodovima",150))
dev.off()


pom1 <- nav_opce %>% mutate(godina=ceiling_date(datum,unit = "year")-1) %>% filter(vrsta1=="UCITS" & cj_udjela_u_valuti!=0 & !(subjekt %in% c("InterCapital Bond (HRICAMUCAON0)", "InterCapital SEE Equity  (HRICAMUCATW0)") & year(datum)==2016) & !(subjekt=="InterCapital Smart II   (HRVBINUVBSM7)" & year(datum)==2014) & datum==godina) %>% select(datum,oib,subjekt,klasa,vrsta4,drustvo,nav,cj_udjela_u_valuti,valuta,tecaj) %>% group_by(oib,subjekt,klasa,vrsta4,drustvo) %>% arrange(datum) %>% mutate(promjena_valute=case_when(valuta==lag(valuta)~0,T~1),prinos=case_when(promjena_valute==0 | drustvo=="INTERCAPITAL ASSET MANAGEMENT d.o.o."~cj_udjela_u_valuti/lag(cj_udjela_u_valuti)-1,valuta %in% c("EUR","USD") & lag(valuta)=="HRK"~(cj_udjela_u_valuti*tecaj)/lag(cj_udjela_u_valuti)-1,valuta=="HRK" & lag(valuta) %in% c("EUR","USD")~(cj_udjela_u_valuti/lag(tecaj))/lag(cj_udjela_u_valuti)-1,(valuta!="HRK" & lag(valuta)!="HRK")~(cj_udjela_u_valuti*(tecaj/lag(tecaj)))/lag(cj_udjela_u_valuti)-1),lnav=lag(nav)) %>% ungroup() %>% filter(datum=="2022-12-31" & !is.na(prinos)) %>% mutate(ponder=lnav/sum(lnav,na.rm=T))



# benchmarks-i
sp500 <- get_data("FM.M.US.USD.DS.EI.S_PCOMP.HSTA") %>% mutate(obstime=case_when(substr(obstime,6,7) %in% c("01","03","05","07","08","10","12")~paste(obstime,"-31",sep=""),substr(obstime,6,7) %in% c("04","06","09","11")~paste(obstime,"-30",sep=""),substr(obstime,6,7)=="02" & is.wholenumber(as.numeric(substr(obstime,1,4))/4)==T~paste(obstime,"-29",sep=""),T~paste(obstime,"-28",sep=""))) %>% mutate(obstime=as.Date(obstime)) %>% select(datum=obstime,sp500=obsvalue) %>% filter(datum<="2022-06-30") 
blmbrg_bench <- read_excel("Z:/DSR/FinancijskaStabilnost/radni_fs2.xlsx",sheet="bloomberg_benchmarks") %>% mutate(datum=as.Date(datum))
benchmarks <- inner_join(sp500,blmbrg_bench,by="datum") %>% mutate(godina=ceiling_date(datum,"year")-1) %>% group_by(godina) %>% filter(datum==max(datum)) %>% ungroup() %>% select(-godina) %>% gather("benchmark","iznos",-datum) %>% arrange(datum) %>% group_by(benchmark) %>% mutate(iznos=(iznos/lag(iznos)-1)*100) %>% ungroup() %>% na.omit() %>% mutate(datum=if_else(datum==max(datum),as.Date("2022-12-31"),datum),benchmark=case_when(benchmark=="sp500"~"S&P 500",benchmark=="LET3TREU"~"Fiksni prinos",benchmark=="BXIIUDB2"~"Mješoviti portfelj"),benchmark=factor(benchmark,levels=c("S&P 500","Fiksni prinos","Mješoviti portfelj")))
p2 <- ggplot(benchmarks,aes(datum,iznos,fill=benchmark)) + geom_col(position="dodge") + facet_wrap(~benchmark) + boje_fill + scale_x_date(date_breaks="1 year",date_labels="%y.") + labs(x="",y="",subtitle=str_wrap("Godišnje stope promjene referentnih tržišnih indeksa, u postotnim bodovima",100))
emf(file = "Z:/DSR/FinancijskaStabilnost/slike/71_ucits_nom_prinosi.emf",width = 8,height = 6)
p1 / p2 + plot_annotation(title=str_wrap("Slika 71. Investicijski fondovi ostvarivali su konkurentske nominalne prinose u prijašnjim razdobljima, međutim aktualne tržišne nestabilnosti značajno potiskuju njihovu profitabilnost",90),caption=str_c(str_wrap("Napomena: Linijske oznake na gornjoj slici prikazuju interkvartilni raspon prinosa pojedinačnih fondova u uzorku. Posljednji podaci na prikazima pokazuju ostvarenje od početka 2022. godine do kraja lipnja. Fiksni prinos je vrijednost indeksa Bloomberg Euro Aggregate Treasury 3-5 Year, a mješoviti portfelj je vrijednost indeksa Bloomberg US Dynamic Balance Indeks II.",135),"\nIzvori: Bloomberg, Hanfa"))
dev.off()
rm(pom,sp500,blmbrg_bench,benchmarks,p1,p2)

# Godišnji prinos za 2022. - Lider Media ####
pom1 <- nav_opce %>% mutate(godina=ceiling_date(datum,unit = "year")-1) %>% filter(vrsta1=="UCITS" & cj_udjela_u_valuti!=0 & !(subjekt %in% c("InterCapital Bond (HRICAMUCAON0)", "InterCapital SEE Equity  (HRICAMUCATW0)") & year(datum)==2016) & !(subjekt=="InterCapital Smart II   (HRVBINUVBSM7)" & year(datum)==2014) & datum==godina) %>% select(datum,oib,subjekt,klasa,vrsta4,drustvo,nav,cj_udjela_u_valuti,valuta,tecaj) %>% group_by(oib,subjekt,klasa,vrsta4,drustvo) %>% arrange(datum) %>% mutate(promjena_valute=case_when(valuta==lag(valuta)~0,T~1),prinos=case_when(promjena_valute==0 | drustvo=="INTERCAPITAL ASSET MANAGEMENT d.o.o."~cj_udjela_u_valuti/lag(cj_udjela_u_valuti)-1,valuta %in% c("EUR","USD") & lag(valuta)=="HRK"~(cj_udjela_u_valuti*tecaj)/lag(cj_udjela_u_valuti)-1,valuta=="HRK" & lag(valuta) %in% c("EUR","USD")~(cj_udjela_u_valuti/lag(tecaj))/lag(cj_udjela_u_valuti)-1,(valuta!="HRK" & lag(valuta)!="HRK")~(cj_udjela_u_valuti*(tecaj/lag(tecaj)))/lag(cj_udjela_u_valuti)-1),lnav=lag(nav)) %>% ungroup() %>% filter(datum=="2022-12-31" & !is.na(prinos)) %>% mutate(ponder=lnav/sum(lnav,na.rm=T)) %>% summarise(Prinos=weighted.mean(prinos,w=lnav,na.rm=T))
pom2 <- nav_opce %>% mutate(godina=ceiling_date(datum,unit = "year")-1) %>% filter(vrsta1=="UCITS" & cj_udjela_u_valuti!=0 & !(subjekt %in% c("InterCapital Bond (HRICAMUCAON0)", "InterCapital SEE Equity  (HRICAMUCATW0)") & year(datum)==2016) & !(subjekt=="InterCapital Smart II   (HRVBINUVBSM7)" & year(datum)==2014) & datum==godina) %>% select(datum,oib,subjekt,klasa,vrsta4,drustvo,nav,cj_udjela_u_valuti,valuta,tecaj) %>% group_by(oib,subjekt,klasa,vrsta4,drustvo) %>% arrange(datum) %>% mutate(promjena_valute=case_when(valuta==lag(valuta)~0,T~1),prinos=case_when(promjena_valute==0 | drustvo=="INTERCAPITAL ASSET MANAGEMENT d.o.o."~cj_udjela_u_valuti/lag(cj_udjela_u_valuti)-1,valuta %in% c("EUR","USD") & lag(valuta)=="HRK"~(cj_udjela_u_valuti*tecaj)/lag(cj_udjela_u_valuti)-1,valuta=="HRK" & lag(valuta) %in% c("EUR","USD")~(cj_udjela_u_valuti/lag(tecaj))/lag(cj_udjela_u_valuti)-1,(valuta!="HRK" & lag(valuta)!="HRK")~(cj_udjela_u_valuti*(tecaj/lag(tecaj)))/lag(cj_udjela_u_valuti)-1),lnav=lag(nav)) %>% ungroup() %>% filter(datum=="2022-12-31" & !is.na(prinos) & vrsta4=="Dionički") %>% mutate(ponder=lnav/sum(lnav,na.rm=T))

##%>% group_by(vrsta4) %>% summarise(Prinos=weighted.mean(prinos,w=lnav,na.rm=T))
write_clip(pom2)

# Ultimativni kod za prinose UCITS-a ####
pom <- nav_opce %>% mutate(godina=ceiling_date(datum,unit = "year")-1,vrsta=if_else(vrsta4 %in% c("Obveznički","Dionički"),vrsta4,"Ostali")) %>% filter(vrsta1=="UCITS" & cj_udjela_u_valuti!=0 & !(subjekt %in% c("InterCapital Bond (HRICAMUCAON0)", "InterCapital SEE Equity  (HRICAMUCATW0)") & year(datum)==2016) & !(subjekt=="InterCapital Smart II   (HRVBINUVBSM7)" & year(datum)==2014) & datum==godina) %>% select(datum,oib,subjekt,klasa,vrsta4,vrsta,drustvo,nav,cj_udjela_u_valuti,valuta,tecaj) %>% group_by(oib,subjekt,klasa,vrsta4,vrsta,drustvo) %>% arrange(datum) %>% mutate(promjena_valute=case_when(valuta==lag(valuta)~0,T~1),prinos=case_when(promjena_valute==0 | drustvo=="INTERCAPITAL ASSET MANAGEMENT d.o.o."~cj_udjela_u_valuti/lag(cj_udjela_u_valuti)-1,valuta %in% c("EUR","USD") & lag(valuta)=="HRK"~(cj_udjela_u_valuti*tecaj)/lag(cj_udjela_u_valuti)-1,valuta=="HRK" & lag(valuta) %in% c("EUR","USD")~(cj_udjela_u_valuti/lag(tecaj))/lag(cj_udjela_u_valuti)-1,(valuta!="HRK" & lag(valuta)!="HRK")~(cj_udjela_u_valuti*(tecaj/lag(tecaj)))/lag(cj_udjela_u_valuti)-1),lnav=lag(nav))

ggplot(pom %>% filter(!is.na(prinos)),aes(x=prinos)) + geom_histogram(binwidth = .05) + geom_rug() + scale_x_continuous(limits = c(-1,1),breaks = seq(-1,1,by=.1))

#%>% group_by(datum,vrsta) %>% summarise(p25=quantile(prinos,probs=0.25,na.rm=T),p75=quantile(prinos,probs=0.75,na.rm=T),medijan=median(prinos,na.rm=T),prinos_avg=weighted.mean(prinos,w = lnav,na.rm = T)) %>% na.omit()




# 3. Kretanje novozaraženih ####
# broj zaraženih
covid <- fromJSON("https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/json/")
pom1 <- covid %>% mutate(year_week=paste(year_week,"5",sep="-"),datum=as.Date(year_week,"%Y-%U-%u")) %>% select(datum,indicator,ctry=country_code,slucajevi=rate_14_day,kontinent=continent) %>% mutate(slucajevi=as.numeric(slucajevi)) %>% filter(ctry %in% reg$ctry & indicator=="cases" & ctry!="LUX" & ctry!="CYP") %>% filter(datum>="2020-02-15")
emf(file = "Z:/DSR/FinancijskaStabilnost/slike/03_pandemija.emf",width = 8,height = 6)
ggplot() + geom_line(data=subset(pom1,ctry!="HRV"),aes(x=datum,y=slucajevi,group=ctry),col="grey",size=1.05,alpha=0.6) + geom_line(data=subset(pom1,ctry=="HRV"),aes(x=datum,y=slucajevi),col="#2DBAA7",size=1.6) + boje_col + labs(x="",y="") + scale_x_date(date_breaks = "6 months",date_labels = "%b-%y.")+ plot_annotation(title="Slika 3. Vrhunac pandemije zabilježen početkom 2022. godine", subtitle="Kumulativni 14-dnevni broj slučajeva na 100.000 stanovnika", caption="Napomena: Linije na grafikonu označuju kretanje indikatora za pojedinačne zemlje EU-a, dok istaknuta linija označuje kretanje \nindikatora za Hrvatsku.\nIzvori: ECDC, Our World in Data") +scale_y_continuous(labels = hr_format)
dev.off()
rm(covid,pom1)

# XX. JVI Prezentacija ####
gtema <- theme_minimal() + theme(panel.background = element_rect(fill="white",linetype = 0),plot.background = element_rect(fill="white",linetype = 0),legend.box.background = element_rect(fill="white",linetype = 0),text = element_text(colour = "#000000"),plot.caption = element_text(hjust = 0),legend.position = "top",legend.title = element_blank(),panel.border = element_blank(),axis.line = element_blank(),panel.grid.major = element_line(size = 0.5, linetype = "dotted",colour = "#233142"))
theme_set(gtema)
# Slika 1: Turska i Hrvatska
pom <- read_excel("D:/Edukacije_službena_putovanja_i_sl/JVI 2235/podaci usporedba.xlsx",sheet="Sheet1") %>% mutate(net_notional=net_notional/1000000000)
p1 <- ggplot(data=pom %>% mutate(ref_entity=factor(ref_entity,levels=c("IT","FR","TR","RU","PT","UA","PL","RO","HR"))),aes(x=ref_entity,y=net_notional,fill=group_column)) + geom_col(alpha=.8) + boje_fill + labs(subtitle = "Net Notional, USD bn",x="",y="") + theme(legend.position="none")
p2 <-ggplot(data=pom %>% mutate(ref_entity=factor(ref_entity,levels=c("TR","RU","FR","IT","PT","UA","PL","RO","HR"))),aes(x=ref_entity,y=contracts,col=group_column)) + geom_point(size=10,alpha=.8) + boje_col + labs(subtitle="Number of contracts",x="",y="") + scale_y_continuous(label=hr_format) + theme(legend.position="none")
p1 + p2 + plot_annotation(title="Comparison of CDS markets - 2th April 2022",caption=str_c(str_wrap("Note: IT - Italija, FR - France, TR - Turkey, RU - Russia, PT - Portugal, UA - Ukraine, PL - Poland, RO - Romania, HR - Croatia",130),"\nSource: DTCC, CDS Kinetics, Credit Default Swaps Data"))
# Slika 2: Turska 
pom <- read_excel("D:/Edukacije_službena_putovanja_i_sl/JVI 2235/prezentacija/podaci usporedba.xlsx",sheet="Sheet2") %>% mutate(net_notional=net_notional/1000000000)
p1 <- ggplot(data=pom %>% mutate(ref_entity=factor(ref_entity,levels=c("IT","MX","KR","CN","BR","ZA","FR","TR","RU","CO"))),aes(x=ref_entity,y=net_notional,fill=group_column)) + geom_col(alpha=.8) + boje_fill + labs(subtitle = "Net Notional, USD bn",x="",y="") + theme(legend.position="none")
p2 <-ggplot(data=pom %>% mutate(ref_entity=factor(ref_entity,levels=c("IT","MX","ZA","TR","BR","RU","CO","CN","KR","FR"))),aes(x=ref_entity,y=contracts,col=group_column)) + geom_point(size=10,alpha=.8) + boje_col + labs(subtitle="Number of contracts",x="",y="") + scale_y_continuous(label=hr_format) + theme(legend.position="none")
p1 + p2 + plot_annotation(title="High demand for CDS contracts on Turkey sovereign bonds - 2th April 2022",caption=str_c(str_wrap("Note: IT - Italija, MX - Mexico, KR - Korea, CN - China, BR - Brasil, ZA - South Africa, FR - France, TR - Turkey, RU - Russia, CO - Columbia",130),"\nSource: DTCC, CDS Kinetics, Credit Default Swaps Data"))

#

# Vježbanje 3.2.2023. Slika1_Reforme_i_kriza_CEE ####
pom <- read_excel("Slika1_pom.xlsx",sheet="Sheet2") %>% mutate(godina=as.Date(godina),drzava=factor(drzava),događaj=factor(događaj,levels=c("Uvođenje II. stupa","Ukidanje obveznog II. stupa","Smanjenje stope doprinosa","Bez značajnijih izmjena")))
ggplot(pom %>% filter(događaj=="Uvođenje II. stupa"),aes(x=godina,y=drzava)) + geom_point(shape=15) + geom_point(data=pom %>% filter(događaj!="Uvođenje II. stupa"),aes(x=godina,y=drzava,color=događaj),shape=1)

ggplot() + geom_point(data=pom %>% filter(događaj=="Uvođenje II. stupa"),mapping=aes(x=godina,y=drzava),shape=1) + geom_point(data=pom %>% filter(događaj!="Uvođenje II. stupa"),mapping=aes(x=godina,y=drzava,color=događaj),shape=15) + geom_line()

kriza <- data.frame(begin=as.Date("2008-01-01"),end=as.Date("2012-01-01"),ymin="Estonija",ymax="Mađarska")
ggplot(data=pom,aes(x=godina,y=drzava)) + geom_line() + geom_point(aes(color=događaj)) + geom_rect(data=kriza,aes(xmin=begin,xmax=end,ymin=-Inf,ymax=Inf),inherit.aes=FALSE,fill="blue",alpha=.5) + scale_x_date(limits=as.Date(c("1998-01-01","2022-01-01")),date_breaks="1 year",date_labels="%y.") + theme(legend.position="top",legend.box.margin = margin(6, 116, 6, 6))

ggplot(data=pom,aes(x=godina,y=drzava)) + geom_line() + geom_point(aes(color=događaj)) + geom_rect(data=kriza,aes(xmin=begin,xmax=end,ymin=-Inf,ymax=Inf),inherit.aes=FALSE,fill="blue",alpha=.5) + scale_x_date(limits=as.Date(c("1998-01-01","2022-01-01")),date_breaks="1 year",date_labels="%y.") + theme(legend.position="top")

# Vježbanje 3.2. Slika: Bruto omjer zamjene dohotka ####
gtema <- theme_minimal() + theme(panel.background = element_rect(fill="white",linetype = 0),plot.background = element_rect(fill="white",linetype = 0),legend.box.background = element_rect(fill="white",linetype = 0),text = element_text(colour = "#000000"),legend.position = "top",legend.title = element_blank(),panel.border = element_blank(),axis.line = element_blank(),panel.grid.major = element_line(size = 0.5, linetype = "dotted",colour = "#233142"))
theme_set(gtema)

ggplot(pom %>% filter(država!="Hrvatska")) + geom_boxplot(aes(x=datum,y=omjer,group=datum)) + scale_y_continuous(limits=c(.3,.7),breaks=c(.3,.4,.5,.6,.7),labels=hr_format_postotak) + geom_point(data=pom %>% filter(država=="Hrvatska"),aes(x=datum,y=omjer),shape=23,size=5,fill="red",color="blue") + scale_x_date(date_breaks="1 year",date_label="%Y.") + labs(x="",y="",caption="ja sam caption") + plot_annotation(title=str_c(str_wrap("Slika 2. Bruto omjer zamjene dohotka u Hrvatskoj konvergira prosjeku CEE zemalja blablalbla bla bla blblall",80),"\nDistribucija bruto omjera zmajene dohotka","\nĐe si ba š'a ima!?")) + theme(plot.caption=element_text(hjust=1))

#subtitle = "Distribucija bruto omjera zamjene dohotka zemalja srednje i istočne Europe (Mađarska, Poljska, Slovačka,\nSlovenija, Bugarska, Rumunjska, Litva, Latvija i Estonija) te podatak o bruto omjeru zamjene dohotka za Hrvatsku",caption = "Bruto omjer zamjene dohotka (eng. gross replacement rate) predstavlja omjer medijalne vrijednosti mirovine stanovništva starog \nizmeđu 65-74 proizašle iz rada i medijalne vrijednosti dohotka stanovništva starog između 50-59, bez oduzimanja plaćenih poreza. \nZa godinu 2009. nedostaje podatak za Hrvatsku, dok za godinu 2020. nedostaju podaci za Latviju, Litvu, Poljsku i Slovačku.\nIzvor: Eurostat") 


pom111 <- midore %>% filter(datum %in% c(dtm,yoy0,yoy0-1) & vrsta_iznosa=="Zaračunata bruto premija" & razina2!="Bilješka") %>% group_by(datum,subjekt,razina2) %>% summarise(iznos=sum(iznos,na.rm=T)/1000) %>% mutate(stupac=str_c(razina2,ifelse(datum==dtm,"t1","t0"),sep = "-")) %>% ungroup() %>% select(subjekt,stupac,iznos) %>% spread(stupac,iznos)

# Vježbanje 9. siječanj 2023. ####
ggplot(pom %>% filter(država!="Hrvatska"),aes(x=datum,y=omjer,group=datum)) + geom_boxplot() + geom_point(data=pom %>% filter(država=="Hrvatska"),aes(datum,omjer),shape=23,color="black",fill="red",size=2.5) + scale_x_date(date_breaks="1 year",date_labels="%Y.") + scale_y_continuous(breaks=seq(),labels=hr_format_postotak) + labs(x="",y="") + plot_annotation(title="YEAAAAAAH!")

# slika sa kartom europe
## Instaliraj paket
library("rworldmap") 
## Napravi objekt geografske mape sa popisom svih država na svijetu i njihovim koordinatama
mapa <- getMap()
## Filtriraj samo države Europe
EU_drzave <- which(mapa$NAME %in% pom$country)















# Nanobit 21.1.2023. ####
# Library
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(readxl)
library(clipr)
# Vremenske postavke
Sys.setlocale("LC_TIME", "English")
# Da ti ne ispisuje znanstveni format broja
options(scipen=999)
# Funkcija za decimalne zareze umjesto točaka
hr_format <- function(x) format(x, big.mark = ".", decimal.mark =",", scientific = FALSE)
hr_format_postotak <- percent_format(big.mark = ".", decimal.mark =",")
# tema za slike
gtema <- theme_minimal() + theme(panel.background = element_rect(fill="white",linetype = 0),plot.background = element_rect(fill="white",linetype = 0),legend.box.background = element_rect(fill="white",linetype = 0),text = element_text(colour = "#000000"),plot.caption = element_text(hjust = 0),legend.position = "top",legend.title = element_blank(),panel.border = element_blank(),axis.line = element_blank(),panel.grid.major = element_line(size = 0.5, linetype = "dotted",colour = "#233142"))
theme_set(gtema)
# paleta boja
boje_fill <- scale_fill_manual(values = c("#155e63","#e84545","#25a55f","#ffc93c","#9b5d73","#ff7c38","#7e6752","#679186","#2e99b0","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314"))
boje_col <- scale_color_manual(values = c("#155e63","#e84545","#25a55f","#ffc93c","#9b5d73","#ff7c38","#7e6752","#679186","#2e99b0","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314"))
# dataset
temp <- read_excel("D:/JuniorAnalyst/Nanobit/2023_Junior Data Analyst.xlsx", sheet="KPIs") %>% mutate(datum=as.Date(datum))
kpi <- temp %>% arrange(datum) %>% mutate(d_inst=(lag(inst)/inst)-1,d_du=(lag(du)/du)-1,d_rev=(lag(rev)/rev)-1,mc=case_when(datum>"2022-01-30" & datum<"2022-02-05"~1,T~0)) 
# time series
mc <- data.frame(begin=as.Date("2022-01-31"),end=as.Date("2022-02-06"))
ggplot(kpi,aes(x=datum,y=inst)) + geom_line() + geom_rect(data=mc,aes(xmin=begin,xmax=end,ymin=-Inf,ymax=+Inf),inherit.aes=FALSE,fill="red",alpha=0.15) + labs(x="",y="") + scale_x_date(date_breaks="7 days",date_labels="%d/%m") + scale_y_continuous(labels=hr_format) + plot_annotation(title="Number of daily installs",caption="Note: Red area represents the period of active marketing campaign from Jan 31th to Feb 6th.")
ggplot(kpi,aes(x=datum,y=du)) + geom_line() + geom_rect(data=mc,aes(xmin=begin,xmax=end,ymin=-Inf,ymax=+Inf),inherit.aes=FALSE,fill="red",alpha=0.15) + labs(x="",y="") + scale_x_date(date_breaks="7 days",date_labels="%d/%m") + scale_y_continuous(labels=hr_format) + plot_annotation(title="Number of daily active users",caption="Note: Red area represents the period of active marketing campaign from Jan 31th to Feb 6th.")
ggplot(kpi,aes(x=datum,y=rev)) + geom_line() + geom_rect(data=mc,aes(xmin=begin,xmax=end,ymin=-Inf,ymax=+Inf),inherit.aes=FALSE,fill="red",alpha=0.15) + labs(x="",y="") + scale_x_date(date_breaks="7 days",date_labels="%d/%m") + scale_y_continuous(labels=hr_format) + plot_annotation(title="Daily revenue",subtitle="In dollars",caption="Note: Red area represents the period of active marketing campaign from Jan 31th to Feb 6th.")
# rev_sd
rev_sd1 <- kpi %>% filter(du<384000) %>% summarise(rev_sd=sd(rev))
rev_sd2 <- kpi %>% filter(du>384000) %>% summarise(rev_sd=sd(rev))
# correlation - pearson
cor(kpi$inst,kpi$rev,use="complete.obs")
cor(kpi$du,kpi$rev,use="complete.obs")
cor(kpi$du,kpi$inst,use="complete.obs")
# liner model
lm <- lm(data=kpi,d_rev~lag(d_inst)+lag(d_du)+mc)
summary(lm)
ggplot(data=kpi %>% filter(daily_user>384000),aes(x=daily_user,y=rev)) + geom_point() + geom_smooth(method=lm)
# panel
inst <- read_excel("D:/JuniorAnalyst/Nanobit/2023_Junior Data Analyst.xlsx", sheet="Installs") %>% rename(datum=Date) %>% mutate(datum=as.Date(datum)) %>% gather("drzava","inst",-datum)
du <- read_excel("D:/JuniorAnalyst/Nanobit/2023_Junior Data Analyst.xlsx", sheet="Daily Active Users") %>% rename(datum=Date) %>% mutate(datum=as.Date(datum)) %>% gather("drzava","du",-datum)
rev <- read_excel("D:/JuniorAnalyst/Nanobit/2023_Junior Data Analyst.xlsx", sheet="Revenue") %>% rename(datum=Date) %>% mutate(datum=as.Date(datum)) %>% gather("drzava","rev",-datum)
panel_dataset <- inst %>% inner_join(du,by=c("datum","drzava")) %>% inner_join(rev,by=c("datum","drzava")) %>% mutate(mc=if_else(datum>"2022-01-30" & datum<"2022-02-05",1,0)) %>% arrange(datum) %>% group_by(drzava) %>% mutate(d_inst=(inst/lag(inst))-1,d_du=(du/lag(du))-1,d_rev=(rev/lag(rev))-1) %>% ungroup()
panel_data <- plm::pdata.frame(panel_dataset, index=c("drzava","datum"), drop.index=TRUE, row.names=TRUE) %>% na.omit()
panel <- plm::plm(formula=d_rev~lag(d_inst)+lag(d_du)+mc, data = panel_data, model = "within", effect="individual")
summary(panel)
# grafička analiza po državama
kpi_countries <- panel_dataset %>% gather("var","iznos",-c(datum,drzava)) %>% filter(var!="mc")
ggplot(kpi_countries %>% filter(var=="du"),aes(datum,iznos,color=drzava)) + geom_line(size=1) + geom_hline(yintercept=0)

ggplot(kpi_countries %>% filter(var=="rev") %>% group_by(drzava) %>% mutate(avg=mean(iznos,na.rm=T)) %>% ungroup() %>% filter(avg>1250),aes(datum,iznos,color=drzava)) + geom_line(size=1) + geom_hline(yintercept=1250)
ggplot(kpi_countries %>% filter(var=="inst") %>% group_by(drzava) %>% mutate(avg=mean(iznos,na.rm=T)) %>% ungroup() %>% filter(avg>1000),aes(datum,iznos,color=drzava)) + geom_line(size=1) + geom_hline(yintercept=1000)
ggplot(kpi_countries %>% filter(var=="du") %>% group_by(drzava) %>% mutate(avg=mean(iznos,na.rm=T)) %>% ungroup() %>% filter(avg>30000),aes(datum,iznos,color=drzava)) + geom_line(size=1) + geom_hline(yintercept=30000)
# drugi zadatak
temp <- read.csv("D:/JuniorAnalyst/Nanobit/2023_ABtest_data.csv") %>% rename(datum=`ď.żdate`) %>% mutate(datum=as.Date(datum))
temp1 <- temp %>% filter(group=="Control" & level==10) %>% group_by(id) %>% summarise(rev=sum(revenue))
temp2 <- temp %>% filter(group=="Treatment" & level==10) %>% group_by(id) %>% summarise(rev=sum(revenue))

## ukupna zarada
pom1 <- temp %>% filter(group=="Control" & level==10) %>% summarise(total=sum(revenue,na.rm=T)) %>% mutate(group="control",level="l10")
pom2 <- temp %>% filter(group=="Control") %>% summarise(total=sum(revenue,na.rm=T)) %>% mutate(group="control",level="all")
pom3 <- temp %>% filter(group=="Treatment" & level==10) %>% summarise(total=sum(revenue,na.rm=T)) %>% mutate(group="treatment",level="l10")
pom4 <- temp %>% filter(group=="Treatment") %>% summarise(total=sum(revenue,na.rm=T)) %>% mutate(group="treatment",level="all")
pom <- rbind(pom1,pom2,pom3,pom4)

ggplot(pom %>% filter(level=="l10"),aes(x=`group`,y=`total`)) + geom_col() + boje_col

## prosječna zarada po danu i igraču
pom1 <- temp %>% filter(datum<"2022-08-08" & level==10) %>% summarise(avg=mean(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="before",level="l10")
pom2 <- temp %>% filter(datum<"2022-08-08") %>% group_by(datum,id) %>% summarise(rev=sum(revenue)) %>% ungroup() %>% summarise(avg=mean(rev),sd_plus=sd(rev),sd_minus=sd(rev)*-1) %>% mutate(group="before",level="all")

pom3 <- temp %>% filter(group=="Control" & level==10) %>% summarise(avg=mean(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="control",level="l10")
pom4 <- temp %>% filter(group=="Control") %>% summarise(avg=mean(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="control",level="all")
pom5 <- temp %>% filter(group=="Treatment" & level==10) %>% summarise(avg=mean(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="treatment",level="l10")
pom6 <- temp %>% filter(group=="Treatment" & level==10) %>% summarise(avg=mean(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="treatment",level="all")
pom <- rbind(pom3,pom4,pom5,pom6)

ggplot(pom %>% filter(level=="l10"),aes(x=`group`,y=`avg`)) + geom_col() + boje_col

## prosječna zarada po danu i igraču za one koji su potrošili više od nula $
pom1 <- temp %>% filter(datum<"2022-08-08" & level==10) %>% summarise(avg=mean(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="before",level="l10")
pom2 <- temp %>% filter(datum<"2022-08-08") %>% group_by(datum,id) %>% summarise(rev=sum(revenue)) %>% ungroup() %>% summarise(avg=mean(rev),sd_plus=sd(rev),sd_minus=sd(rev)*-1) %>% mutate(group="before",level="all")

pom3 <- temp %>% filter(revenue>0 & group=="Control" & level==10) %>% summarise(avg=mean(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="control",level="l10")
pom4 <- temp %>% filter(revenue>0 & group=="Control") %>% summarise(avg=mean(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="control",level="all")
pom5 <- temp %>% filter(revenue>0 & group=="Treatment" & level==10) %>% summarise(avg=mean(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="treatment",level="l10")
pom6 <- temp %>% filter(revenue>0 & group=="Treatment" & level==10) %>% summarise(avg=mean(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="treatment",level="all")
pom <- rbind(pom3,pom4,pom5,pom6)

ggplot(pom %>% filter(level=="l10"),aes(x=`group`,y=`avg`)) + geom_col() + boje_col 

## broj onih iznad nule (udio)
pom1 <- temp %>% filter(group=="Control" & level==10) %>% mutate(help=if_else(revenue!=0,"more","less")) %>% group_by(help) %>% summarise(n=n()) %>% ungroup() %>% spread(help,n) %>% mutate(total=more+less,share=more/total,group="control",level="l10")
pom2 <- temp %>% filter(group=="Treatment" & level==10) %>% mutate(help=if_else(revenue!=0,"more","less")) %>% group_by(help) %>% summarise(n=n()) %>% ungroup() %>% spread(help,n) %>% mutate(total=more+less,share=more/total,group="treatment",level="l10")
pom <- rbind(pom1,pom2)

ggplot(pom %>% filter(level=="l10"),aes(x=`group`,y=`share`)) + geom_col() + boje_col

## medijalna zarada po danu i igraču za one koji su potrošili više od nula $
pom1 <- temp %>% filter(datum<"2022-08-08" & level==10) %>% summarise(avg=mean(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="before",level="l10")
pom2 <- temp %>% filter(datum<"2022-08-08") %>% group_by(datum,id) %>% summarise(rev=sum(revenue)) %>% ungroup() %>% summarise(avg=mean(rev),sd_plus=sd(rev),sd_minus=sd(rev)*-1) %>% mutate(group="before",level="all")

pom3 <- temp %>% filter(revenue>0 & group=="Control" & level==10) %>% summarise(avg=median(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="control",level="l10")
pom4 <- temp %>% filter(revenue>0 & group=="Control") %>% summarise(avg=median(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="control",level="all")
pom5 <- temp %>% filter(revenue>0 & group=="Treatment" & level==10) %>% summarise(avg=median(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="treatment",level="l10")
pom6 <- temp %>% filter(revenue>0 & group=="Treatment" & level==10) %>% summarise(avg=median(revenue),sd_plus=sd(revenue),sd_minus=sd(revenue)*-1) %>% mutate(group="treatment",level="all")
pom <- rbind(pom3,pom4,pom5,pom6)

ggplot(pom %>% filter(level=="l10"),aes(x=`group`,y=`avg`)) + geom_col() + boje_col 





















