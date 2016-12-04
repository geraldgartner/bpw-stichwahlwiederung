#Analyse der Stichwahl
library(ggthemes)
library(ggThemeAssist)
library(ggplot2)
library(dplyr)
library(tidyr)
library(git2r)
library(formatR)
library(scales)
library(grid)
library(extrafont)
library(corrplot)
library(plotly)
library(reshape2)

#Unser Style
source('theme_fivethirtyeight.R')

#Testing
ggplot(mpg, aes(x = cty, y = hwy, color = factor(cyl))) +
  geom_jitter() +
  labs(
    x = "City mileage/gallon",
    y = "Highway mileage/gallon",
    color = "Cylinders")+
  labs(title = "MLB run scoring,\n 1901-2015",
       subtitle = "Run scoring has been falling for 15 years,\n reversing a 30 year upward trend",
       caption = "Source: the Lahman baseball database", 
       x = "year", y = "team runs per game") +
  theme_fivethirtyeight()+
  scale_colour_fivethirtyeight()+
  coord_fixed() #saving aspect ratio
  #bank_slopes(cty,hwy, method = 'as')

library("googlesheets")
suppressPackageStartupMessages(library("dplyr"))

(my_sheets <- gs_ls())
gemeindedaten <- gs_key("1I8oAyCA4OTGLjnCINtvAzeXc-SU4n72WUvk5f8ZkExQ")

#Alle Worksheets aus dem GSheet Gemeindedaten importieren
demografie <- gs_read(gemeindedaten, ws = 'pivoteddem', col_names = TRUE)
v1stichwahl <- gs_read(gemeindedaten, ws = "stichwahl-v1", col_names = TRUE)
v2stichwahl <- gs_read(gemeindedaten, ws = "stichwahl-v2", col_names = TRUE)
wahlgang1 <- gs_read(gemeindedaten, ws = "wahlgang-v1", col_names = TRUE)
kaufkraft <- gs_read(gemeindedaten, ws = "kaufkraft", col_names = TRUE)
medianeinkommen <- gs_read(gemeindedaten, ws = "medianeinkommen", col_names = TRUE)
alter <- gs_read(gemeindedaten, ws = 'alter', col_names = TRUE)
wahltagsbefragungen <- gs_read(gemeindedaten, ws = 'wahltagsbefragungen', col_names = TRUE)

#Reshapen der Wahlergebnisse auf Long-Format

#Wahlgang 1
wahlgang1 <- wahlgang1 %>% gather(kandidat, ergebnis, griss:vdb)
wahlgang1$pct <- wahlgang1$ergebnis/wahlgang1$gueltig

#Stichwahl 1
v1stichwahl <- v1stichwahl %>% gather(kandidat, ergebnis, hofer:vdb)
v1stichwahl$pct <- v1stichwahl$ergebnis/v1stichwahl$gueltig 

#Stichwahl 2
v2stichwahl <- v2stichwahl %>% gather(kandidat, ergebnis, hofer:vdb)
v2stichwahl$pct <- v2stichwahl$ergebnis/v2stichwahl$gueltig 


#Anlegen der Prozentspalten für die Korrelationen-Analyse
alter$junge_pct <- alter$bis29/alter$wahlalter_gesamt
alter$ueber60_pct<- alter$ueber60/alter$wahlalter_gesamt

demografie$geburtsland_geburtsland_at_pct <- demografie$geburtsland_at/demografie$geburtsland_gesamt
demografie$geburtsland_ex_yugo_pct <- demografie$geburtsland_ex_yugo/demografie$geburtsland_gesamt
demografie$geburtsland_tuerkei_pct <- demografie$geburtsland_tuerkei/demografie$geburtsland_gesamt

demografie$pflichtschule_pct <- demografie$pflichtschule/demografie$bildung_gesamt
demografie$lehrabschluss_pct <- demografie$lehrabschluss/demografie$bildung_gesamt
demografie$mittlereundhoehere_pct <- demografie$mittlereundhoehere/demografie$bildung_gesamt
demografie$hochschuleuakadamie_pct <- demografie$hochschuleuakadamie/demografie$bildung_gesamt

demografie$arbeiter_pct <- demografie$arbeiter/demografie$aktiv_erwerbstätige_gesamt
demografie$angestellte_pct <- demografie$angestellte/demografie$aktiv_erwerbstätige_gesamt
demografie$selbststaendige_pct <- demografie$selbstständig/demografie$aktiv_erwerbstätige_gesamt

#filter relevant calculations
pctdemografie <- demografie[ , c("gkz", "geburtsland_geburtsland_at_pct", 
                                 "geburtsland_ex_yugo_pct",  
                                 "geburtsland_tuerkei_pct",   
                                 "pflichtschule_pct",         
                                 "lehrabschluss_pct",       
                                 "mittlereundhoehere_pct",       
                                 "hochschuleuakadamie_pct",        
                                 "arbeiter_pct",                 
                                 "angestellte_pct",            
                                 "selbststaendige_pct"      
)]

pctalter <- alter[ , c("gkz", "junge_pct", "ueber60_pct")]


#Mergen der Sheets für weitere Analyse
dem <- merge(x=pctdemografie, y=v1stichwahl, by.x = "gkz", by.y = "gkz", incomparables = NA)
alt <- merge(x=pctalter, y=v1stichwahl, by.x = "gkz", by.y = "gkz", incomparables = NA)
kk <- merge(x=kaufkraft, y=v1stichwahl, by.x = "gkz", by.y = "gkz", incomparables = NA)


#Filtern der Kandiaten vor der Korrelationsmatrix

demhofer <- filter(dem, dem$kandidat=="hofer")
demvdb <- filter(dem, dem$kandidat=="vdb" )

althofer <- filter(alt, alt$kandidat=="hofer")
altbvdb <- filter(alt, alt$kandidat=="vdb" )

demhofer$wb_pct <- demhofer$gueltig/demhofer$wb

demhofercor <- demhofer[, c(2:11, 19,20)]
demvdbcor <- demvdb[, c(2:11, 19)]

althofercor <- althofer[, c(2:3, 11)]
altvdbcor <- altbvdb[, c(2:3, 11)]

#KORRELATIONEN-TABELLE FÜR HOFER

demhofercor <- cor(demhofercor, use="pairwise.complete.obs")
round(demhofercor, digits=2)

althofercor <- cor(althofercor, use="pairwise.complete.obs")
round(althofercor, digits=2)

#BESSERE FARBPALETTE
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
as.matrix(as.data.frame(demhofer))
corrplot(demhofercor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         col=col(200), addCoef.col="black",order="AOE")

as.matrix(as.data.frame(althofercor))
corrplot(althofercor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         col=col(200), addCoef.col="black", order="AOE")

#KORRELATIONEN-MATRIX FÜR VDB

#Demografie
demvdbcor <- cor(demvdbcor, use="pairwise.complete.obs")
round(demvdbcor, digits=2)
corrplot(demvdbcor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         col=col(200), addCoef.col="black",order="AOE")

#Alter
altvdbcor <- cor(altvdbcor, use="pairwise.complete.obs")
round(altvdbcor, digits=2)

as.matrix(as.data.frame(altvdbcor))
corrplot(altvdbcor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         col=col(200), addCoef.col="black", order="AOE")

#SCATTERPLOT FÜR BEZIEHUNGEN
source('theme_fivethirtyeight.R')

#Je mehr Arbeiter, desto mehr Hofer
arbeiterplot <- ggplot(dem, aes(x=arbeiter_pct, y=pct, colour=kandidat)) +
  geom_point(alpha=1/4) + 
  facet_grid(kandidat ~ ., labeller=labeller(.default=kandidaten)) +
  geom_smooth(method=lm)  +
  scale_y_continuous(labels = percent)+
  scale_x_continuous(labels = percent)+
  labs(title = "Je mehr Arbeiter, desto mehr Hofer-Wähler",
       subtitle = "Je höher der Arbeiter-Anteil pro Gemeinde,\ndesto höher ist der Hofer-Stimmenanteil", 
      caption = "Quellen: BMI, Statistik Austria",
       x = "Stimmenanteil für den Kandidaten",
       y = "Anteil der Arbeiter") +
  guides(fill=FALSE) +
  scale_colour_manual(values = c("hofer"="#7A8FCC", "vdb"="#548750")) +
  theme_fivethirtyeight()+
  theme(strip.background = element_rect(fill="gray97"))+
  theme(legend.position ="none")+ 
  theme(plot.subtitle=element_text(size=12))
  #coord_fixed() #saving aspect ratio

plot(arbeiterplot)
#ggplotly(arbeiterplot)%>% config(displayModeBar = F)

#Je mehr Akademiker, desto mehr VdB-Wähler
akademikerplot <- ggplot(dem, aes(x=hochschuleuakadamie_pct, y=pct, colour=kandidat)) +
  geom_point(alpha=1/4) + 
  facet_grid(kandidat ~ ., labeller=labeller(.default=kandidaten)) +
  geom_smooth(method=lm)  +
  scale_y_continuous(labels = percent)+
  scale_x_continuous(labels = percent)+
  labs(title = "Je mehr Akademiker, desto mehr VdB-Wähler",
       subtitle = "Je höher der Akademiker-Anteil pro Gemeinde,\ndesto höher ist der VdB-Stimmenanteil", 
       caption = "Quellen: BMI, Statistik Austria",
       x = "Stimmenanteil für den Kandidaten",
       y = "Anteil der Akademiker") +
  guides(fill=FALSE) +
  scale_colour_manual(values = c("hofer"="#7A8FCC", "vdb"="#548750")) +
  theme_fivethirtyeight()+
  theme(strip.background = element_rect(fill="gray97"))+
  theme(legend.position ="none")+ 
  theme(plot.subtitle=element_text(size=12))
#coord_fixed() #saving aspect ratio

plot(akademikerplot)

#Je höher der Anteil der Personen mit Pflichtschulabschluss, desto mehr Hofer
pflichtschulplot <- ggplot(dem, aes(x=pflichtschule_pct, y=pct, colour=kandidat)) +
  geom_point(alpha=1/4) + 
  facet_grid(kandidat ~ ., labeller=labeller(.default=kandidaten)) +
  geom_smooth(method=lm)  +
  scale_y_continuous(labels = percent)+
  scale_x_continuous(labels = percent)+
  labs(title = "Je mehr Arbeiter, desto mehr Hofer-Wähler",
       subtitle = "Je höher der Anteil der Personen mit Pflichtschulabschluss, desto mehr Hofer", 
       caption = "Quellen: BMI, Statistik Austria",
       x = "Stimmenanteil für den Kandidaten",
       y = "Anteil der Personen mit Pflichtschulabschluss") +
  guides(fill=FALSE) +
  scale_colour_manual(values = c("hofer"="#7A8FCC", "vdb"="#548750")) +
  theme_fivethirtyeight()+
  theme(strip.background = element_rect(fill="gray97"))+
  theme(legend.position ="none")+ 
  theme(plot.subtitle=element_text(size=12))
#coord_fixed() #saving aspect ratio
plot(pflichtschulplot)


#Je mehr Junge, desto mehr VdB
jungeplot <- ggplot(alt, aes(x=junge_pct, y=pct, colour=kandidat)) +
  geom_point(alpha=1/4) + 
  facet_grid(kandidat ~ ., labeller=labeller(.default=kandidaten)) +
  geom_smooth(method=lm)  +
  scale_y_continuous(labels = percent)+
  scale_x_continuous(labels = percent)+
  labs(title = "Je mehr Junge, desto mehr VdB-Wähler",
       subtitle = "Je höher der Anteil unter 29-Jähriger pro Gemeinde,\ndesto höher ist der VdB-Stimmenanteil", 
       caption = "Quellen: BMI, Statistik Austria",
       x = "Stimmenanteil für den Kandidaten",
       y = "Anteil der Arbeiter") +
  guides(fill=FALSE) +
  scale_colour_manual(values = c("hofer"="#7A8FCC", "vdb"="#548750")) +
  theme_fivethirtyeight()+
  theme(strip.background = element_rect(fill="gray97"))+
  theme(legend.position ="none")+ 
  theme(plot.subtitle=element_text(size=12))
plot(jungeplot)

#Wahltagsbefragungen
wahltagsbefragungen <- wahltagsbefragungen %>% gather(kategorie, prozent, männlich:Universität)

wahltagsbefragungen <- filter(wahltagsbefragungen, wahltagsbefragungen$kandidat=="Alexander van der Bellen" | 
                                wahltagsbefragungen$kandidat=="Norbert Hofer") %>% spread(kandidat, prozent)

wahltagsbefragungen <- spread(kandidat, prozent)

wahltagsbefragungen <- as.numeric(wahltagsbefragungen$vdb)
wahltagsbefragungen <- as.numeric(wahltagsbefragungen$hofer)

library(plyr)
wahltagsbefragungen <- rename(wahltagsbefragungen, c("Alexander van der Bellen"="vdb", "Norbert Hofer"="hofer"))
wahltagsbefragungen$margin <-  wahltagsbefragungen$vdb-wahltagsbefragungen$hofer

#Berechnungen nach Kaufkraft
kaufkraftdaten <- tbl_df(kk) 
library(dplyr)

kkagg <- kaufkraftdaten %>%
  spread(kandidat, ergebnis)

  group_by(kkiproew) %>%
  summarise(sumstimmen = sum(ergebnis), sumgueltig = sum(gueltig). na.rm = TRUE) 

topland$prozent <- c((topland$Sumstimmen/topland$SumGueltig))


