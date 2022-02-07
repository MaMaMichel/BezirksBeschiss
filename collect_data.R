library(tidyverse)

# Read Flächen Daten
flaeche <- read.csv2('data/stadtgebiet.csv') %>% 
            rename(DISTRICT = Gemeindebezirk, GesamtFläche = Flächen.basierend.auf.rechtlichen.Bezirksgrenzen)

# Read Sackerl Data

sackerl <- read.csv('https://data.wien.gv.at/daten/geo?service=WFS&request=GetFeature&version=1.1.0&typeName=ogdwien:HUNDESACKERLOGD&srsName=EPSG:4326&outputFormat=csv')
sackerl <- sackerl %>% group_by(BEZIRK) %>% summarise(SackerlSpender = n()) %>%
            rbind(c(0,sum(.$SackerlSpender))) %>% 
            arrange(BEZIRK) %>% 
            mutate(DISTRICT = flaeche$DISTRICT) %>%
            select(DISTRICT, SackerlSpender) 

# Read Mietpreise Quelle: https://www.immopreise.at/Wien/Wohnung/Miete

mietpreise <- read.csv2('data/mietpreise.csv') %>% 
              select(DISTRICT, Durchschnitt) %>%
              rename(Mietpreis = Durchschnitt) %>%
              rbind(c('Wien',sum(.$Mietpreis))) %>%
              mutate(Mietpreis = as.numeric(Mietpreis))

#Read Anszahl der Hunde Pro Bezirk
hunde <- read.csv2('https://www.wien.gv.at/gogv/l9ogdspobezdog')

hunde <- hunde %>% filter(REF_YEAR == 2020) %>% 
          mutate(DISTRICT_NUM = (DISTRICT_CODE-90000)/100) %>% 
          arrange(DISTRICT_NUM) %>%
  mutate(DISTRICT = flaeche$DISTRICT, DOG_VALUE = as.numeric(gsub("\\.", "", DOG_VALUE, perl=TRUE))) %>%
          select(DISTRICT,DOG_VALUE, DISTRICT_NUM)



#Read Anszahl der Einwohner Pro Bezirk
einwohner <- read.csv2('https://www.wien.gv.at/statistik/ogd/pop-bez-pop.csv')

einwohner <- einwohner %>% filter(REF_YEAR == 2020) %>% 
  mutate(DISTRICT_NUM = (DISTRICT_CODE-90000)/100) %>% 
  arrange(DISTRICT_NUM) %>%
  mutate(DISTRICT = flaeche$DISTRICT) %>%
  select(DISTRICT,REF_YEAR,POP_VALUE,POP_DENSITY)



# Combine to one Data Set

CombinedData <- flaeche %>% left_join(sackerl, by="DISTRICT") %>% 
                            left_join(einwohner, by="DISTRICT") %>% 
                            left_join(hunde, by="DISTRICT") %>%
                            left_join(mietpreise, by="DISTRICT") %>% 
                            arrange(DISTRICT_NUM) %>%
                            mutate(DISTRICT = factor(DISTRICT,levels = DISTRICT)) %>%
                            mutate(Sackerlspender_pro_Hund = SackerlSpender/DOG_VALUE,
                                   Prozent_Grünfläche = Grünland/GesamtFläche,
                                   Hunde_pro_Ha = DOG_VALUE/GesamtFläche,
                                   Hunde_pro_Einwohner = DOG_VALUE/POP_VALUE,
                                   SCORE = (scale(Sackerlspender_pro_Hund) + scale(Prozent_Grünfläche))/2)
        


ggplot(CombinedData[-1,], aes(x = DISTRICT, y = SackerlSpender/DOG_VALUE)) +
  geom_bar(stat="identity", fill = "#0390fc") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(CombinedData[-1,], aes(x = Prozent_Grünfläche, y = Sackerlspender_pro_Hund, label = DISTRICT)) +
  theme_classic() +
  geom_point(aes(size = Hunde_pro_Ha, col = SCORE)) +
  scale_color_continuous(low="red", high="green") +
  geom_text(nudge_x = 0.01, nudge_y = 0.004)





write_csv(CombinedData, "data/CombinedData.csv")

