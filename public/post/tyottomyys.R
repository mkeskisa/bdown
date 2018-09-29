# ---- chunk-1 ----
library(tidyverse)
library(lubridate)
library(pxweb)
library(gdalUtils)
library(sf)
library(janitor)
library(gganimate)
library(rvest)
get_df <- 
    get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Explorer/Indikaattorit/alueindikaattorit.px",
                   dims = list(Alue = c('*'),
                               Tiedot = c('Työttömiä_o', 'Nuortyöt_o'),
                               Vuosi = c('*')),
                   clean = TRUE) %>% as.tibble()

ogr2ogr("WFS:http://geo.stat.fi/geoserver/tilastointialueet/wfs", "kunnat.shp", "kunta1000k_2013")
kunnat <- read_sf("kunnat.shp")

# ---- chunk-2 ----
prepped_data <- get_df %>% 
    filter(Vuosi %in% 1990:2012, !str_detect(Alue, 'SK|ELY|MK|Koko')) %>%
    spread(Tiedot, -Alue) %>% 
    janitor::clean_names() %>% 
    rename(n_tyo = nuorisotyottomyysaste_percent, k_tyo = tyottomyysaste_percent)

kuntatiedot <- 
    'https://www.tilastokeskus.fi/meta/luokitukset/kunta/001-2012/index.html' %>% #Tämän voisi kopioida suoraa triblenä, mutta scrapetaan uudelleenajettavuuden vuoksi
    rvest::html() %>% 
    rvest::html_nodes(xpath = '//*[@id="content"]/table') %>% 
    rvest::html_table(fill = T) %>% 
    as.data.frame() %>% 
    as.tibble() %>% 
    select(1:2) %>% 
    tail(-1) %>% 
    rename(kuntanumero = 1, alue = 2)

kuntanumerot <- prepped_data %>% 
    mutate(alue = alue %>% str_replace("(?s) .*", ""),
           vuosi = vuosi %>% as.character() %>% as.integer()) %>% 
    left_join(kuntatiedot)

# ---- chunk-3 ----
kuntanumerot %>% 
    filter(is.na(kuntanumero)) %>% 
    count(alue)

#Note to self: "Koski TI", "Maarianhamina - Marienhamn" ja "Pedersören kunta" ovat ainoat kunnat, joiden nimessä on välilyönti
#Maarianhamina on ainoa kunta, jonka nimi ilmoitetaan suomenkielisessä versiossa suomeksi ja ruotsiksi

fixed_kunnat <- kuntanumerot %>% 
    mutate(kuntanumero = case_when(!is.na(kuntanumero) ~ kuntanumero %>% as.integer(), 
                                   alue == 'Koski' ~ 284L,
                                   alue == 'Maarianhamina' ~ 478L,
                                   alue == 'Pedersören' ~ 599L),
           nuorisokerroin = n_tyo / (n_tyo + k_tyo)) %>% 
    mutate(n_tyo = ifelse(n_tyo > 50, 50, n_tyo))

# ---- chunk-4 ----
#Muodostetaan gif työttömyydestä vuosittain 1990-2012
gg_tyottomyys <- kunnat %>% 
    select(-vuosi) %>% 
    mutate(kuntanumero = kunta %>% as.integer()) %>% left_join(fixed_kunnat) %>% 
    ggplot() + 
    theme_void() +
    geom_sf(aes(fill = k_tyo)) + 
    scale_fill_gradient2(low = "white", high = "#5b0000") +
    labs(title = 'Työttömyys (%) vuonna: {frame_time}', fill = '') +
    transition_time(vuosi)

gg_tyottomyys
# ---- chunk-5 ----
gg_tyottomyys + geom_sf(aes(fill = n_tyo)) + labs(title = 'Nuorisotyöttömyys (%) vuonna: {frame_time}', fill = '')
# ---- chunk-6 ----
kunnat_maakunnat <- 'https://www.stat.fi/meta/luokitukset/kunta/001-2013/luokitusavain_maakunta.html' %>% 
    rvest::html() %>% 
    rvest::html_nodes(xpath = '//*[@id="content"]/table') %>% 
    rvest::html_table(fill = T) %>% 
    as.data.frame() %>% 
    as.tibble() %>% 
    tail(-1) %>% 
    rename(kuntanumero = 1, kunta = 2, maakuntanumero = 3, maakunta = 4)


rajat <- c(0, 50)


fixed_kunnat %>% 
    mutate(kuntanumero = kuntanumero %>% as.character()) %>% 
    left_join(kunnat_maakunnat) %>% #https://www.stat.fi/meta/luokitukset/kunta/001-2013/luokitusavain_maakunta.html
    ggplot() +
    geom_line(data = 1:50 %>% as.tibble(), aes(x = value, y = value), color = 'grey70') +
    theme_minimal() +
    geom_point(aes(n_tyo, k_tyo)) +
    scale_y_continuous(limits = rajat) +
    scale_x_continuous(limits = rajat) +
    facet_wrap(~maakunta, nrow = 5)  +
    labs(title = 'Nuorisotyöttömyys ja työttömyys vuonna: {frame_time}', fill = '', y = 'Työttömyysaste (%)', x = 'Nuorisotyöttömyysaste (%)') +
    shadow_wake(0.1, alpha = 0.25)  
transition_time(vuosi)
# ---- chunk-7 ----