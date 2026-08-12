getwd()

setwd("C:/Users/nclac/github pimp/Marketingkutatás és piacelemzés-20260723T092838Z-1-001/Marketingkutatás és piacelemzés")
library(haven)
library(foreign)
library(labelled)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)
library(fs)
library(stringr)
df_valaszok_szamkoddal<-haven::read_sav("tampon_data_final.sav")
#df<-df %>% filter(df$TamponHasználó !=1)
df_valaszok_alap<-glimpse(df_valaszok_szamkoddal %>% unlabelled())


########################################################################################################################################################################

######################################## Univariate and likert plots

############################################################################################################################################################################


#functions:
plot_es_ment_demografia<- function(data, valtozo, tengely_cimke = NULL, egyedi_title = NULL, tengely_betumeret = 12, mappa_nev = "demografiai_abrak", abratipus = "png") {
  
  # 1. Változónév kinyerése szövegként
  valtozo_szoveg <- deparse(substitute(valtozo))
  
  # 2. Főcím (Title) meghatározása
  if (is.null(egyedi_title)) {
    fo_cim <- paste0("A válaszadók megoszlás a(z) ", valtozo_szoveg, " változó szerint")
  } else {
    fo_cim <- egyedi_title
  }
  
  # 3. X-tengely címkéjének kinyerése
  if (is.null(tengely_cimke)) {
    spss_label <- attr(data[[valtozo_szoveg]], "label")
    if (!is.null(spss_label)) {
      x_felirat <- as.character(spss_label) 
    } else {
      x_felirat <- valtozo_szoveg
    }
  } else {
    x_felirat <- tengely_cimke
  }
  
  # 4. Adat előkészítése
  statisztika <- data %>%
    filter(!is.na({{ valtozo }})) %>%
    mutate(Kategoria = as_factor({{ valtozo }})) %>%
    group_by(Kategoria) %>%
    summarize(
      darabszam = n(),
      szazalek = (n() / nrow(data)) * 100,
      .groups = "drop"
    )
  
  # 5. Grafikon elkészítése
  p <- ggplot(statisztika, aes(x = Kategoria, y = darabszam)) +
    geom_col(fill = "steelblue", width = 0.7) +
    geom_text(
      aes(label = paste0(darabszam, " fő\n(", round(szazalek, 1), "%)")), 
      vjust = -0.3, 
      size = 3.5, 
      lineheight = 0.8
    ) +
    scale_y_continuous(limits = c(0, max(statisztika$darabszam) * 1.15)) +
    labs(
      title = fo_cim, 
      subtitle = "Esetszámok és százalékos arányok (a minta egészén)",
      x = x_felirat, 
      y = "Esetszám (fő)"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      # Itt a megadott tengely_betumeret paraméter határozza meg a betűméretet:
      axis.title.x = element_text(face = "plain", size = tengely_betumeret, margin = margin(t = 15)),
      panel.grid.major.x = element_blank()
    )
  
  # 6. Mappa ellenőrzése és létrehozása
  if (!dir.exists(mappa_nev)) {
    dir.create(mappa_nev, recursive = TRUE)
  }
  
  # 7. Fájlnév összeállítása és mentés
  fajl_utvonal <- file.path(mappa_nev, paste0("plot_", valtozo_szoveg, ".", abratipus))
  
  ggsave(
    filename = fajl_utvonal,
    plot = p,
    width = 8, 
    height = 6, 
    dpi = 300
  )
  
  message(paste0("Az ábra sikeresen elmentve ide: ", fajl_utvonal))
  
  return(p)
}

plot_es_ment_multiple_choice <- function(data, oszlop_vektor, egyedi_title = "Többválaszos kérdés megoszlása", 
                                         tengely_cimke = "Említések aránya a válaszadók körében (%)",
                                         tengely_betumeret = 11, kategoria_betumeret = 10,
                                         mappa_nev = "demografiai_abrak", fajl_nev = "plot_multiple_choice.png") {
  
  # 1. Teljes válaszadói létszám meghatározása (bázis a százalékhoz)
  teljes_n <- nrow(data)
  
  # 2. Adatok átalakítása és az említések kiszámítása
  statisztika <- data %>%
    # Csak a kérdéshez tartozó válasz-oszlopokat tartjuk meg
    select(all_of(oszlop_vektor)) %>%
    # Hosszú formátumra hozzuk
    pivot_longer(cols = everything(), names_to = "Opció_Kód", values_to = "Érték") %>%
    # Akkor van említés, ha NEM NA (mivel unlabelled() után vagyunk, a tényleges szöveg van ott)
    filter(!is.na(Érték)) %>%
    # Csoportosítunk a bejelölt válasz szerint
    group_by(Érték) %>%
    summarize(
      darabszam = n(),
      # A százalékot a teljes mintaszámhoz (213 fő) viszonyítjuk!
      szazalek = (n() / teljes_n) * 100,
      .groups = "drop"
    ) %>%
    # Sorbarendezzük a legnépszerűbbtől a legkevésbé népszerűig
    arrange(szazalek)
  
  # A faktor szintek beállítása a helyes vizuális rangsorhoz (fentről lefelé csökkenő)
  statisztika$Érték <- factor(statisztika$Érték, levels = statisztika$Érték)
  
  # 3. Grafikon elkészítése (vízszintes elrendezés coord_flip nélkül, y = Érték-kel)
  p <- ggplot(statisztika, aes(x = szazalek, y = Érték)) +
    geom_col(fill = "darkred", width = 0.6) + # Más szín, hogy elkülönüljön a single choice-tól
    # Szöveges feliratok az oszlopok végére / mellé
    geom_text(
      aes(label = paste0(darabszam, " fő (", round(szazalek, 1), "%)")), 
      hjust = -0.1, 
      size = 3.5
    ) +
    # Tengelyek skálázása, hogy a feliratok ne lógjanak ki a jobb oldalon
    scale_x_continuous(limits = c(0, max(statisztika$szazalek) * 1.25)) +
    labs(
      title = egyedi_title,
      subtitle = paste0("Említésszámok és arányok a teljes mintában (N = ", teljes_n, " fő)\nA százalékok összege meghaladhatja a 100%-ot."),
      x = tengely_cimke,
      y = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      # Itt szabályozhatóak a betűméretek a paraméterekből
      axis.text.y = element_text(face = "bold", size = kategoria_betumeret),
      axis.title.x = element_text(size = tengely_betumeret, margin = margin(t = 15)),
      panel.grid.major.y = element_blank()
    )
  
  # 4. Mappa ellenőrzése és mentés
  if (!dir.exists(mappa_nev)) {
    dir.create(mappa_nev, recursive = TRUE)
  }
  
  fajl_utvonal <- file.path(mappa_nev, fajl_nev)
  ggsave(filename = fajl_utvonal, plot = p, width = 10, height = 6, dpi = 300)
  
  message(paste0("A többválaszos ábra sikeresen elmentve ide: ", fajl_utvonal))
  
  return(p)
}

kerdes_cimkek <- sapply(likert_valtozok, function(v) {
  lbl <- attr(df_valaszok_alap[[v]], "label")
  if (!is.null(lbl)) {
    # Kiszedi a zárójelben lévő részt, pl: "Termék ára"
    rovid_nev <- str_extract(lbl, "(?<=\\()[^\\)]+(?=\\))")
    if (is.na(rovid_nev)) return(as.character(lbl)) else return(rovid_nev)
  } else {
    return(v)
  }
})



########################################### Plots for the whole sample (target population) ##############################

#demography and habits that might relate to the product
korosztály= df_valaszok_alap$D2 #age
plot_es_ment_demografia(df_valaszok_alap,korosztály, tengely_cimke = "korosztály kategóriák")
lakhely=df_valaszok_alap$D3 #place of residence
plot_es_ment_demografia(df_valaszok_alap,lakhely, "lakhely kategóriák")
sport=df_valaszok_alap$K1 #sport
plot_es_ment_demografia(df_valaszok_alap,sport,tengely_cimke = "Milyen gyakran sportolsz?")
buli=df_valaszok_alap$K2 #party 
plot_es_ment_demografia(df_valaszok_alap,buli,tengely_cimke = "Milyen gyakran jársz el szórakozni?")
preferencia=df_valaszok_alap$K3 #prefered product (female intimate hygiene)
plot_es_ment_demografia(df_valaszok_alap,preferencia,"A menstruáció során elsősorban milyen terméket használsz?")


#main factor taking into account when purchasing the product of scope (factors that influence the purchase of feminine hygiene products)
likert_valtozok <- c("L1_1", "L1_2", "L1_3", "L1_4", "L1_5", "L1_6", "L1_7", "L1_8")

# variable Labels 
kerdes_cimkek <- sapply(likert_valtozok, function(v) {
  lbl <- attr(df_valaszok_alap[[v]], "label")
  if (!is.null(lbl)) {
    # Kiszedi a zárójelben lévő részt, pl: "Termék ára"
    rovid_nev <- str_extract(lbl, "(?<=\\()[^\\)]+(?=\\))")
    if (is.na(rovid_nev)) return(as.character(lbl)) else return(rovid_nev)
  } else {
    return(v)
  }
})

# tidy data
likert_adat <- df_valaszok_alap %>%
  select(all_of(likert_valtozok)) %>%
  mutate(across(everything(), as_factor)) %>% 
  pivot_longer(cols = everything(), names_to = "Valtozo", values_to = "Valasz") %>%
  filter(!is.na(Valasz))

#ranking
likert_statisztika <- likert_adat %>%
  group_by(Valtozo, Valasz) %>%
  summarize(darabszam = n(), .groups = "drop_last") %>%
  mutate(
    szazalek = (darabszam / sum(darabszam)) * 100,
    Kerdes_Szoveg = kerdes_cimkek[Valtozo]
  ) %>%
  ungroup()

sorrend_matrix <- likert_statisztika %>%
  filter(Valasz %in% c("Kicsit fontos", "Nagyon fontos")) %>%
  group_by(Kerdes_Szoveg) %>%
  summarize(pozitiv_arany = sum(szazalek)) %>%
  arrange(pozitiv_arany) # Az arrange() növekvő sorrendbe rakja, így a ggplot fentről lefelé csökkenőként ábrázolja

likert_statisztika$Kerdes_Szoveg <- factor(likert_statisztika$Kerdes_Szoveg, levels = sorrend_matrix$Kerdes_Szoveg)

skala_sorrend <- c("Egyáltalán nem fontos", "Nem annyira fontos", "Semleges", "Kicsit fontos", "Nagyon fontos")
likert_statisztika$Valasz <- factor(likert_statisztika$Valasz, levels = skala_sorrend)

# Likert plot
p_likert <- ggplot(likert_statisztika, aes(x = szazalek, y = Kerdes_Szoveg, fill = Valasz)) +
  geom_col(position = "fill", width = 0.7) + 
  geom_text(
    aes(label = paste0(darabszam, " fő\n(", round(szazalek, 1), "%)")),
    position = position_fill(vjust = 0.5), 
    size = 2.8, # Kicsit kisebb betűméret, hogy a 8 sorban is biztosan elférjen
    lineheight = 0.8,
    color = "black"
  ) +
  scale_x_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  labs(
    title = "Tényezők fontossága női intim higiéniai termékek vásárlása során",
    subtitle = "Pozitív válaszok aránya szerint csökkenő sorrendbe rendezve",
    x = "Százalékos megoszlás",
    y = NULL,
    fill = "Értékelés"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )
# Map
mappa_nev <- "Likert"
if (!dir.exists(mappa_nev)) {
  dir.create(mappa_nev, recursive = TRUE)
}
fajl_utvonal <- file.path(mappa_nev, "plot_likert_rendezett_8db.png")
ggsave(filename = fajl_utvonal, plot = p_likert, width = 12, height = 8, dpi = 300)

print(p_likert)


################################################Consumer journay  ##############################

##tampon users only

#tampon usage intensity
df_valaszok_alap <- df_valaszok_alap %>%
  mutate(K5 = if_else(
    K5 == "csak meghatározott alkalmakkor (pl. sportolás, strandolás, buli) használok tampont",
    "csak meghatározott alkalmakkor használok tampont",
    K5
  ))

plot_es_ment_demografia(df_valaszok_alap,K5,  egyedi_title = "A válszadók megoszlása a(z) intenzitás változó szerint","A menstruáció során mikor használsz tampont?")

#innovation_seeking
plot_es_ment_demografia(df_valaszok_alap,K9,  egyedi_title = "A válszadók megoszlása a(z) innovációra való nyitottság változó szerint","Használtál már applikátoros tampont?")

##multiple choice

#reasons for using tampon contrast to other products
motiváció_oszlopok <- c("V1_1", "V1_2", "V1_3", "V1_4", "V1_5", "V1_6", "V1_7")
plot_es_ment_multiple_choice(
  data = df_valaszok_alap, 
  oszlop_vektor = motiváció_oszlopok,
  egyedi_title = "A tamponvásárlás főbb indokai",
  tengely_cimke = "Említések aránya a válaszadók körében (%)",
  tengely_betumeret = 11,     # X-tengely (alsó felirat) mérete
  kategoria_betumeret = 10,   # Y-tengely (bejelölt válaszok) mérete
  fajl_nev = "plot_motiváció_multiple.png" # Egyedi fájlnév a mentéshez
)

#information seeking before purchase (info channels)
honnan_tajekozodik_oszlopok  <- c("V2_1", "V2_2", "V2_3", "V2_4", "V2_5")
plot_es_ment_multiple_choice(
  data = df_valaszok_alap, 
  oszlop_vektor = honnan_tajekozodik_oszlopok ,
  egyedi_title = "Információszerzési csatornák intim higiéniai termékekről",
  tengely_cimke = "Említések aránya a válaszadók körében (%)",
  tengely_betumeret = 11,     # X-tengely (alsó felirat) mérete
  kategoria_betumeret = 10,   # Y-tengely (bejelölt válaszok) mérete
  fajl_nev = "plot_tájékozódás_multiple.png" # Egyedi fájlnév a mentéshez
)

#reasons for  trying out innovations
innováció_mozgrugó_oszlopok <- c("V6_1", "V6_2", "V6_3", "V6_4", "V6_5", "V6_6", "V6_7")

df_valaszok_alap <- df_valaszok_alap %>%
  mutate(V6_2 = if_else(
    V6_2 == "Fenntarthatóbb anyaghasználat (pl. újrahasznosítható/újrahasznosított applikátor vagy kevesebb csomagolóanyag)",
    "Fenntarthatóbb anyaghasználat",
    V6_2
  ))

df_valaszok_alap <- df_valaszok_alap %>%
  mutate(V6_3 = if_else(
    V6_3 == "Esztétikusabb/vonzóbb/diszkrétebb csomagolás és/vagy design",
    "Esztétikusabb/vonzóbb/diszkrétebb csomagolás",
    V6_3
  ))

plot_es_ment_multiple_choice(
  data = df_valaszok_alap, 
  oszlop_vektor = innováció_mozgrugó_oszlopok,
  egyedi_title = "Az innovatív termék kipróbálásának főbb mozgatórugói",
  tengely_cimke = "Említések aránya a válaszadók körében (%)",
  tengely_betumeret = 11,     # X-tengely (alsó felirat) mérete
  kategoria_betumeret = 10,   # Y-tengely (bejelölt válaszok) mérete
  fajl_nev = "plot_innováció_mozgrugó_oszlopok_multiple.png" # Egyedi fájlnév a mentéshez
)

#parralels usage of other products
parralel_usage_oszlopok  <- c("V8_1", "V8_2", "V8_3", "V8_4", "V8_5", "V8_6")
plot_es_ment_multiple_choice(
  data = df_valaszok_alap, 
  oszlop_vektor = parralel_usage_oszlopok ,
  egyedi_title = "Milyen más terméket használsz egyidejűleg a tampon mellett?",
  tengely_cimke = "Említések aránya a válaszadók körében (%)",
  tengely_betumeret = 11,     # X-tengely (alsó felirat) mérete
  kategoria_betumeret = 10,   # Y-tengely (bejelölt válaszok) mérete
  fajl_nev = "plot_parralel usage_multiple.png" # Egyedi fájlnév a mentéshez
)



