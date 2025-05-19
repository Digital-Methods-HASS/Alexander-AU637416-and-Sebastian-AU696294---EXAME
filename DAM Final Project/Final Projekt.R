
# graf over fertilitet fordelt på aldersgrupper

library(ggplot2)
library(readr)
library(tidyverse)


# indlæser data fra 1945 til 1972 og 1973 til 2024
# filer virkede ikke i starten, så jeg brugte read_delim...

fert_alder1 <- read_csv2("data/Fertilitet_alder 1945-1972.csv")

fert_alder2 <- read_csv2("data/Fertilitet_alder 1973-2024.csv")

fert_alder1 <- read_delim("data/Fertilitet_alder 1945-1972.csv", delim = ";", locale = locale(encoding = "Latin1"))

fert_alder2 <- read_delim("data/Fertilitet_alder 1973-2024.csv", delim = ";", locale = locale(encoding = "Latin1"))

glimpse(fert_alder1)

glimpse(fert_alder2)


#lang format for fert_alder1

fert_alder1_long <- fert_alder1 |>
  pivot_longer(-ÅR, names_to = "år", values_to = "fertilitet") |>
  mutate(år = as.integer(år))

# samme aldersgrupper for fert_alder2

fert_alder2_long <- fert_alder2 |>
  rename(alder = 1) |>
  pivot_longer(-alder, names_to = "år", values_to = "fertilitet") |>
  mutate(
    år = as.integer(år),
    alder_num = as.integer(str_extract(alder, "\\d+")),
    aldersgruppe = case_when(
      alder_num >= 20 & alder_num <= 24 ~ "20-24 år",
      alder_num >= 25 & alder_num <= 29 ~ "25-29 år",
      alder_num >= 30 & alder_num <= 34 ~ "30-34 år",
      alder_num >= 35 & alder_num <= 39 ~ "35-39 år",
      alder_num >= 40 & alder_num <= 44 ~ "40-44 år",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(aldersgruppe)) |>
  group_by(år, aldersgruppe) |>
  summarise(fertilitet = mean(fertilitet, na.rm = TRUE), .groups = "drop") |>
  rename(ÅR = aldersgruppe)

#sammenfletning af de to datasæt

fert_samlet <- bind_rows(fert_alder1_long, fert_alder2_long)

ggplot(fert_samlet, aes(x = år, y = fertilitet, color = ÅR)) +
  geom_line(size = 1) +
  labs(
    title = "Fertilitetsudvikling fordelt på aldersgrupper",
    x = "År", y = "Fødsler pr. 1000 kvinder",
    color = "Aldersgruppe"
  ) +
  theme_minimal()


ggplot(fert_samlet, aes(x = år, y = fertilitet)) +
  geom_line(color = "steelblue", size = 1) +
  facet_wrap(~ÅR, ncol = 2) +
  labs(
    title = "Fertilitetsudvikling pr. aldersgruppe",
    x = "År", y = "Fødsler pr. 1000 kvinder"
  ) +
  theme_minimal()

fert_andel <- fert_samlet |>
  group_by(år) |>
  mutate(andel = fertilitet / sum(fertilitet)) |>
  ungroup()




# Graf over fertilitetskovetient

# ggplot2 aktiveres
library(ggplot2)


# Indlæs data (spring tomme rækker og overskrifter over)
Fertilitet <- read.csv("data/Fertilitetskvo 5.csv", sep = ";", skip = 2, header = FALSE)

# Navngiv kolonnerne
colnames(Fertilitet)[1:2] <- c("År", "Fertilitetskvotient")

# Konverter til tal
Fertilitet$År <- as.numeric(as.character(Fertilitet$År))
Fertilitet$Fertilitetskvotient <- as.numeric(as.character(Fertilitet$Fertilitetskvotient))


# Plot med ggplot2
ggplot(Fertilitet, aes(x = År, y = Fertilitetskvotient)) +
  geom_line(color = "darkred", size = 1) +
  labs(
    title = "Fertilitetsudvikling i Danmark",
    x = "År",
    y = "Fertilitetskvotient (pr. 1000 kvinder)"
  ) +
  theme_minimal()




# søjlediagram over erhversdeltagelse for kvinder


# Load nødvendige pakker
library(ggplot2)
library(dplyr)

# Importer data (juster stien hvis nødvendigt)
erhverv_2 <- read.csv("data/Erhversfrekvens2.csv", sep = ";", dec = ",", fileEncoding = "latin1")

# Omdøb den første kolonne til "År"
colnames(erhverv_2)[1] <- "År"

# Lav søjlediagram
ggplot(erhverv_2, aes(x = factor(År), y = Erhversfrekvens)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.7) +  
  labs(
    title = "Erhvervsdeltagelse for kvinder i Danmark (1950-2023)",
    x = "År",
    y = "Erhvervsfrekvens (%)",
    caption = "Kilde: DST eller din kilde"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Roter årstal for læsbarhed




# graf over lang videregående uddannelser for kvinder

#load nødvendige pakker
library(tidyverse)

# Læs CSV-filen med korrekt encoding
uddannelse <- read.csv("data/Lang videregående uddannelse 2.csv", sep = ";", header = FALSE, fileEncoding = "latin1")

# Tilføj årstal fra 1986 til 2024 som kolonnenavne (undtagen første kolonne)
år <- 1986:(1986 + ncol(uddannelse) - 2)
colnames(uddannelse) <- c("Alder", år)

# Gør data lange
udd_long <- pivot_longer(uddannelse, -Alder, names_to = "År", values_to = "Procent")

# Konverter kolonner
udd_long$År <- as.numeric(udd_long$År)
udd_long$Procent <- as.numeric(gsub(",", ".", udd_long$Procent))  # Erstat komma med punktum hvis nødvendigt

# Plot
ggplot(udd_long, aes(x = År, y = Procent, color = Alder)) +
  geom_line(size = 1.2) +
  labs(title = "Kvinder med Lang Videregående Uddannelse efter Alder",
       x = "År",
       y = "Procent",
       color = "Alder") +
  theme_minimal()

