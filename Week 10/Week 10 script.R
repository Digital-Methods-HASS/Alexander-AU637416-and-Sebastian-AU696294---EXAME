

library(tidyverse)

# create a "kings" object in R

read.csv("data/Kings 3.csv")

read_csv("data/Kings 3.csv")

read.csv2("data/Kings 3.csv")

read_csv2("data/Kings 3.csv")


# Fill in the code below and review the outputs

kings1 <- read.csv("data/Kings 3.csv")

glimpse(kings1)

kings2 <- read_csv("data/Kings 3.csv", na = "NA")


glimpse(kings2)

head(kings2)

kings3 <- read.csv2("data/Kings 3.csv")

glimpse(kings3)

kings4 <- read_csv2("data/Kings 3.csv") 

glimpse(kings4)

# Funktion Kings 2 og Kings4 er "tidyverse"-funktion
# Funktion Kings1 og Kings2 er semikolonseperet fil og Kings3 og Kings4 er kommaseperet.
# jeg skal derfor bruge Kings2


class(kings2)


#2
# class viser at det "tbl" og ikke "dataframe", hvilket er godt ved uploading af et spreadsheet

#3
#kings3 og kings4 har 1 kolonne, mens kings1 og kings2 har 7 kolonner

#4 glimpse funktionen kan bruges her

dkmon <- read_csv("data/Kings 3.csv", na="NA")

glimpse(dkmon)

class(dkmon)

head(dkmon)

ncol(dkmon)


#Calculate the duration of regin for alle the kings in your table

dkmonduration <- dkmon %>% 
  mutate(duration=Regerings_Slut - Regerings_Start_, na.rm = TRUE) %>% 
  mutate(Regerings_Midt_ = Regerings_Start_ + duration/2)

glimpse(dkmonduration)

mean(dkmonduration$duration, na.rm = TRUE)

avg_duration <- mean(dkmonduration$duration, na.rm = TRUE)

#vi udregner gennemsnittet af regeringstiden for kongerne til at være ca. 20 år

long_regin_king <- dkmonduration %>% 
  filter(duration>avg_duration)

count(long_regin_king)
# der er 27 regenter der har regeret længere end gennemsnittet

print(long_regin_king) %>% 
  arrange(desc(long_regin_king))

longest_ruling_kings <- dkmonduration %>% 
  arrange(desc(duration)) %>% 
  slice_max(order_by = duration, n = 3)

longest_ruling_kings <- longest_ruling_kings %>% 
  mutate(Days=duration*365)




dkmonduration %>% 
  ggplot(aes(x=Regerings_Midt_, y=duration))+
  geom_point()+
  geom_smooth()+
  theme_classic()


