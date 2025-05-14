library(pdftools)
library(tidytext)
library(textdata)
library(ggwordcloud)
library(ggplot2)
library(dplyr)

GOT <- here("data","got.pdf")
GOT_Text <- pdf_text(GOT)
GOT_Text
GOTP90 <- GOT_Text[90]
GOTP90

GOT_DF <- data.frame(GOT_Text) %>%
  mutate(text_full = str_split(GOT_Text, pattern = '\n')) %>% #splitting the text
  unnest(text_full) %>%
  mutate(text_full = str_trim(text_full))

GOT_tokens <- GOT_DF %>%
  unnest_tokens(word, text_full)
head(GOT_tokens)


GOT_stop <- GOT_tokens %>%
  anti_join(stop_words) %>% #Antijoin discards the shared words
  select(-GOT_Text)

GOT_WC <- GOT_stop %>%
  count(word) %>%
  arrange(-n)
GOT_WC

GOT_no_numeric <- GOT_stop %>%
  filter(is.na(as.numeric(word)))

length(unique(GOT_no_numeric$word))


GOT_top100 <- GOT_no_numeric %>%
  count(word) %>%
  arrange(-n) %>%
  head(100)
GOT_top100

GOT_cloud <- ggplot(data = GOT_top100, aes(label = word)) +
  geom_text_wordcloud() +
  theme_minimal()+
  ggtitle("The 100 most common words")
GOT_cloud

ggplot(data = GOT_top100, aes(label = word, size = n)) +
  geom_text_wordcloud_area(aes(color = n), shape = "square") +
  scale_size_area(max_size = 12) +
  scale_color_gradientn(colors = c("darkgreen","blue","red")) +
  theme_minimal()+
  ggtitle("The 100 most common words")

get_sentiments(lexicon = "afinn")
afinn_neg <- get_sentiments("afinn") %>%
  filter(value %in% c(-3,-4,-5))

afinn_neg

GOT_afinn <- GOT_stop %>%
  inner_join(get_sentiments("afinn"))

GOT_afinn_hist <- GOT_afinn %>%
  count(value)

ggplot(data = GOT_afinn_hist, aes(x = value, y = n)) +
  geom_col(aes(fill = value)) +
  theme_bw()+
  labs(x = "Words", y = "Sentiment")+
  ggtitle("The number of words associated with a sentiment")



GOT_afinn2 <- GOT_afinn %>%
  filter(value == -2)

GOT_afinn2 %>%
  distinct(word)

unique(GOT_afinn2$word)

GOT_afinn2_n <- GOT_afinn2 %>%
  count(word, sort = TRUE) %>%
  head(10) %>%
  mutate(word = fct_reorder(factor(word), n))

GOT_afinn2_n %>%
  arrange(desc(n)) %>%
  slice_head(n = 50) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  coord_flip() +
  theme_bw()+
  labs(x = "Words with a -2 sentiment", y ="Times mentioned")


GOT_summary <- GOT_afinn %>%
  summarize(
    mean_score = mean(value),
    median_score = median(value)
  )
GOT_summary
#Negative mean score

GOT_nrc <- GOT_stop %>%
  inner_join(get_sentiments("nrc"))


GOT_exclude_n <- GOT_exclude %>%
  count(word, sort = TRUE)

head(GOT_exclude_n)

GOT_nrc_n <- GOT_nrc %>%
  count(sentiment, sort = TRUE)

ggplot(data = GOT_nrc_n, aes(x = sentiment, y = n)) +
  geom_col(aes(fill = sentiment))+
  theme_bw()+
  labs(x = "Words by sentiment")


GOT_nrc_n5 <- GOT_nrc %>%
  count(word,sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup()


GOT_nrc_gg <- ggplot(data = GOT_nrc_n5, aes(x = reorder(word,n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, ncol = 2, scales = "free") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Words associated with each sentiment")+
  labs(x = "", y ="")

GOT_nrc_gg

