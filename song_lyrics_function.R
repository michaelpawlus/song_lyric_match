# The function takes four arguments. 
# A song title and artist as well as another song title and artist.
# It then imports lyrics and identfies 3,2 and 1 n-gram matches.
# It outputs a single data frame with all matches.

library(geniusR)
library(tidyverse)
library(tidytext)

lyric_match <- function(artist1, title1, artist2, title2) {

song1 <- genius_lyrics(artist = artist1, song = title1) 

song2 <- genius_lyrics(artist = artist2, song = title2)

song1_tri <- song1 %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  count(word1, word2, word3, sort = TRUE)

song2_tri <- song2 %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  count(word1, word2, word3, sort = TRUE)

common_tris <- song1_tri %>%
  inner_join(song2_tri, by = c("word1","word2","word3")) %>%
  count(word1, word2, word3, sort = TRUE)

common_tris <- common_tris %>%
  unite(word, word1, word2, word3)

song1_bi <- song1 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)

song2_bi <- song2 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)

common_bis <- song1_bi %>%
  inner_join(song2_bi, by = c("word1","word2")) %>%
  count(word1, word2, sort = TRUE)

common_bis <- common_bis %>%
  unite(word, word1, word2)

song1_words <- song1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

song2_words <- song2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

common_words <- song1_words %>%
  inner_join(song2_words, by = "word") %>%
  count(word, sort = TRUE)

all_matches <- bind_rows(common_tris, common_bis, common_words)

print(all_matches)

}
