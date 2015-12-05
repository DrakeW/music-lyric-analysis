# Convert json file to data frame in R

#################### Packages ##################

require(RJSONIO)    
install.packages("RJSONIO")
install.packages("plyr")
install.packages("hash")
install.packages("ggplot2")

library("hash")
library("ggplot2")


############### CODE ###################

songs <- list()

for (i in 1965:2015) {
  # Read each year's data in R
  name <- paste0("song_", i)
  file_path <- paste0("~/Desktop/", name, ".json")
  temp <- fromJSON(file_path)
  
  # Convert json format to dataframe with 4 columns, num_of_weeks, lyric, name and artist
  temp <- lapply(temp, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
})

temp <- do.call("rbind", temp)
temp <- as.data.frame(temp, stringsAsFactors = FALSE)
temp$year <- rep(i, nrow(temp))

# Add each year's data in main list with song_year as element name
songs[[name]] <- temp
}

# Combine songs from all years
all_songs <- rbind.fill(songs)
write.csv(all_songs, file = "~/Desktop/stats133_final_project/songs_all.csv")

# Read in clean data
songs_df <- read.csv("~/Desktop/stats133_final_project/songs_all.csv")
songs_df <- songs_df[, -1]
# Split lyrics into words
split_lyrics <- character(0)
for (i in 1:nrow(songs_df)) {
  split_lyrics <- c(split_lyrics, tolower(unlist(strsplit(as.character(songs_df$lyric[i]), " "))))
}

# Find the frequency table for words
freq_table <- table(split_lyrics)

# Find repetitive words
repetition <- character(0)
past <- ""
for (i in split_lyrics) {
  if (!is.na(i) & !is.na(past) & i == past) {
    repetition <- c(repetition, i)
  }
  past <- i
}
table()

# score each song based on following criteria
#   1. for each distinct words +2
#   2. for distance between repetitive words, if = 0, -3
#                                             if in [1,3], -2
#                                             if in [4,5], -1
#   3. for avg number of syllables
#   4. for avg number of characters

# GOAL: rank each song, and find percentage of songs of each year to see if songs get smarter or dumber

# add score to df


################## process lyrics ######################################
songs_df$lyric <- as.character(songs_df$lyric)

# Filter out songs that we can't find corresponding lyrics
songs_df <- subset(songs_df, lyric != "")
songs_df <- subset(songs_df, lyric != "Not Found")

# First, we find the length of songs.

# Because when we read data in, all single quotes are removed to avoid r recognizing it as the end of the string.
# Therefore, we have words such as "m" "re" "s" "ve" "t" "ll" in the split_lyrics that are used to be part of I'm, you're, 
# something's, I've, can't, and I'll. Those are meaningless words in our analysis so we remove those words from our data.

abbre <- c("^m$", "^re$", "^s$", "^ve$", "^t$", "^ll$")
full <- c("am", "are", "is", "have", "not", "will")

for (i in 1:nrow(songs_df)) {
  split_lyrics <-  c(tolower(unlist(strsplit(songs_df$lyric[i], " "))))
  for (j in seq_along(abbre)) {
    split_lyrics <- gsub(abbre[j], full[j], split_lyrics)
  }
  songs_df$processed_lyric[i] <-  list(split_lyrics)
}


###################### 1 ########################

songs_num_of_uniq_words <- NULL
songs_length <- NULL

for (i in 1:nrow(songs_df)) {
  song_lyric <- unlist(songs_df$processed_lyric[i])
  songs_length <- c(songs_length, length(song_lyric))
  songs_num_of_uniq_words <- c(songs_num_of_uniq_words, length(unique(song_lyric)))
}

# score of each song based on distinct words
songs_distinct_words <- songs_num_of_uniq_words / songs_length

songs_df$lyric_length <- songs_length
songs_df$num_of_distinct_words <- songs_num_of_uniq_words

###################### 2 ##########################
songs_score_repetition_distance <- NULL

lost_score_calculator <- function(distance) {
  if (distance > 7) {
    0
  }else if (distance > 4) {
    1
  }else if (distance > 0) {
    2
  }else if (distance == 0) {
    3
  }
}

for (i in 1:nrow(songs_df)) {
  song_lyric_record <- hash()
  song_lyric <- songs_df$processed_lyric[i]
  for (index in 1:length(unlist(song_lyric))) {
    song_lyric <- unlist(song_lyric)
    word <- song_lyric[index]
    if (has.key(word, song_lyric_record)) {
      # if has record already, then calculate score
      prev_index <- song_lyric_record[[paste0("", word)]][1]
      diff <- index - prev_index
      score <- lost_score_calculator(diff)
      .set(song_lyric_record, keys=word, values=c(index, score))
    }else {
      # else add that word to record, and its value is (index, score_for_this_word)
      .set(song_lyric_record, keys=word, values=c(index, 0))
    }
  }
  song_repetition_score <- 0
  for (i in ls(song_lyric_record)) {
    song_repetition_score <- song_repetition_score + song_lyric_record[[paste0("",i)]][2]
  }
  songs_score_repetition_distance <- c(songs_score_repetition_distance, song_repetition_score)
}

songs_score_repetition_distance <- (-1)*songs_score_repetition_distance

songs_df$repetition_distance_score <- songs_score_repetition_distance

####################### 3 ######################

songs_syllables <- NULL

is_vowel <- function(char) {
  char %in% c("a", "e", "i", "o", "u")
}

get_num_of_syllables_of_word <- function(word) {
  total <- nchar(gsub("[^X]", "", gsub("[aeiouy]+", "X", tolower(word))))
  bad_vowel_num <- 0
  word_chars <- unlist(strsplit(word, ""))
  if (is_vowel(word_chars[length(word_chars)])) {
    bad_vowel_num <- bad_vowel_num + 1
  }
  total <- total - bad_vowel_num
  total
}

for (i in 1:nrow(songs_df)) {
  song_lyric <- songs_df$processed_lyric[i]
  num_of_syllables <- NULL
  for (index in 1:length(unlist(song_lyric))) {
    song_lyric <- unlist(song_lyric)
    word <- song_lyric[index]
    num_of_syllables <- c(num_of_syllables, get_num_of_syllables_of_word(word))
  }
  songs_syllables <- c(songs_syllables, mean(num_of_syllables))
}

songs_df$avg_num_of_syllables <- songs_syllables


####################### 4 #######################

songs_characters <- NULL

for (i in 1:nrow(songs_df)) {
  song_lyric <- songs_df$processed_lyric[i]
  num_of_char <- NULL
  for (index in 1:length(unlist(song_lyric))) {
    song_lyric <- unlist(song_lyric)
    word <- song_lyric[index]
    num_of_char <- c(num_of_char, nchar(word))
  }
  songs_characters <- c(songs_characters, mean(num_of_char))
}

songs_df$avg_num_of_characters <- songs_characters

################ score calculation ####################
# formula: percentile * weight

get_percentile_of_num <- function(num, data_set) {
  length(data_set[data_set <= num])/length(data_set)*100
}

DISTINCT_WORDS_WEIGHT <- 0.25
REPETITION_DISTANCE_WEIGHT <- 0.25
SYLLABLES_WEIGHT <- 0.25
NUM_CHARACTERS_WEIGHT <- 0.25

score_distinct_words <- NULL
score_repetition_distance_weighted <- NULL
score_avg_syllables <- NULL
score_avg_characters <- NULL

for (i in 1:nrow(songs_df)) {
  score_distinct_words <- c(score_distinct_words, DISTINCT_WORDS_WEIGHT * get_percentile_of_num(songs_distinct_words[i], songs_distinct_words))
  score_repetition_distance_weighted <- c(score_repetition_distance_weighted, REPETITION_DISTANCE_WEIGHT * get_percentile_of_num(songs_score_repetition_distance[i], songs_score_repetition_distance))
  score_avg_syllables <- c(score_avg_syllables, SYLLABLES_WEIGHT * get_percentile_of_num(songs_syllables[i], songs_syllables))
  score_avg_characters <- c(score_avg_characters, NUM_CHARACTERS_WEIGHT * get_percentile_of_num(songs_characters[i], songs_characters))
}

# Final song score
songs_score <- score_distinct_words + score_repetition_distance_weighted + score_avg_syllables + score_avg_characters

songs_df$score <- songs_score



###################### Analysis #################################

avg_score_df <- data.frame(year=unique(songs_df$year))
avg_score_by_year <- NULL
avg_proportion_distinct_word_by_year <- NULL
avg_num_of_syllables_by_year <- NULL
avg_num_of_characters_by_year <- NULL

for (i in 1965:2015) {
  # avg score by year 
  score_of_year <- subset(songs_df$score, songs_df$year == i)
  avg_score_by_year <- c(avg_score_by_year, mean(score_of_year))
  
  # avg proportion of distinct words w.r.t length by year
  distinct_words_of_year <- subset(songs_df$num_of_distinct_words, songs_df$year == i)
  length_of_year <- subset(songs_df$lyric_length , songs_df$year == i)
  avg_proportion_distinct_word_by_year <- c(avg_proportion_distinct_word_by_year, mean(distinct_words_of_year/length_of_year))
  
  # avg number of syllables by year
  syllables_of_year <- subset(songs_df$avg_num_of_syllables, songs_df$year == i)
  avg_num_of_syllables_by_year <- c(avg_num_of_syllables_by_year, mean(syllables_of_year))
  
  # avg number of characters by year
  characters_of_year <- subset(songs_df$avg_num_of_characters , songs_df$year == i)
  avg_num_of_characters_by_year <- c(avg_num_of_characters_by_year, mean(characters_of_year))
}

# avg score data frame
avg_score_df$avg_score <- avg_score_by_year
avg_score_df$avg_prop_distinct_words <- avg_proportion_distinct_word_by_year
avg_score_df$avg_num_syllables <- avg_num_of_syllables_by_year
avg_score_df$avg_num_characters <- avg_num_of_characters_by_year

# dumbest 10 songs
dumb_ten_songs <- songs_df[order(songs_df$score),][1:10, c("name", "artist", "score")]

# smartest 10 songs
smart_ten_songs <- songs_df[order(songs_df$score, decreasing = TRUE),][1:10, c("name", "artist", "score")]

# dumbest 10 artists
artist_song_score_df <- data.frame(artists = unique(songs_df$artist))
avg_score_of_artist <- NULL
for (a in artist_song_score_df$artists) {
  score <- mean(subset(songs_df$score, songs_df$artist == a))
  avg_score_of_artist = c(avg_score_of_artist, score)
}
artist_song_score_df$avg_score <- avg_score_of_artist

dumb_ten_artists <- artist_song_score_df[order(artist_song_score_df$avg_score),][1:10, ]

# smartest 10 artists
smart_ten_artsits <- artist_song_score_df[order(artist_song_score_df$avg_score, decreasing = TRUE),][1:10, ]


###################### Graph #####################################

# avg score by year trend
ggplot(data = avg_score_df, aes(x = year, y = avg_score, color = year)) + 
  geom_line(size=1) +
  geom_point(size=2) + 
  ylab("average score") +
  scale_x_continuous(name = "Year", breaks = seq(1965, 2015, by=5)) +
  ggtitle("Average Score by Year")

# regression of score on number of weeks
ggplot(data = songs_df, aes(x = num_of_weeks, y = score, color=year)) +
  geom_point(shape=1) +
  geom_smooth(method="lm", formula=y~x) +
  xlab("number of weeks") +
  ggtitle("Regression of Score on Number of Weeks")

# avg distinct word by year trend
ggplot(data = avg_score_df, aes(x = year, y = avg_prop_distinct_words , color = year)) + 
  geom_line(size=1) +
  geom_point(size=2) + 
  ylab("average num of proportion of distinct words w.r.t length") +
  scale_x_continuous(name = "Year", breaks = seq(1965, 2015, by=5)) +
  ggtitle("Average Proportion of Distinct Words by Year")

# avg num of characters per word by year
ggplot(data = avg_score_df, aes(x = year, y = avg_num_characters , color = year)) + 
  geom_line(size=1) +
  geom_point(size=2) + 
  ylab("average num of characters") +
  scale_x_continuous(name = "Year", breaks = seq(1965, 2015, by=5)) +
  ggtitle("Average Number of Characters by Year")

# avg nym of syllables per word by year
ggplot(data = avg_score_df, aes(x = year, y = avg_num_syllables , color = year)) + 
  geom_line(size=1) +
  geom_point(size=2) + 
  ylab("average num of syllables") +
  scale_x_continuous(name = "Year", breaks = seq(1965, 2015, by=5)) +
  ggtitle("Average Number of Syllables by Year")

################ Summary ##################
summary(songs_distinct_words)
summary(songs_score_repetition_distance)
summary(songs_characters)
summary(songs_syllables)


