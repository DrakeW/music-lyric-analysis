# Convert json file to data frame in R

#################### Packages ##################

require(RJSONIO)    
install.packages("RJSONIO")
install.packages("plyr")
install.packages("hash")

library("hash")


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
write.csv(all_songs, file = "~/dev/data_analysis/songs_all.csv")

# Read in clean data
songs_df <- read.csv("~/dev/data_analysis/songs_all.csv")

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
#   3. for avg number of syllables, + (avg_sylabbles * number of words)

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
DISTINCT_WORDS_WEIGHT = 1

songs_score_distinct_words = NULL
for (i in 1:nrow(songs_df)) {
  score_distinct_words = DISTINCT_WORDS_WEIGHT * length(unique(unlist(songs_df$processed_lyric[i])))
  songs_score_distinct_words = c(songs_score_distinct_words, score_distinct_words)
}


###################### 2 ##########################
songs_score_repetition_distance <- NULL

lost_score_calculator <- function(distance) {
  if (distance > 5) {
    0
  }else if (distance > 3) {
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

####################### 3 ######################

songs_score_syllables = NULL

get_num_of_syllables_of_word = function(word) {
  nchar(gsub("[^X]", "", gsub("[aeiouy]+", "X", tolower(word))))
}

for (i in 1:nrow(songs_df)) {
  song_lyric <- songs_df$processed_lyric[i]
  num_of_syllables <- NULL
  for (index in 1:length(unlist(song_lyric))) {
    song_lyric <- unlist(song_lyric)
    word <- song_lyric[index]
    num_of_syllables <- c(num_of_syllables, get_num_of_syllables_of_word(word))
  }
  songs_score_syllables = c(songs_score_syllables, mean(num_of_syllables))
}



summary(songs_score_syllables)
summary(songs_score_repetition_distance)
summary(songs_score_distinct_words)
