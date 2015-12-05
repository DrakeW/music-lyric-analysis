###Authors###
1. (Grace) Yuhan Bian (**grace.b@berkeley.edu**)
2. Junyu Wang (**junyuw@berkeley.edu**)
3. Aoyi Shan (**aoyi95@berkeley.edu**)
4. Yuxi Tao (**taoyssi@berkeley.edu**)

----

###Project description###
This project aims to exam how pop music has changed over the past three decades(1965-2015). With data obtained from the Billboard website, we scraped lyrics of weekly top ranked songs and assigned each song an “intelligence score”. This score combines four criterias to evaluate the literacy level of a song: 1. the total word count of lyric; 2. the length of unique words in the lyrics; 3. the number of syllables in each word; 4. the distance between repeated words. Our evaluating model is set to state that the higher the score, the more intelligent the song. As a result of our analysis, we achieved findings that 1. the Intelligence Score of music has substantially decreased over the past three decades; 2. the number of weeks a song is ranked #1 is negatively correlated with its Intelligence Score. We infer from our findings that nowadays, the complexity of music is decreasing, and the “dumber” a song is, the more popular it is among musical listeners.

--------

###Organization of Files###
* Stats_133_Final_Project
    * Raw_data
        * song_{year}.json
    * Clean_data
        * songs_all.csv
        * scraper.py
        * run.sh
    * Code
        * song_analysis.R
    * Images
        * average_number_of_chars_by_year_plot.pdf
        * average_number_of_syllables_by_year_plot.pdf
        * average_proportion_of_uniq_words_by_eyar_plot.pdf
        * average_score_by_year_plot.pdf
        * regression_of_scoer_on_number_of_weeks.pdf
    * Report
    * skeleton.R
    * README.md

