# Predict_the_next_word_app
Predict the next word given some preceding words 

A pipeline of R scripts to build a ngram model from corpus from online sources and use that model to predict the next word given a phrase/sentence. A simple web app is built based on this prediction algorithm using shiny. This project is the capstone project for Data Science Specialization from Johns Hopkins University on Coursera  

## Milestone_Report.Rmd
**Inputs**: .txt file of corpus of online news, blogs and tweets <br />
**Outputs**: a summary report (pdf or html) <br />
**Steps**: <br />
1. Read in .txt files 
2. Report total words, lines and characters 
3. Build ngram models 
4. Show histograms of unigrams, bigrams and trigrams 
5. Show a scatterplot of correlation between percentage of words instances covered and minimal number of words required to achieve that coverage.
6. Show a histogram of top 20 most frequently used bigrams and trigrams 
A published html version can be found on Rpubs: https://rpubs.com/missingboy/296695

## KN_Ngram_model.R
**Inputs**: .txt file of corpus of online news, blogs and tweets <br />
**Outputs**: .csv file of normalized frequencies for unigrams to fourgrams respectively <br />
**Steps**: <br />
1. Read in .txt files 
2. Preprocess corpus texts: extend contraction, convert to lower text, remove punctuation and numbers. Remove foreign words
3. Build tokens and ngram tables 
4. Build normalized frequencies for unigram, bigram, trigram and fourgram sequentially using ngram tables with Kneser-Ney Smoothing 

## nextword3options
Stored all scripts and raw data used to build the shiny app to predict the next words. **prediction_three_choices.R, ui.R and server.R** are stored in the folder **nextword3options**, together with inputs .csv files.

## prediction_three_choices.R
**Inputs**: .csv files of normalized frequencies for bigrams to fourgrams and a phrase/sentence as preceding words for prediction of the next word <br /> 
**Outputs**: three words with the highest frequencies given one to three preceding words <br />
**Steps**: <br />
1. Read in .csv files and the phrase/sentence 
2. Preprocess the phrase/sentence  
3. Starting from fourgrams, search for top three words with the highest probability given the three preceding words. If the three preceding words are not found, recursively “downgrade” to trigrams and search for the top three words with the highest probability give the two preceding words. If still not found, “downgrade” to bigram or unigram and repeat the process. Then output these three words. 

## ui.R and server.R
ui.R and server.R together make a simple online app that takes a phrase/sentence and outputs three possible next words. The app can be found at https://lijiangds.shinyapps.io/nextword3options/ 
A published short presentation of this can be found on Rpubs: https://rpubs.com/missingboy/302036
