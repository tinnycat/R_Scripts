################################################################################
#
#  American Populism Project - Text Pre-processing
#  Author: Hannah Cake 
#  Last updated: 4/29/2024
#
################################################################################

# Clear environment 
rm(list = ls())

# Load packages 
library(quanteda)
library(readtext)
library(haven)
library(textclean)
library(tidyverse)
library(tm)

### Loading Data/Texts

# Load document variables 
doc_vars <- read.csv("rep_data_final.csv")
View(doc_vars)

# Load and read in Text
filelocation <- "C:/Users/Hannah Cake/Documents/Research Agenda/American Populism II/American Populism/corpus"
saveencodes <- encoding(readtext(filelocation))
onecharcvec <- saveencodes$all
setdiff(onecharcvec, iconvlist())
identified_text <- readtext(filelocation, encoding = onecharcvec)
text_newread <- readtext(filelocation, encoding = onecharcvec, docvarsfrom = "metadata")

# Create corpus
camp_corpus <- corpus(text_newread)
summary(camp_corpus)

### Cleaning Corpus
# Tokenize / Removing stop words and numbers, change to lower case
clean_camp <- camp_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower()
# Convert to DFM 
camp_dfm <- dfm(clean_camp)
print(camp_dfm)

### Create dictionary 
gender_pop <- dictionary(list(dict_gender = c("gender","mother", "transgender",
                                       "trans", "lgbt", "lgbtq", 
                                       "girl","female", "male", "abortion",
                                       "unborn", "woman", "women",
                                       "men", "man"),
                              dict_populist = c("corrupt","big", "elite", "woke", 
                                         "socialist", "indoctrinate", 
                                         "indoctrination", "radical",
                                         "lie", "lying", "establishment")))

# Check that dictionary is properly constructed
is.dictionary(gender_pop)

# Apply dictionary and convert to data frame 
gender_pop_dict <- dfm_lookup(camp_dfm, gender_pop)
gender_pop_dict
dict.df <- convert(gender_pop_dict, to = "data.frame")
View(dict.df)

# Merge data frame with document vars
camp_txt <- merge(dict.df, doc_vars, by = c("doc_id"))
View(camp_txt)

# Export as CSV 
write.csv(camp_txt, "camp_txt.csv", row.names = FALSE)


