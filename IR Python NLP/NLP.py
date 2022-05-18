
# NLP vs text mining:
#
# -text mining is ripping apart words, getting rid of puntuation, etc
# -NLP depends on previous knowledge of the language!


import pandas as pd

import nltk

nltk.download('stopwords')
nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')
nltk.download('wordnet')

import gensim


f = open(
    "path\survey comments.txt",
    "r", encoding="utf8")

Survey = f.read()

print(Survey[:100])

#Exploratory Analysis
# creating a function called "count words" that splits document into words
def count_words(doc):
    doc = doc.split()
    words_counts = dict.fromkeys(set(doc), 0)
    for word in doc:
        words_counts[word] += 1

        return words_counts

original_counts = count_words(Survey)

print(original_counts)

n_unique_original = len(original_counts)

print(n_unique_original)

# Now we're cleaning! NOTE: text cleaning is - cleaning, then looking at what we got, then cleaning again, then looking again!

# turning everything into lowercase
cleaned = Survey.lower()

# counting the unique words after normalizing case (turning all to lower case)
cleaned_counts = count_words(cleaned)

n_unique_cleaned = len(cleaned_counts)
print(n_unique_cleaned)

# removing punctuations
s = "I'm a self-taught programmer."
# regex cheat sheet https://www.rexegg.com/regex-quickstart.html


# In[ ]:


# regex library in python
import re

cleaned = re.sub(r"-", " ", cleaned)
cleaned = (re.sub(r"[^\w\s]", "", cleaned))
print(cleaned[:100])

# removing underscores and replacing with "" (nothing)
cleaned = re.sub(r"_", "", cleaned)

# removing numbers now replacing with nothing
cleaned = re.sub(r"[0-9]", "", cleaned)

# checking how much the above cleaning has reduced the number of tokens in our corpus
cleaned_counts = count_words(cleaned)
n_unique_words = len(cleaned_counts)
print(n_unique_words)

# getting word frequency and then putting it into dataframe using pandas
cleaned_counts = pd.Series(cleaned).value_counts()
pd.DataFrame(cleaned_counts, columns=['COUNT']).head(25)

# This shows us there are a lot of stopwords - plotting word frequency
cleaned_counts.plot(figsize=(15, 10), ylabel="Count", xlabel="Word");

# defining a function on word counts but on a smaller subset
def plot_counts(word_counts, n_words=100, label_sample=5):
    xticks_sample = range(0, n_words, label_sample)

    word_counts[:n_words].plot(
        figsize=(15, 10),
        ylabel="Count",
        xlabel="Word",
        xticks=xticks_sample,
        rot=90
    );


plot_counts(cleaned_counts, n_words=200, label_sample=5)

# importing two different nlp libraries
from nltk.corpus import stopwords
from gensim.parsing.preprocessing import STOPWORDS

# loaded two different lists of stopwords (note the difference in length of lists!)
nltk_stopwords = stopwords.words('english')
gensim_stopwords = list(STOPWORDS)

print(
    "Number of entries in `ntlk` stop list:", len(nltk_stopwords),
    "\nNumber of entries in `gensim` stop list:", len(gensim_stopwords)
)

# In[ ]:


print(nltk_stopwords)

#

# In[ ]:


print(gensim_stopwords)

# IMPORTANT: Look at the list of stop words and see if there's anything you need to get rid of (because maybe "anyway" is important to your corpus) or need to add.
#
# OR: creating stopwords from scratch
#
# **Check**: 3.1.5.2.

# **3.2. Text Cleaning: Advanced**

# In[ ]:


# stemming - brute force token reduction
# porterStemmer goes through steps where it removes "ing", etc
from nltk.stem.porter import PorterStemmer

stemmer = PorterStemmer()

# In[ ]:


# creating list of words called to_stem that we're stemming
to_stem = ['books', 'having', 'running', 'complicated', 'complicity', 'malleability']

for word in to_stem:
    print(f"{word:<12} => {stemmer.stem(word)}")

# PROBLEM: complicated and complicity become the same thing - boo!

# Lemmatization: tree model of the english language and how the words are related and what parts of speech they are.
#
# This is NLP now!
#
# ***The revised text cleaning steps would look like this:***
#
# Tokenize
#
# Assign POS tags
#
# Resolve word casing
#
# Remove punctuation
#
# Remove numbers
#
# Remove extra whitespaces
#
# Remove stopwords
#
# Lemmatize

# In[ ]:


# lemmatizing a sample sentence

import nltk
from nltk.tokenize import word_tokenize
from nltk.stem import WordNetLemmatizer
from nltk.corpus import wordnet

sample_string = """
The strong coffee, which I had after lunch, was $3. It kept me going the rest of the day.
"""
tokenized = word_tokenize(sample_string)

print("String after `nltk` tokenization:\n")
for entry in tokenized:
    print(entry)

# In[ ]:


tagged = nltk.pos_tag(tokenized)

print("Tagged tokens:\n")
for entry in tagged:
    print(entry)

# In[ ]:


# putting above into easier-to-read dataframe
tagged = pd.DataFrame(tagged, columns=['WORD', 'TAG'])
tagged

# In[ ]:


# cleaning by removing punctation, etc
tagged = tagged.assign(WORD=tagged['WORD'].str.lower())
tagged = tagged[~tagged['WORD'].isin([",", "$", "."])]
tagged = tagged[tagged['WORD'].str.isalpha()]
tagged

# In[ ]:


# creating a lemmatizer and tagging with wordnet tag
lemmatizer = WordNetLemmatizer()


def convert_tag(tag):
    if tag.startswith('J'):
        tag = wordnet.ADJ
    elif tag.startswith('V'):
        tag = wordnet.VERB
    elif tag.startswith('N'):
        tag = wordnet.NOUN
    elif tag.startswith('R'):
        tag = wordnet.ADV
    else:
        tag = ''
    return tag


# In[ ]:


tagged = tagged.assign(NEW_TAG=tagged['TAG'].apply(convert_tag))
tagged


# In[ ]:


# now we lemmatize!
def lemmatize_word(word, new_tag):
    if new_tag != '':
        lemma = lemmatizer.lemmatize(word, pos=new_tag)
    else:
        lemma = lemmatizer.lemmatize(word)
    return lemma


tagged = tagged.assign(
    LEMMATIZED=tagged.apply(lambda row: lemmatize_word(row['WORD'], row['NEW_TAG']), axis=1)
)
tagged

# KEY: do a thing, then LOOK AT YOUR DATA! Don't use cookie-cutter approach, look at what each process is doing to your data!

#
