# General purpose
from collections import Counter
import numpy as np
import os
import warnings
warnings.filterwarnings(action = 'ignore')

# Data wrangling
import pandas as pd  # similar to tidyverse

# Visualization
import matplotlib.pyplot as plt
import seaborn as sns

# Text mining
import nltk
import string
nltk.download("omw-1.4")
nltk.download('punkt')
nltk.download('stopwords')
nltk.download('wordnet')
from nltk.corpus import stopwords
from nltk.util import ngrams
from nltk.stem import WordNetLemmatizer
from nltk.tokenize import word_tokenize
from nltk.tokenize import WordPunctTokenizer
from wordcloud import WordCloud
from wordcloud import STOPWORDS
from textblob import TextBlob
from textblob import Word

# Import the data
df=pd.read_csv("workshop_data.csv", encoding='latin-1')

# Examine the data
df.head(n=2)
df.info()

# Look at a single review
df.loc[:,'Review_Text'].values[10]

# Convert dates
df.Year_Month=df.Year_Month.apply(pd.to_datetime, errors ='coerce')
df['Month']=df.Year_Month.dt.month
df['Year']=df.Year_Month.dt.year

# Convert to lowercase
df['Clean_Text'] = df['Review_Text'].apply(lambda x : ' '.join(x.lower() for x in x.split()))

# Create a function to remove punctuation
def remove_punctuations(text):
  for char in string.punctuation:
    text = text.replace(char, '')
  return text

# Remove punctuation
df['Clean_Text'] = df['Clean_Text'].apply(remove_punctuations)

# Import list of stop words
stop = stopwords.words('english')

# Create a custom list of stop words
wc_removals = ['day', 'disney', 'disneyland', 'rides', 'park']

# Combine stop word lists
stop_extended = stop + wc_removals

# Remove stop words
df['Clean_Text'] = df['Clean_Text'].apply(lambda x : ' '.join(x for x in x.split() if x not in stop_extended))

# Look at the data
df['Clean_Text'].head()

# Tokenize
df['Tokens'] = df['Clean_Text'].apply(lambda x: TextBlob(x).words)

# Look at the data
df['Tokens'].head()

# Calculate word frequency
df['Frequency'] = df['Clean_Text'].apply(lambda x: len(str(x).split(' '))) 

# Look at the data
df['Frequency'].head()

# Convert words to single text
all_review_text = ' '.join(i for i in df.Review_Text)
all_clean_text = ' '.join(i for i in df.Clean_Text)

# Create a word cloud
wordcloud = WordCloud(
    colormap="Set2",
    collocations=False).generate(all_review_text)
plt.figure(figsize=[11,11])
plt.imshow(wordcloud, interpolation="bilinear")
plt.axis("off")
plt.title("Disney Park Reviews")
plt.show()

# Create a word cloud without stop words
wordcloud = WordCloud(
    colormap="Set2",
    collocations=False).generate(all_clean_text)
plt.figure(figsize=[11,11])
plt.imshow(wordcloud, interpolation="bilinear")
plt.axis("off")
plt.title("Disney Park Reviews")
plt.show()

# Plot reviews by park
sns.set(rc = {'figure.figsize':(15, 5)})
sns.countplot(x=df["Branch"])

# Plot ratings by year
sns.lineplot(data=df, x="Year", y="Rating", hue="Branch").set(title="Ratings per Year");

# Plot the most common words (less stop words)
most_common = Counter(' '.join(df['Clean_Text']).split()).most_common(20)
most_common_df = pd.DataFrame(most_common, columns = ['Words', 'Freq'])
sns.barplot(data = most_common_df, x = 'Words', y = 'Freq')
plt.title('20 Most Common Words')
plt.xticks(rotation = 60)
plt.show()

# Function to create n-grams
def documentNgrams(documents, size):
    ngrams_all = []
    for document in documents:
        tokens = document.split()
        if len(tokens) <= size:
            continue
        else:
            output = list(ngrams(tokens, size))
        for ngram in output:
            ngrams_all.append(" ".join(ngram))
    cnt_ngram = Counter()
    for word in ngrams_all:
        cnt_ngram[word] += 1
    df = pd.DataFrame.from_dict(cnt_ngram, orient='index').reset_index()
    df = df.rename(columns={'index':'words', 0:'count'})
    df = df.sort_values(by='count', ascending=False)
    df = df.head(15)
    df = df.sort_values(by='count')
    return(df)
  
# Function to plot the n-grams
def plotNgrams(documents):
    unigrams = documentNgrams(documents, 1)
    bigrams = documentNgrams(documents, 2)
    
    # Set plot figure size
    fig = plt.figure(figsize = (20, 6))
    plt.subplots_adjust(wspace=.5)

    ax = fig.add_subplot(131)
    ax.barh(np.arange(len(unigrams['words'])), unigrams['count'], align='center', alpha=.5)
    ax.set_title('Unigrams')
    plt.yticks(np.arange(len(unigrams['words'])), unigrams['words'])
    plt.xlabel('Count')

    ax2 = fig.add_subplot(132)
    ax2.barh(np.arange(len(bigrams['words'])), bigrams['count'], align='center', alpha=.5)
    ax2.set_title('Bigrams')
    plt.yticks(np.arange(len(bigrams['words'])), bigrams['words'])
    plt.xlabel('Count')

    plt.show()
    
# Plot n-grams
plotNgrams(df['Clean_Text'])

# Create bigrams
bigrams = ngrams(word_tokenize(df['Clean_Text'].sum()), 2)
bigrams_freq = Counter(bigrams)
bigrams_freq.most_common(10)

# Create trigrams
trigrams = ngrams(word_tokenize(df['Clean_Text'].sum()), 3)
trigrams_freq = Counter(trigrams)
trigrams_freq.most_common(10)
