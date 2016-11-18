# Project: Words 4 Music

### [Project Description](doc/Project4_desc.md)

![image](http://cdn.newsapi.com.au/image/v1/f7131c018870330120dbe4b73bb7695c?width=650)

Term: Fall 2016

+ [Data link](https://courseworks2.columbia.edu/courses/11849/files/folder/Project_Files?preview=763391)-(**courseworks login required**)
+ [Data description](doc/readme.html)
+ Contributor's name:Wenhang Bao
+ Projec title: Lorem ipsum dolor sit amet
+ Project summary: Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
	
Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

# Code:
My code which is /Project4.R is in folder /lib.
The output file /results.csv is in folder /output

# Methodology:

## 1 Features Extraction
### Data structure

In this part, I extracted the 25% quantile, mean, 75% quantile, sd and median of each feature.Moreover I extracted the skewness and kurtosis of timbre as it is a key feature in music genre recognition. And I calculated the tempo and time signiture which are also used as features. The music features are with the dimension 2350 * 182. 

### Data cleaning

I use mean to substitute the NA and NaN.

### Data Normalization
I normalized all the features except timbre and pitches. 

## 2 Models

### Lyrics

I used topic moddling techniques--LDA for lyrics. 
After parameter tuning and reading articles, the parameters are set to:

K <- 15
G <- 5000
alpha <- 5
eta <- 0.02

which could give us balanced clusters(no especially large cluster). 

### Features model
I tried several multi-class models but their performances are really poor. 
I tried several binary-class models and Neural Network has the best performance.
So I construct Neural Network model for each of the 15 Lyrics topics. And the average validation accuracy is 62%. 
I tuned the parameters and hyperparameters. The best parameters space are :

size=20
rang=0.2
decay=5e-4
maxit=300
MaxNWt=7000

## 3 Results

I extracted the testset's features. And use Neural Network models to predict its topic. 
If the song cannot be labaled as even 1 of the topic, the algorithm will just give the most generalized rank of words. 
If the song is labled as only 1 topic, the algorithm will return the rank of words in that specific topic.
If the song is labled for multiple topic, the algorithm will return the average rank of words in all the related topic.

Here I use topic's words rank directly as song's words rank to avoid overfitting. 
