
############################## Load Data and Packages #################################

id=read.table('~/Fall 2016/GR5243/Project4/Project4_data/common_id.txt')
load('~/Fall 2016/GR5243/Project4/Project4_data/lyr.RData')
traindata=read.table('~/Fall 2016/GR5243/Project4/Project4_data/mxm_dataset_train.txt',header = T)
setwd("~/Fall 2016/GR5243/Project4/Project4_data/data")
z <- c("A","B")
files.train <- dir(z, recursive=TRUE, full.names=TRUE)[1:2000]
files.validate <-dir(z, recursive=TRUE, full.names=TRUE)[2001:2350]

files.test <- dir('~/Fall 2016/GR5243/Project4/TestSongFile100',recursive=TRUE,full.names=TRUE)
files.test.name <- dir('~/Fall 2016/GR5243/Project4/TestSongFile100',recursive=TRUE,full.names = FALSE)
#source("https://bioconductor.org/biocLite.R")
#biocLite("rhdf5")
#devtools::install_github("cpsievert/LDAvisData")
library(rhdf5)
library(NLP)
#install.packages("lda")
library(lda)
library(tm)
#install.packages("LDAvis")
library(LDAvis)
#h5ls(files[1])
library(dplyr)
library(e1071)
library(tree)
library(randomForest)
library(servr)
library(nnet)
library(mlogit)
#install.packages("xgboost")
library(xgboost)
library(moments)
library(data.table)

h5ls(files.train[1])
pitches=h5read(files.train[1],"/analysis/segments_pitches")[,1:6]
timbre=h5read(files.train[1],"/analysis/segments_timbre")[2,]
rowMeans(timbre)
beats=h5read(files[715],"analysis/beats_start")
bars=h5read(files[4],"/analysis/bars_start")
which(beats-60>0)[1]
which.min(abs(beats-60))
max(which(beats==bars[2])-which(beats==bars[1]),0)
which.min(abs(h5read(files[3],"/analysis/beats_start")-60))
mean(h5read(files[3],"/analysis/segments_loudness_max"))
train_features[2,]
#quantile
#median
#mean

####################### Generating features funciton ################
quantile25=function(x){
  quantile(x,0.25)
}

quantile75=function(x){
  quantile(x,0.75)
}


genfeatures=function(files){
  
  n=length(files)
  
  bars_start_mean=vector()
  bars_start_25=vector()
  bars_start_75=vector()
  bars_start_median=vector()
  bars_start_sd=vector()
  
  beats_start_mean=vector()
  tempo=vector()
  time_signature=vector()
  beats_start_25=vector()
  beats_start_75=vector()
  beats_start_median=vector()
  beats_start_sd=vector()
  
  sections_start_mean=vector()
  sections_start_25=vector()
  sections_start_75=vector()
  sections_start_median=vector()
  sections_start_sd=vector()
  
  segments_loudness_max_mean=vector()
  segments_loudness_max_25=vector()
  segments_loudness_max_75=vector()
  segments_loudness_max_median=vector()
  segments_loudness_max_sd=vector()
  
  segments_loudness_max_time_mean=vector()
  segments_loudness_max_time_25=vector()
  segments_loudness_max_time_75=vector()
  segments_loudness_max_time_median=vector()
  segments_loudness_max_time_sd=vector()
  
  segments_loudness_start_mean=vector()
  segments_loudness_start_25=vector()
  segments_loudness_start_75=vector()
  segments_loudness_start_median=vector()
  segments_loudness_start_sd=vector()
  
  segments_start_mean=vector()
  segments_start_25=vector()
  segments_start_75=vector()
  segments_start_median=vector()
  segments_start_sd=vector()
  
  tatums_start_mean=vector()
  tatums_start_25=vector()
  tatums_start_75=vector()
  tatums_start_median=vector()
  tatums_start_sd=vector()
  
  segments_pitches_mean=matrix(ncol=12,nrow = n)
  segments_pitches_25=matrix(ncol=12,nrow = n)
  segments_pitches_75=matrix(ncol=12,nrow = n)
  segments_pitches_median=matrix(ncol=12,nrow = n)
  segments_pitches_sd=matrix(ncol=12,nrow = n)
  
  segments_timbre_mean=matrix(ncol=12,nrow = n)
  segments_timbre_25=matrix(ncol=12,nrow = n)
  segments_timbre_75=matrix(ncol=12,nrow = n)
  segments_timbre_median=matrix(ncol=12,nrow = n)
  segments_timbre_sd=matrix(ncol=12,nrow = n)
  segments_timbre_skew=matrix(ncol=12,nrow = n)
  segments_timbre_kurt=matrix(ncol=12,nrow = n)
  
  
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  for (j in 1:n){
    i=files[j]
    bars=h5read(i,"analysis/bars_start")
    beats=h5read(i,"analysis/beats_start")
    sections=h5read(i,"analysis/sections_start")
    slm=h5read(i,"analysis/segments_loudness_max")
    slmt=h5read(i,"analysis/segments_loudness_max_time")
    sls=h5read(i,"analysis/segments_loudness_start")
    ss=h5read(i,"analysis/segments_start")
    ts=h5read(i,"analysis/tatums_start")
    pitches=h5read(i,"analysis/segments_pitches")
    timbre=h5read(i,"analysis/segments_timbre")
    
    bars_start_mean[j]=mean(diff(bars))
    bars_start_25[j]=quantile(diff(bars),0.25)
    bars_start_75[j]=quantile(diff(bars),0.75)
    bars_start_median[j]=median(diff(bars))
    bars_start_sd[j]=max(sd(diff(bars)),0)
    
    beats_start_mean[j]=mean(diff(beats))
    tempo[j]=which(beats-60>0)[1]
    time_signature[j]=max(which(beats==bars[2])-which(beats==bars[1]),0)
    beats_start_25[j]=quantile(diff(beats),0,25)
    beats_start_75[j]=quantile(diff(beats),0.75)
    beats_start_median[j]=median(diff(beats))
    beats_start_sd[j]=max(sd(diff(beats)),0)
    
    sections_start_mean[j]=mean(diff(sections))
    sections_start_25[j]=quantile(diff(sections),0.25)
    sections_start_75[j]=quantile(diff(sections),0.75)
    sections_start_median[j]=median(diff(sections))
    sections_start_sd[j]=max(sd(diff(sections)),0)
  
    segments_loudness_max_mean[j]=mean(slm)
    segments_loudness_max_25[j]=quantile(slm,0.25)
    segments_loudness_max_75[j]=quantile(slm,0.75)
    segments_loudness_max_median[j]=median(slm)
    segments_loudness_max_sd[j]=max(sd(slm),0)
    
    segments_loudness_max_time_mean[j]=mean(slmt)
    segments_loudness_max_time_25[j]=quantile(slmt,0.25)
    segments_loudness_max_time_75[j]=quantile(slmt,0.75)
    segments_loudness_max_time_median[j]=median(slmt)
    segments_loudness_max_time_sd[j]=max(sd(slmt),0)
    
    segments_loudness_start_mean[j]=mean(diff(sls))
    segments_loudness_start_25[j]=quantile(diff(sls),0.25)
    segments_loudness_start_75[j]=quantile(diff(sls),0.75)
    segments_loudness_start_median[j]=median(diff(sls))
    segments_loudness_start_sd[j]=max(sd(diff(sls)),0)
    
    segments_start_mean[j]=mean(diff(ss))
    segments_start_25[j]=quantile(diff(ss),0.25)
    segments_start_75[j]=quantile(diff(ss),0.75)
    segments_start_median[j]=median(diff(ss))
    segments_start_sd[j]=max(sd(diff(ss)),0)
  
    tatums_start_mean[j]=mean(diff(ts))
    tatums_start_25[j]=quantile(diff(ts),0.25)
    tatums_start_75[j]=quantile(diff(ts),0.75)
    tatums_start_median[j]=median(diff(ts))
    tatums_start_sd[j]=max(sd(diff(ts)),0)
    
    segments_pitches_mean[j,]=rowMeans(pitches)
    segments_pitches_25[j,]=apply(pitches,1,quantile25)
    segments_pitches_75[j,]=apply(pitches,1,quantile75)
    segments_pitches_median[j,]=apply(pitches,1,median)
    segments_pitches_sd[j,]=apply(pitches,1,sd)
    
    segments_timbre_mean[j,]=rowMeans(timbre)
    segments_timbre_25[j,]=apply(timbre,1,quantile25)
    segments_timbre_75[j,]=apply(timbre,1,quantile75)
    segments_timbre_median[j,]=apply(timbre,1,median)
    segments_timbre_sd[j,]=apply(timbre,1,sd)
    segments_timbre_skew[j,]=apply(timbre,1,skewness)
    segments_timbre_kurt[j,]=apply(timbre,1,kurtosis)
    
    setTxtProgressBar(pb, j)
    
  }
  close(pb)
  H5close()
  segments_pitches_mean=as.data.frame(segments_pitches_mean)
  colnames(segments_pitches_mean)=c('pm1','pm2','pm3','pm4',
                                    'pm5','pm6','pm7','pm8',
                                    'pm9','pm10','pm11','pm12')
  
  segments_pitches_25=as.data.frame(segments_pitches_25)
  colnames(segments_pitches_25)=c('pq21','pq22','pq23','pq24',
                                    'pq25','pq26','pq27','pq28',
                                    'pq29','pq210','pq211','pq212')
  
  segments_pitches_75=as.data.frame(segments_pitches_75)
  colnames(segments_pitches_75)=c('pq71','pq72','pq73','pq74',
                                    'pq75','pq76','pq77','pq78',
                                    'pq79','pq710','pq711','pq712')
  
  segments_pitches_median=as.data.frame(segments_pitches_median)
  colnames(segments_pitches_median)=c('pme1','pme2','pme3','pme4',
                                    'pme5','pme6','pme7','pme8',
                                    'pme9','pme10','pme11','pme12')
  
  segments_pitches_sd=as.data.frame(segments_pitches_sd)
  colnames(segments_pitches_sd)=c('ps1','ps2','ps3','ps4',
                                    'ps5','ps6','ps7','ps8',
                                    'ps9','ps10','ps11','ps12')
  
  segments_timbre_mean=as.data.frame(segments_timbre_mean)
  colnames(segments_timbre_mean)=c('tm1','tm2','tm3','tm4',
                                   'tm5','tm6','tm7','tm8',
                                   'tm9','tm10','tm11','tm12')
  
  segments_timbre_25=as.data.frame(segments_timbre_25)
  colnames(segments_timbre_25)=c('tq21','tq22','tq23','tq24',
                                   'tq25','tq26','tq27','tq28',
                                   'tq29','tq210','tq211','tq212')
  
  segments_timbre_75=as.data.frame(segments_timbre_75)
  colnames(segments_timbre_75)=c('tq71','tq72','tq73','tq74',
                                   'tq75','tq76','tq77','tq78',
                                   'tq79','tq710','tq711','tq712')
  
  segments_timbre_median=as.data.frame(segments_timbre_median)
  colnames(segments_timbre_median)=c('tme1','tme2','tme3','tme4',
                                   'tme5','tme6','tme7','tme8',
                                   'tme9','tme10','tme11','tme12')
  
  segments_timbre_sd=as.data.frame(segments_timbre_sd)
  colnames(segments_timbre_sd)=c('ts1','ts2','ts3','ts4',
                                   'ts5','ts6','ts7','ts8',
                                   'ts9','ts10','ts11','ts12')
  
  segments_timbre_skew=as.data.frame(segments_timbre_skew)
  colnames(segments_timbre_skew)=c('tsk1','tsk2','tsk3','tsk4',
                                 'tsk5','tsk6','tsk7','tsk8',
                                 'tsk9','tsk10','tsk11','tsk12')
  
  segments_timbre_kurt=as.data.frame(segments_timbre_kurt)
  colnames(segments_timbre_kurt)=c('tk1','tk2','tk3','tk4',
                                 'tk5','tk6','tk7','tk8',
                                 'tk9','tk10','tk11','tk12')
  
  data=data.frame(bars_start_mean,
                  bars_start_25,
                  bars_start_75,
                  bars_start_median,
                  bars_start_sd,
                  beats_start_mean,
                  tempo,
                  time_signature,
                  beats_start_25,
                  beats_start_75,
                  beats_start_median,
                  beats_start_sd,
                  sections_start_mean,
                  sections_start_25,
                  sections_start_75,
                  sections_start_median,
                  sections_start_sd,
                  segments_loudness_max_mean,
                  segments_loudness_max_25,
                  segments_loudness_max_75,
                  segments_loudness_max_median,
                  segments_loudness_max_sd,
                  segments_loudness_max_time_mean,
                  segments_loudness_max_time_25,
                  segments_loudness_max_time_75,
                  segments_loudness_max_time_median,
                  segments_loudness_max_time_sd,
                  segments_loudness_start_mean,
                  segments_loudness_start_25,
                  segments_loudness_start_75,
                  segments_loudness_start_median,
                  segments_loudness_start_sd,
                  segments_start_mean,
                  segments_start_25,
                  segments_start_75,
                  segments_start_median,
                  segments_start_sd,
                  tatums_start_mean,
                  tatums_start_25,
                  tatums_start_75,
                  tatums_start_median,
                  tatums_start_sd)
  data=cbind(data,segments_timbre_mean,
             segments_timbre_25,segments_timbre_75,
             segments_timbre_median,segments_timbre_sd,
             segments_timbre_skew,segments_timbre_kurt,
             segments_pitches_mean,segments_pitches_25,
             segments_pitches_75,segments_pitches_median,
             segments_pitches_sd)
  return(data)
}

############### generating features ############################
train_features <- genfeatures(files.train)
validate_features <- genfeatures(files.validate)
test_features <- genfeatures(files.test)

n.variables=dim(train_features)[2]
for ( i in 1:n.variables){
  train_features[,i][is.nan(train_features[,i])] <- mean(train_features[,i],na.rm = T)
  train_features[,i][is.na(train_features[,i])] <- mean(train_features[,i],na.rm = T)
  validate_features[,i][is.nan(validate_features[,i])] <- mean(validate_features[,i],na.rm = T)
  validate_features[,i][is.na(validate_features[,i])] <- mean(validate_features[,i],na.rm = T)
}


for ( i in 1:n.variables){
  test_features[,i][is.nan(test_features[,i])] <- mean(test_features[,i],na.rm = T)
  test_features[,i][is.na(test_features[,i])] <- mean(test_features[,i],na.rm = T)
}

############### K-means #########################################
#songs_cluster <- kmeans(train_features,20,20)
#names(train_features)
#songs_cluster



##################### LDA #####################################

f_words<-colSums(lyr[,-1])
stop_words <- stopwords("English")
del <- colnames(lyr[,-1]) %in% stop_words | f_words < 5
term.table <- f_words[!del]
vocab_m <- names(term.table)
index <- match(vocab_m,colnames(lyr))

lyr_cleaned<-lyr[,index]
trackid <- lyr[,1]
rownames(lyr_cleaned) <- trackid
get.terms <- function(x) {
        index <-which(x != 0)
        rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

documents <- apply(lyr_cleaned,1, get.terms)

D <- length(documents)  # number of documents (2,000)
W <- length(vocab_m)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

K <- 15
G <- 5000
alpha <- 5
eta <- 0.02

fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab_m, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

Lyrics <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab_m,
                     term.frequency = term.frequency)

#install.packages("servr")

json <- createJSON(phi = Lyrics$phi, 
                   theta = Lyrics$theta, 
                   doc.length = Lyrics$doc.length, 
                   vocab = Lyrics$vocab, 
                   term.frequency = Lyrics$term.frequency)
serVis(json, out.dir = 'vissample', open.browser = T)

cluster_LDA=vector()

for (i in 1:2350){
  cluster_LDA[i]=which.max(theta[i,])
}
######################### the relationship ####################

theta[which(theta>0.06667)]=1
theta[which(theta<=0.06667)]=0
theta=data.frame(theta)
#colnames(theta)=c("V1","V2","V3","V4","V5","V6","V7","V8",
#                  "V9","V10","V11","V12","V13","V14","V15")
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#train_features2=as.matrix(train_features) %*% diag(1/sapply(train_features,max,2))
#train_features2=scale(as.matrix(train_features))
train_features2=as.data.frame(lapply(train_features,normalize))
train_features3=train_features
train_features3[,1:42]=as.data.frame(lapply(train_features3[,1:42],normalize))

validate_features2=as.data.frame(lapply(validate_features,normalize))
validate_features3=validate_features
validate_features3[,1:42]=as.data.frame(lapply(validate_features3[,1:42],normalize))

test_features[,1:42]=as.data.frame(lapply(test_features[,1:42],normalize))
#table(as.vector(cluster_LDA)[1:2000],as.vector(songs_cluster$cluster))


train_data=cbind(train_features2,theta[1:2000,])

validate_data=cbind(validate_features2,theta[2001:2350,])

#theta[,1]
#train_data %>%
#  group_by(cluster)%>%
#  summarise(avg_tempo=mean(tempo),
#            avg_time_signature=mean(time_signature),
#            total=n())

#table(as.vector(cluster_LDA))

######################### SVM ############################################
svm.linear=svm(as.factor(X15)~.,data=train_data[,c(1:42,57)],cost=1,kernel="radial")
summary(predict(svm.linear,validate_features2))
mean(predict(svm.linear,validate_features2)==theta[2001:2350,15])

prediction=matrix(ncol=15,nrow=350)
for(i in 1:15){
  data=cbind(train_features2[,43:162],y=theta[1:2000,i])
  svm.linear=svm(as.factor(y)~.,data=data,cost=1,kernel="radial")
  prediction[,i]=predict(svm.linear,validate_features2[,43:162])
  l=summary(prediction[,i])
  print(l)
  print(mean(prediction[,i]==theta[2001:2350,i]))
}

##########################  Tree  ########################################
#install.packages("tree")
#tree=tree(as.factor(X1)~.,data=train_data[,1:43])
#mean(predict(tree,validate_features,type="class")==theta[2001:2350,1])


########################## Random Forest #################################
#install.packages("randomForest")
#rf=randomForest(as.factor(X1)~.,data=train_data[,1:43],ntree=300)
#mean(predict(rf,validate_features)==theta[2001:2350,1])

########################## Neural Network #################################

nn=nnet(as.factor(X1)~.,data=train_data[,1:163],size=20,rang = 0.2,decay=5e-4,maxit=200)
mean(predict(nn,validate_features2,type="class")==theta[2001:2350,1])

#prediction=matrix(ncol=15,nrow=350)
#acc3=vector()
#for(i in 1:15){
#  data=cbind(train_features3,y=theta[1:2000,i])
#  nn=nnet(as.factor(y)~.,data=data,size=20,rang=0.2,decay=5e-4,maxit=300,trace=F,MaxNWt=5000)
#  prediction[,i]=as.vector(as.numeric(predict(nn,validate_features3,type="class")))
#  l=table(prediction[,i])
#  print(l)
#  acc3[i]=mean(prediction[,i]==theta[2001:2350,i])
#  print(acc2[i])
#}


accuracy=function(train_features,validate_features,m=theta){
  prediction=matrix(ncol=15,nrow=350)
  acc=vector()
  for(i in 1:15){
    data=cbind(train_features,y=m[1:2000,i])
    nn=nnet(as.factor(y)~.,data=data,size=20,rang=0.2,decay=5e-4,maxit=300,trace=F,MaxNWt=5000)
    prediction[,i]=as.vector(as.numeric(predict(nn,validate_features,type="class")))
    l=table(prediction[,i])
    print(l)
    acc[i]=mean(prediction[,i]==m[2001:2350,i])
    print(acc[i])
  }
  return(acc)
}

#acc1=accuracy(train_features2,validate_features2)
acc2=accuracy(train_features3,validate_features3)

#phi[2,]
#dim(phi)
#sum(phi[2,])
#prediction[1,]
#colSums(as.matrix(phi[prediction[1,],]))
#sort(colSums(as.matrix(phi[prediction[1,],])),decreasing = T)[1:100]
#sort(colSums(as.matrix(lyr[theta[,1]==1,2:5001])),decreasing = T)[1:100]

#sort(colSums(as.matrix(fit$topics)),decreasing = T)[1:100]

cluster_info=matrix(nrow=15,ncol=5000)
for (i in 1:15){
  cluster_info[i,]=colSums(as.matrix(lyr[theta[,i]==1,2:5001]))
}
colnames(cluster_info)=colnames(lyr)[2:5001]
cluster_info[,c(2,3,6:30)]=0



pred=function(train_features,test_features,m=theta){
  prediction=matrix(ncol=15,nrow=dim(test_features)[1])
  for (i in 1:15){
    data=cbind(train_features,y=m[,i])
    nn=nnet(as.factor(y)~.,data=data,size=20,rang=0.2,decay=5e-4,maxit=300,trace=F,MaxNWt=7000)
    prediction[,i]=as.vector(as.numeric(predict(nn,test_features,type="class")))
  }
  return(prediction)
}

all_train_features=rbind(train_features3,validate_features3)
prediction=pred(all_train_features,test_features)


rankings=matrix(nrow=100,ncol=5000)
for (i in 1:100){
  if (sum(prediction[i,])==0){
    rankings[i,]=frankv(colSums(cluster_info),order=-1,ties.method = "average")
  }
  if(sum(prediction[i,])==1){
    rankings[i,]=frankv(cluster_info[prediction[i,]==1,],order=-1,ties.method = "average")
  }
  else{
    rankings[i,]=frankv(colSums(cluster_info[prediction[i,]==1,]),order=-1,ties.method="average")
  }
}
colnames(rankings)=colnames(cluster_info)
rownames(rankings)=files.test.name
rankings[,c(2,3,6:30)]=4987
write.csv(rankings,'~/Fall 2016/GR5243/Project4/results.csv',row.names = T,col.names = T )

########################## XGBoost #######################################
#xgboost=xgboost(data=as.matrix(train_features),
#                label=as.factor(train_data$cluster),
#         max.depth=2,eta=0.5,nthread=2,nround=2)
#mean(predict(xgboost,as.matrix(validate_features))==cluster_LDA[2001:2350])

######################### Multi-class Logistic ############################
#mlogit(cluster~.,train_data)
