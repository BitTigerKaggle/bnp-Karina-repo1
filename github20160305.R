####load data####
train_raw<-data.table::fread('./data/train.csv',stringsAsFactors=T)
test_raw<-data.table::fread('./data/test.csv',stringsAsFactors=T)
test_raw<-as.data.frame(test_raw) #dont know why have to define again
submission_raw<-data.table::fread('./data/sample_submission.csv',stringsAsFactors=T)

#####convert character variables to numeric variables####
train_chr<-train_raw[sapply(train_raw,is.factor)]
temp<-sapply(train_chr,as.numeric)
temp<-as.data.frame(temp)
train_num<-train_raw[sapply(train_raw,is.numeric)]
train_num<-cbind(train_num,temp)

test_chr<-test_raw[sapply(test_raw,is.factor)]
temp<-sapply(test_chr,as.numeric)
temp<-as.data.frame(temp)
test_num<-test_raw[sapply(test_raw,is.numeric)]
test_num<-cbind(test_num,temp)

####model####
#logistic
fit<-lm(target~.,train_num_cor)
fit_select<-fit$coefficients[which(fit$coefficients<0.001)]
fit_select<-fit_select[2:length(fit_select)]
temp<-train_num_cor[,c('target',names(fit_select))]
temp2<-test_num_cor[,c(names(fit_select))]

fit<-lm(target~.,temp)
res=predict(fit,temp2)  #submission1

submission_log<-submission_raw
submission_log$PredictedProb<-res
submission_log$PredictedProb[is.na(submission_log$PredictedProb)]<-mean(res,na.rm = T)

####feature selection####
temp_stat<-cor(temp)
temp_stat<-as.data.frame(temp_stat)
temp_stat=temp_stat[rowMeans(temp_stat,na.rm = T)==1]
temp_stat<-colnames(temp_stat)

train_num_cor<-train_num[,c('target',temp_stat)]
test_num_cor<-test_num[,temp_stat]

#omit na
train_num_cor_na_del<-na.omit(train_num_cor)

train_num_cor_na<-zoo::na.locf(train_num_cor)
test_num_cor_na<-zoo::na.locf(test_num_cor)

#decision tree
fit<-randomForest::randomForest(target~.,train_num_cor_na_del,ntree=20,mtry=10,do.trace=T)
res<-predict(fit,test_num_cor) #submission2

fit<-randomForest::randomForest(target~.,train_num_cor_na,ntree=30,mtry=5,do.trace=T)
res<-predict(fit,test_num_cor_na) #submission3