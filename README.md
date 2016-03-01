# DS-HA-KG
Data Science for Business Project
---

title: "HA - Data Science (modelling)"
output: html_document

---

Data are from:
<http://archive.ics.uci.edu/ml/datasets/Diabetes+130-US+hospitals+for+years+1999-2008#>
Reference research:
<http://www.hindawi.com/journals/bmri/2014/781670/>


```{r echo=FALSE, message=FALSE, warning=FALSE}
library(dummies)
library(data.table)
library(dplyr)
library(ggplot2)
library(ROCR)
library(pander)
library(Rmisc)
library(knitr)
library(h2o)
library(stats)
library(randomForest)
library(rpart)
library(gbm)

h2o.init(max_mem_size = "4g", nthreads = -1)
```

Exploratory Analysis
--------------------

### Read data and quick look
```{r echo=FALSE, message=FALSE, warning=FALSE}
# import data
# temp <- tempfile()
# download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00296/dataset_diabetes.zip",temp)
# dt <- read.csv(unz(temp,"dataset_diabetes/diabetic_data.csv"),dec = ".",na.strings="?",stringsAsFactors = F)
#unlink(temp)
dt <- read.csv("C:/Users/KG/Documents/CEU/DS proj/diabetic_data.txt",dec = ".",na.strings="?",stringsAsFactors = F)


df <- read.csv("C:/Users/KG/Documents/CEU/DS proj/diabetic_data.txt",dec = ".",na.strings="?",stringsAsFactors = T)


knitr::kable(summary(df))
knitr::kable(str(df))


pander(table(dt$readmitted, dt$A1Cresult=='None'))

dt %>% group_by(readmitted) %>% summarize(n = n()) %>% mutate(pc = n/sum(n)*100)

```

### Getting know main predictor variables


```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width = 10, fig.height = 10}
g0a<-ggplot(dt,aes(x=medical_specialty,fill=readmitted)) + geom_bar(position = 'fill')+coord_flip()+ ylab("%")+
    theme(legend.position="none")
g0b<-  ggplot(dt,aes(x=medical_specialty,fill=readmitted)) + geom_bar()+coord_flip()+    
  theme(axis.text.y=element_blank(),axis.title.y=element_blank())

multiplot(g0a,g0b,cols=2)
```

```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width = 10, fig.height = 5}

g1a<-ggplot(dt,aes(x=number_inpatient,fill=readmitted)) + geom_bar()
g1b<-ggplot(dt,aes(x=number_inpatient,fill=readmitted)) +
      geom_bar(position = 'fill') + ylab("%")
g2a<-ggplot(dt,aes(x=time_in_hospital,fill=readmitted)) + geom_bar()
g2b<-ggplot(dt,aes(x=time_in_hospital,fill=readmitted)) +
      geom_bar(position = 'fill') + ylab("%")
g3a<-ggplot(dt,aes(x=age,fill=readmitted)) + geom_bar()
g3b<-ggplot(dt,aes(x=age,fill=readmitted)) +
      geom_bar(position = 'fill') + ylab("%")
g4a<-ggplot(dt,aes(x=num_medications,fill=readmitted)) + geom_bar()
g4b<-ggplot(dt,aes(x=num_medications,fill=readmitted)) +
      geom_bar(position = 'fill') + ylab("%")
g5a<-ggplot(dt,aes(x=payer_code,fill=readmitted)) + geom_bar()
g5b<-ggplot(dt,aes(x=payer_code,fill=readmitted)) +
      geom_bar(position = 'fill') + ylab("%")

multiplot(g1a,g1b,g2a,g2b, cols=2)
multiplot(g3a,g3b,g4a,g4b, cols=2)
multiplot(g5a,g5b)

```


### Feature engeneering

I followed the rule of thumb to convert (cathegory) variables to dummies (NAs as well) but I also left the original data with the target dummy.

I used the cathegory info on on diagn data:

“circulatory” for icd9: 390–459, 785, “digestive” for icd9: 520–579, 787, “genitourinary” for icd9: 580–629, 788, “diabetes” for icd9: 250.xx, “injury” for icd9: 800–999, “musculoskeletal” for icd9: 710–739, “neoplasms” for icd9: 140–239, “respiratory” for icd9: 460–519, 786, and “other”

```{r echo=FALSE, message=FALSE, warning=FALSE}

dt <- dt %>% mutate(
    admission_type_id = as.character(admission_type_id),
    discharge_disposition_id = as.character(discharge_disposition_id),
    admission_source_id = as.character(admission_source_id),
    diag_1 = as.character(diag_1),
    diag_2 = as.character(diag_2),
    diag_3 = as.character(diag_3),
    d1_diabetes = as.numeric(diag_1 %like% '250'),
    d1_circulatory = as.numeric((diag_1>='390' & diag_1<='459') | diag_1 =='785'),
    d1_digestive = as.numeric((diag_1>='520' & diag_1<='579') | diag_1 =='787'),
    d1_genitourinary = as.numeric((diag_1>='580' & diag_1<='629') | diag_1 =='788'),
    d1_injury = as.numeric(diag_1>='800' & diag_1<='999'),
    d1_musculoskeletal = as.numeric(diag_1>='710' & diag_1<='739'),
    d1_neoplasms = as.numeric(diag_1>='140' & diag_1<='239'),
    d1_respiratory = as.numeric((diag_1>='460' & diag_1<='519') | diag_1 =='786'),
    d1_other = as.numeric(diag_1 > '999'),
    #
    d2_diabetes = as.numeric(diag_2 %like% '250'),
    d2_circulatory = as.numeric((diag_2>='390' & diag_2<='459') | diag_2 =='785'),
    d2_digestive = as.numeric((diag_2>='520' & diag_2<='579') | diag_2 =='787'),
    d2_genitourinary = as.numeric((diag_2>='580' & diag_2<='629') | diag_2 =='788'),
    d2_injury = as.numeric(diag_2>='800' & diag_2<='999'),
    d2_musculoskeletal = as.numeric(diag_2>='710' & diag_2<='739'),
    d2_neoplasms = as.numeric(diag_2>='140' & diag_2<='239'),
    d2_respiratory = as.numeric((diag_2>='460' & diag_2<='519') | diag_2 =='786'),
    d2_other = as.numeric(diag_2 > '999'),
    #
    d3_diabetes = as.numeric(diag_3 %like% '250'),
    d3_circulatory = as.numeric((diag_3>='390' & diag_3<='459') | diag_3 =='785'),
    d3_digestive = as.numeric((diag_3>='520' & diag_3<='579') | diag_3 =='787'),
    d3_genitourinary = as.numeric((diag_3>='580' & diag_3<='629') | diag_3 =='788'),
    d3_injury = as.numeric(diag_3>='800' & diag_3<='999'),
    d3_musculoskeletal = as.numeric(diag_3>='710' & diag_3<='739'),
    d3_neoplasms = as.numeric(diag_3>='140' & diag_3<='239'),
    d3_respiratory = as.numeric((diag_3>='460' & diag_3<='519') | diag_3 =='786'),
    d3_other = as.numeric(diag_3 > '999')
)    

dt$diag_1 <- NULL
dt$diag_2 <- NULL
dt$diag_3 <- NULL
dt$examide <- NULL
dt$citoglipton <- NULL
dt$encounter_id <- NULL
dt$patient_nbr <- NULL
dt$target<- as.numeric(dt$readmitted=='<30')
dt$readmitted <- NULL

dd <-dummy.data.frame(dt)

dd[is.na(dd)]<-0
summary(dd)
# randomForest likes factors more
ddf <- as.data.frame(lapply(dd[,names(dd)], factor))
ddf <- ddf %>% mutate(
  num_lab_procedures = as.numeric(num_lab_procedures),
  num_procedures = as.numeric(num_procedures),
  num_medications = as.numeric(num_medications),
  number_outpatient = as.numeric(number_outpatient),
  number_emergency = as.numeric(number_emergency),
  number_inpatient = as.numeric(number_inpatient),
  number_diagnoses = as.numeric(number_diagnoses)
)


```


### Modelling

I tried the following R modells:
linear regression (Lin), Decision tree (Dec), Random Forest (RF),
Gradient Boost Method  (GBM) with some parameters. After this I tried out H2O models as well.

```{r echo=FALSE, message=FALSE, warning=FALSE}

set.seed(123)
N <- nrow(dt)
idx_train <- sample(1:N,N/2)
idx_valid <- sample(base::setdiff(1:N, idx_train), N/4)
idx_test <- base::setdiff(base::setdiff(1:N, idx_train),idx_valid)

# from df1?
d_train <- dt[idx_train,]
d_valid <- dt[idx_valid,]
d_test  <- dt[idx_test,]

dftr <- ddf[idx_train,]
dfte <- ddf[idx_test,]

idx_prob <- sample(1:N,N/10)
df1 <- ddf[idx_prob,]
N <- nrow(df1)
idx <- sample(1:N, 0.6*N)
p_train <- df1[idx,]
p_test <- df1[-idx,]

```


GLM

```{r echo=FALSE, message=FALSE, warning=FALSE}

# glm needs dummies
lin <- glm(target~.,dd1,family=gaussian())

pandar(summary(lin))

pred_lin <- prediction(fitted(lin, dd1),dd1$target)

# ROC
plot(performance(pred_lin,"tpr","fpr"), main="ROC curve - Lin")
# lift chart
plot(performance(pred_lin,"lift","rpp"), main="lift curve")
#AUC
as.numeric(performance(pred_lin,"auc")@y.values)

```


Random Forest

```{r echo=FALSE, message=FALSE, warning=FALSE}

# RF needs factors
rf <- randomForest(target~., data=p_train, ntree = 100, importance=T)
varImpPlot(rf, type = 2)

pred_rf <- prediction(predict(rf, p_test, type = "prob")[,"1"],p_test$target)

hist((predict(rf, p_test, type = "prob"))[,"1"])

# ROC
plot(performance(pred_rf,"tpr","fpr"), main="ROC curve")
# lift chart
plot(performance(pred_rf,"lift","rpp"), main="lift curve")
#AUC
as.numeric(performance(pred_rf,"auc")@y.values)

```


Decision tree

```{r echo=FALSE, message=FALSE, warning=FALSE}

# decision tree sima fa

dect <- rpart(target ~ ., data = p_trin, control = rpart.control(cp = 0))

dect$variable.importance

plot(dect, uniform = TRUE, compress = TRUE)

pred_dec <- prediction(predict(dect, p_test)[,"1"], p_test$target)
# ROC
plot(performance(pred_dec,"tpr","fpr"), main="ROC curve")
# lift chart
plot(performance(pred_dec,"lift","rpp"), main="lift curve")
#AUC
as.numeric(performance(pred_dec,"auc")@y.values)

```


GBM

```{r echo=FALSE, message=FALSE, warning=FALSE}

gbm1 <- gbm(target ~ ., data = p_train, distribution = "bernoulli",
          n.trees = 100, interaction.depth = 10, shrinkage = 0.01, cv.folds = 5)
  
#best.iter <- gbm.perf(gbm1,method="OOB")
#(gbm1,method="cv")
#print(best.iter)

pred_gbm <- prediction(predict(gbm1,p_test,100),p_test$target)
# ROC
plot(performance(pred_gbm,"tpr","fpr"), main="ROC curve")
# lift chart
plot(performance(pred_gbm,"lift","rpp"), main="lift curve")
#AUC
as.numeric(performance(pred_gbm,"auc")@y.values)



```

### Modelling with H2O

```{r echo=FALSE, message=FALSE, include=FALSE, cache=FALSE}

idx_prob <- sample(1:N,N/10)
df1 <- ddf[idx_prob,]
N <- nrow(df1)
idx <- sample(1:N, 0.6*N)
p_train <- df1[idx,]
p_test <- df1[-idx,]
str(p_train[280:288])
str(p_train)

h_train <- as.h2o(p_train)
h_valid <- as.h2o(d_valid)
h_test <- as.h2o(p_test)

```

### Random forest

```{r echo=FALSE, message=FALSE, include=FALSE, cache=FALSE}

system.time({
  hrf1 <- h2o.randomForest(x = 1:287, y = 288, 
            training_frame = h_train, 
            mtries = -1, ntrees = 500, max_depth = 20, nbins = 200)
})

hrf1
h2o.auc(hrf1) 
h2o.auc(h2o.performance(hrf1, h_test))

```



