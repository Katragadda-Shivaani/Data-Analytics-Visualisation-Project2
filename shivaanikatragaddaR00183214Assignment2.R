#R SCRIPT
#STUDENT NAME : SHIVAANI KATRAGADDA
#STUDENT NUMBER : R00183214
#SUBJECT : DATA VISUALISATION 
#ASSIGNMENT : 2
#PROFESSOR : ANGEUS DALY
#DATE : 16-05-2020
#**********************************************************************************

# installing and loading all the required packages
install.packages("readxl")
library(readxl)

install.packages("DataExplorer")
library(DataExplorer)

install.packages("dplyr")
library(dplyr)

install.packages("psych")
library(psych)

install.packages("caret")
library(caret)

install.packages("skimr")
library(skimr)

install.packages("RANN")
library(RANN)

install.packages("visdat")
library(visdat)

install.packages("funModeling")
library(funModeling)

install.packages("Hmisc")
library(Hmisc)

install.packages("inspectdf")
library(inspectdf)

install.packages("ggplot2")
library(ggplot2)

install.packages("corrplot")
library(corrplot)

install.packages("ggcorrplot")
library(ggcorrplot)

install.packages("GGally")
library(GGally)

install.packages("DMwR")
library(DMwR)
#***************************************************************************
#LOADING THE TRAINING DATASET

Train<-read_excel("F:/semester2/DA&V/DV2/RF_TrainingDatasetA_Final.xlsx")
View(Train)
#checking class of dataset
class(Train)
#Converting to data frame
Train<- as.data.frame(Train)
View(Train)
class(Train)

#*****************************************************************************

#QUESTION A

#Investigate the data by carrying out some exploratory data analysis (EDA). Perform the
#necessary data cleaning/data reduction tasks and outline how you do this in R. Word count 750.

# SOLUTION

# Performing EXPLANATORY DATA ANALYSIS
#Knowing about the numerica summary of data by using dim,hed,tail,names,str,summary,glimpse,sapply,
#df_status,describe,skim
dim(Train)
names(Train)
head(Train)
tail(Train)
str(Train)
summary(Train)
glimpse(Train)

#sapply returns vector or matrix instead of list object
sapply(Train, class)
df_status(Train)
plot_num(Train)
profiling_num(Train)
describe(Train)#describes about the dataset
skim(Train)
#The skim function is a good addition to the summary function.  It displays most of the numerical attributes from summary, but it also displays missing values, more quantile information and an inline histogram for each variable!
any(duplicated(Train))
#checking for duplicates
which(duplicated(rownames(Train)))
which(duplicated(colnames(Train)))

#Knowing about the data by using plots and tables

introduce(Train)#tells about the columns 

plot_str(Train)#plots the graphs by showing all the columns 

plot_intro(Train)#gives information about the columns and rows

plot_missing(Train)#gives information about missing columns data

profile_missing(Train)

#tells about the missing data by usig graphs 
vis_miss(Train)
vis_dat(Train)

#tells about the missing values
b1<-inspect_na(Train)
show_plot(b1)

#***************

#Visualisation of dataset by using inspectdf package
#tells about the type of data
a<-inspect_types(Train)
show_plot(a)
#tells about the columns and size
b<-inspect_mem(Train)
show_plot(b)
#inspect numerical data
b2<-inspect_num(Train)
show_plot(b2)
#inspect categorical data
b4<-inspect_cat(Train)
show_plot(b4)
#considering some columns 
z=Train[,c("EffectiveFadeMargindB1","EffectiveFadeMargindB2","EIRPdBm1","EIRPdBm2","Elevation2","Elevationm1",
           "ERPdbm1")]
#inspects the correlation
b5<-inspect_cor(z)
show_plot(b5)

#**************
#Visualisation of dataset by using DataExplorere package package
#histograms of numerical data
plot_histogram(Train)
# bar plot of categorical data
plot_bar(Train)
#density plot of numerical data
plot_density(Train)
# boxplot
plot_boxplot(Train, by = "AntennagaindBd1")
plot_boxplot(Train, by = "AntennagaindBi2")
plot_boxplot(Train, by = "ERPwatts1")
plot_boxplot(Train, by = "Pathinclinationmr")
plot_boxplot(Train, by = "FrequencyMHz")
plot_boxplot(Train, by = "Verticalangle1")
#Scatterplot
plot_scatterplot(Train,by = "Antennamodel1")
plot_scatterplot(Train,by = "Trueazimuth1")
plot_scatterplot(Train,by = "Elevation2")
plot_scatterplot(Train,by = "OtherTXlossdB2")
#qq plot
qq_data <-Train[, c("AntennagaindBd1","AntennagaindBd2","AntennagaindBi1","AntennagaindBi2","Antennaheightm1","Antennaheightm2","AtmosphericabsorptionlossdB","AverageannualtemperatureC","CirculatorbranchinglossdB1","CirculatorbranchinglossdB2","dbperKmRatio","DiffractionlossdB",
                    "Dispersivefadeoccurrencefactor","EffectiveFadeMargindB1","EffectiveFadeMargindB2","EIRPdBm1","EIRPdBm2","Elevation2","Elevationm1",
                    "ERPdbm1","ERPdbm2","ERPwatts1","ERPwatts2","FadeoccurrencefactorPo","FlatfademarginmultipathdB1","FlatfademarginmultipathdB2","FreespacelossdB","FrequencyMHz","Geoclimaticfactor","MainnetpathlossdB1","MainnetpathlossdB2","MainreceivesignaldBm1","MainreceivesignaldBm2","OtherRXlossdB1","OtherRXlossdB2",
                    "OtherTXlossdB1","OtherTXlossdB2","Pathinclinationmr","Pathlengthkm","ThermalFadeMargindB1","R_Powerfd1","R_Powerfd2","ThermalFadeMargindB2","Trueazimuth1","Trueazimuth2","TXpowerdBm1","TXpowerdBm2","DpQ_R2","Verticalangle1","Verticalangle2","XPDfademarginmultipathdB1","XPDfademarginmultipathdB2","Fullmaxt1","Fullmint1",
                    "RXthresholdleveldBm1", "RXthresholdleveldBm2","RXthresholdlevelv1","RXthresholdlevelv2")]

plot_qq(qq_data)#plotting qq plot for qq_data

plot_qq(qq_data, by = "RXthresholdleveldBm1")
plot_qq(qq_data, by = "FreespacelossdB")
plot_qq(qq_data, by = "Geoclimaticfactor")
plot_qq(qq_data, by = "Pathlengthkm")
plot_qq(qq_data, by = "R_Powerfd1")

#correlation
plot_correlation(Train)

####***********************
#GGPLOT2
#ggplot for Antennamodel1,Antennaheightm1
ggplot(data=Train,aes(x=Antennamodel1,y=Antennaheightm1)) + 
   geom_bar(stat ='identity',aes(fill=Antennaheightm1))+
   coord_flip() + 
   theme_grey() + 
   scale_fill_gradient(name="Antennaheightm1")+
   labs(title = 'Antennaheightm1 vs Antennamodel1',
        y='Antennaheightm1',x='Antennamodel1')+ 
   geom_hline(yintercept = mean(Train$Antennaheightm1),size = 1, color = 'blue')

#ggplot for Polarization,Eng_Class
ggplot(data = Train, aes(x=Polarization,y=Eng_Class, fill=Polarization)) + 
   geom_boxplot()+
   scale_color_brewer(palette="Dark2") + 
   geom_jitter(shape=16, position=position_jitter(0.2))+
   labs(title = 'Polarization vs Eng_Class ',
        y='Eng_Class',x='Polarization')

#ggplot for verticalangel1 and its mean
ggplot(data=Train,mapping= aes(x=Verticalangle1,colour=Eng_Class)) +
   geom_freqpoly(binwidth=0.1)+
   theme_light()+ geom_hline(yintercept = mean(Train$Verticalangle1), size=1, color="black") +
   scale_fill_gradient() +
   labs(title="Verticalangle1 and Eng_Class", x="Verticalangle1", y="% mean(Train$Verticalangle1)")
#ggplot for RXthresholdcriteria1 and Eng_Class
ggplot(Train, aes(x =  RXthresholdcriteria1, fill = Eng_Class)) +
   geom_bar()
#ggplot for RXthresholdcriteria1 and Polarization
ggplot(Train, aes(x = RXthresholdcriteria1, color =Polarization)) +
   geom_density(size = 2)
#ggplot for RXthresholdcriteria1 and R_Powerfd1
ggplot(Train, aes(x = RXthresholdcriteria1, y =R_Powerfd1 , color = Outcome)) +
   geom_point(size = 3) +
   ylim(100, 0)
#ggplot for Eng_Class, Verticalangle1
ggplot(data = Train) +
   geom_bin2d(mapping = aes(y = Eng_Class, x = Verticalangle1))

#selecting columns randomly
df = Train[,c(5,6,7,8,9,10)]
#correlation of df
res = cor(df)
#ggcorrplot for the df
ggcorrplot(res, type = "lower", outline.col = "black",
           lab=TRUE,
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

ggcorrplot(res, type = "lower", outline.col = "black",
           method="circle",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))
#corrplot
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


#*********************
#ggpairs
#The  ggpairs() function produces a matrix of scatter plots for visualizing the correlation between variables

ggpairs(df)#Make a matrix of plots with Train data set


#*****************************
#DATA CLEANING
# converting the categorical data to factors
Train<-mutate_if(Train,is.character,as.factor)
glimpse(Train)
sum(is.na(Train))

#printing the complete cases in the data
cat('The Complete cases in the dataset are : ', sum(complete.cases(Train)))

#printing the total number of null values present in the dataset
cat('Total null values in the Train dataset are : ' , sum(is.na(Train)))

#printing the name of the column with null values
list_na <- colnames(Train)[apply(Train, 2, anyNA)]
cat('Columns with null values : ',list_na)

#Data Preparation and Preprocessing
#Last three digits of my student number is used to set the seed
set.seed(214)
#impute missing values using preProcess
preProcValues <- preProcess(Train, method = c("knnImpute"))
preProcValues
train_processed <- predict(preProcValues, Train)
#checking for missing values after imputation
sum(is.na(train_processed))
plot_missing(train_processed)

#filling NA for categorical data by using mode
table(is.na(train_processed$Outcome))#checking how many misssig values are there in the column

sort(table(train_processed$Outcome))#sorting the column in order to get the total number of observation for each domain in the column in ascending order

names(table(train_processed$Outcome))[table(train_processed$Outcome)==max(table(train_processed$Outcome))] #now taking the domain which is having the high value     

train_processed$Outcome[is.na(train_processed$Outcome)] <- "Y"#Now assigining the highest value to the missing rows in the column

table(is.na(train_processed$Outcome))#printing the table

plot_missing(train_processed)#plotting the missing data graph to check whether any missing values is present

#DATA REDUCTION

#How to create One-Hot Encoding (dummy variables)
#Converting Eng_Class variable to numeric
train_processed$Eng_Class<-ifelse(train_processed$Eng_Class=='under',0,1)

#Checking the structure of processed train file
str(train_processed)

#Removing RFDBid,Antennafilename1,Antennafilename2 columns.
train_processed$RFDBid<-NULL
train_processed$Antennafilename1<-NULL
train_processed$Antennafilename2<-NULL
dim(train_processed)

#Creating Dummy Variables
dmy <- dummyVars(" ~ .", data = train_processed,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))
View(train_transformed)
dim(train_transformed)
str(train_transformed)
tail(train_transformed)

# Remove Zero and Near Zero-Variance Predictors
nzv <- nearZeroVar(train_transformed, saveMetrics = TRUE)
nzv$nzv
nzv[nzv$nzv,][1:10,]
dim(train_transformed)

nzv <- nearZeroVar(train_transformed)
dat2 <- train_transformed[, -nzv]
dim(dat2)
str(dat2)


# Identifying numeric variables
numericData <- dat2[sapply(dat2, is.numeric)]
str(numericData)
dim(numericData)

# Calculate correlation matrix
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)
dim(descrCor)
summary(descrCor[upper.tri(descrCor)])

# Check Correlation Plot
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.70)

# print indexes of highly correlated attributes
print(highlyCorrelated)

# Indentifying Variable Names of Highly Correlated Variables
highlyCorCol <- colnames(numericData)[highlyCorrelated]

# Print highly correlated attributes
highlyCorCol

# Remove highly correlated variables and create a new dataset
dat3 <- dat2[, -which(colnames(dat2) %in% highlyCorCol)]
dim(dat3)
str(dat3)
#converting the data to factors again
dat3$Eng_Class<-ifelse(dat3$Eng_Class==0,'under','okay')
dat3$Eng_Class<-as.factor(dat3$Eng_Class)

#checking for duplicates in dat3
which(duplicated(rownames(dat3)))
which(duplicated(colnames(dat3)))

#feature plots
featurePlot(x = dat3[1:10], y = dat3$Eng_Class, plot = 'pairs', auto.key = list(column = 2))
featurePlot(x = dat3[, 2:10],y = dat3$Eng_Class, plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))
featurePlot(x = dat3[, 11:20], 
            y = dat3$Eng_Class, 
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))


#***********************************************************************************


#QUESTION B

#Set up a training/testing methodology. Using a least 2 models, tune these models and
#compare your results. Give your best model and comment.

# SOLUTION

#MODEL BUILDING
#Spliting training set into two parts based on outcome: 70% and 30%

train.model.ind <- createDataPartition(dat3$Eng_Class, p = 0.7, list = FALSE)
#training
train.model <- dat3[train.model.ind,]
str(train.model)
#testing
test.model <- dat3[-train.model.ind,]
str(test.model)

#class imbalances
#library(DMwR)
set.seed(214)
#class imbalance for dat3
smote_train <- SMOTE(Eng_Class ~ ., data  = dat3)                         
table(smote_train$Eng_Class) 

#class imbalance for train.model

smote_train <- SMOTE(Eng_Class ~ ., data  = train.model)                         
table(smote_train$Eng_Class) 

#class imbalance for test.model

smote_train <- SMOTE(Eng_Class ~ ., data  = test.model)                         
table(smote_train$Eng_Class) 

# See available algorithms in caret
modelnames <- paste(names(getModelInfo()), collapse=',  ')
modelnames

#THREE MODELS
#1.KNN
#2.SVMLINEAR
#3.RANDOM FOREST

# setting conditions for trainControl
set.seed(214)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary,
                           savePredictions = TRUE,
                           sampling="smote")

#*********************************
#1.KNN
modelLookup('knn')

# Set the seed for reproducibility
set.seed(214)

# Train the model using knn and predict on the training data itself.
#Basic Parameter Tuning
KNN_basic_tuning = train(Eng_Class ~ ., data = train.model,method='knn',trControl=fitControl,metric="ROC",preProc = c("center", "scale"))
KNN_basic_tuning 

#summary of the model
summary(KNN_basic_tuning )
plot(KNN_basic_tuning , main="Model Accuracies with KNN")

#variable importance of the model
varimp <- varImp(KNN_basic_tuning )
plot(varimp, main="Variable Importance with KNN")

#predicting by using test data and the confusion matrix
knn_predict_basic <- predict(KNN_basic_tuning ,test.model)
confusionMatrix(test.model$Eng_Class,knn_predict_basic)

#Alternate Tuning Grids
#setting seed value
set.seed(214)
#defining the grid
grid=expand.grid(k =1:7)
#settig the seed value
set.seed(214)
#building the model
KNN_alternate_tuning = train(Eng_Class ~ ., data = train.model, method='knn', metric='ROC', tuneGrid = grid, trControl = fitControl)
KNN_alternate_tuning

#summary of the model
summary(KNN_alternate_tuning)
plot(KNN_alternate_tuning , main="Model Accuracies with KNN")

#variable importance of the model
varimp1 <- varImp(KNN_alternate_tuning )
plot(varimp1, main="Variable Importance with KNN")

#predicting by using test data and the confusion matrix
knn_predict_alternate <- predict(KNN_alternate_tuning, test.model)
confusionMatrix(test.model$Eng_Class,knn_predict_alternate)

#**********************************
#2.SVMLINEAR

modelLookup('svmLinear')
#setting the seed value
set.seed(214)
#Basic Parameter Tuning
svm_basic_tuning <- train(Eng_Class ~ ., data = train.model, 
                   method = "svmLinear", 
                   metric = "ROC",
                   trControl = fitControl,
                   verbose = FALSE,preProc = c("center", "scale"))
svm_basic_tuning 

#summary of the model
summary(svm_basic_tuning)

#variable importance of the model
varimp2 <- varImp(svm_basic_tuning )
plot(varimp2, main="Variable Importance with SVMLINEAR")

#predicting by using test data and the confusion matrix

svm_predict_basic <- predict(svm_basic_tuning, test.model)
confusionMatrix(test.model$Eng_Class,svm_predict_basic)

#Alternate Tuning Grids
set.seed(214)
svm_alternate_tuning <- train(Eng_Class ~., data = train.model,
                              method = "svmLinear",
                         trControl=fitControl,
                         metric = "ROC",
                         verbose = FALSE,
                         tuneGrid=expand.grid(C = c(1,2,3,10)))

svm_alternate_tuning 

#summary of the model

summary(svm_alternate_tuning)
plot(svm_alternate_tuning , main="Model Accuracies with SVMLINEAR")
#variable importance of the model
varimp3 <- varImp(svm_alternate_tuning )
plot(varimp3, main="Variable Importance with SVMLINEAR")

#predicting by using test data and the confusion matrix

svm_predict_alternate<-predict(svm_alternate_tuning,test.model)
confusionMatrix(test.model$Eng_Class,svm_predict_alternate)

#************************
#3.RANDOM FOREST
set.seed(214)
#Model basic tuning

rf_basic_tuning <- train(Eng_Class ~ ., data = train.model, 
                  method = "rf", 
                  ## Specify which metric to optimize, by default, this is the accuracy
                  metric = "ROC",
                  trControl = fitControl,
                  verbose = FALSE)
rf_basic_tuning

#summary of the model

summary(rf_basic_tuning)
plot(rf_basic_tuning, main="Model Accuracies with RF")

#variable importance of the model
varimp5 <- varImp(rf_basic_tuning)
plot(varimp5, main="Variable Importance with RF")

#predicting by using test data and the confusion matrix
rf_basic_predict <- predict(rf_basic_tuning, test.model)
confusionMatrix(test.model$Eng_Class,rf_basic_predict)

#model alternate tuning

# setting conditions for trainControl
set.seed(214)
# setting different values of mtry for the model 
man_grid <-  expand.grid(mtry = c(1:15))
# doing the grid search                                                              
set.seed(214)
# building the model                                  
rf_alternate_tuning <- train(Eng_Class ~ ., data = train.model, 
                       method = "rf", 
                       ## Specify which metric to optimize, by default, this is the accuracy
                       metric = "ROC",
                       trControl = fitControl,
                       verbose = FALSE,
                       tuneGrid = man_grid)

rf_alternate_tuning
plot(rf_alternate_tuning, xlab = 'mtry')
#Best tune of the model
rf_alternate_tuning$bestTune    # mtry = 10
set.seed(214)

# building the model
# build the model by selecting the mtry given by the grid searches (mtry = 10)
hyperparams <- expand.grid(mtry=10)

set.seed(214)
rf_alternate_tuning1 <- train(Eng_Class ~ ., data = train.model, 
                       method = 'rf', 
                       tuneGrid = hyperparams,
                       metric = "ROC",
                       trControl = fitControl,
                       verbose = F)
rf_alternate_tuning1

#summary of the model

summary(rf_alternate_tuning1)

#variable importance of the model
varimp6 <- varImp(rf_alternate_tuning1)
plot(varimp6, main="Variable Importance with RF")

#predicting by using test data and the confusion matrix

rf_alternate_predict <- predict(rf_alternate_tuning1, test.model)
confusionMatrix(test.model$Eng_Class,rf_alternate_predict)

#************************************************************************************

#QUESTION C

#Perform feature selection on your model in c). Explain how you do this, giving a rational and
#comment on your results.

# SOLUTION 

#FEATURE SELECTION FOR THE BEST MODEL
#setting the seed
set.seed(214)
#defining the rfecontrol() function
rctrl1 <- rfeControl(method = "repeatedcv",
                     number = 10,
                     repeats=3,
                     returnResamp = "all",
                     functions = rfFuncs,
                     saveDetails = TRUE,verbose = FALSE)
#setting the seed value
set.seed(214)
#performing the feature selection using random forest method
model <- rfe(Eng_Class ~., data = train.model,
             method = "rf",
             trControl = trainControl(method = "cv",
                                      classProbs = TRUE),
             rfeControl = rctrl1)
#printing the model
model
#priting the total number of features
predictors(model)
#printing the model fir
model$fit
#printing the  head model$sample
head(model$resample)
#plotting the graph for the model
trellis.par.set(caretTheme())
plot(model, type = c("g", "o"))

#building the model by considering all the features after performing feature selection by using random forest method.
rf_feature_selection <- train(Eng_Class ~Fullmaxt1+DpQ_R2+MainnetpathlossdB1+Fullmint1+TXpowerdBm2+ERPwatts2+
                                 ERPdbm2+Emissiondesignator2.28M00D7WET+AtmosphericabsorptionlossdB+Emissiondesignator2.56M00D7WET+FrequencyMHz+Polarization.Vertical+AntennagaindBd2+OtherTXlossdB2+Geoclimaticfactor+Emissiondesignator1.28M6G7W, data = train.model, 
                              method = 'rf', 
                              tuneGrid = hyperparams,
                              metric = "ROC",
                              trControl = fitControl,
                              verbose = F)
rf_feature_selection
#summary of the model
summary(rf_feature_selection)

#variable importance of the model
varimp7 <- varImp(rf_feature_selection)
plot(varimp7, main="Variable Importance with RF")

#predicting by using test data and the confusion matrix
rf_feature_predict <- predict(rf_feature_selection, test.model)
confusionMatrix(test.model$Eng_Class,rf_feature_predict)

#**********************************************************************************

#QUESTION D

#For your best model explain to Daniel how this model works. Give and explain the cost/loss
#function used in your modelling. Word count 750.

# SOLUTION

# stastics of rf_alternate_tuning final model 
rf_alternate_tuning$finalModel
#The OOB estimate of  error rate is 7.14%

#PLEASE LOOK IN TO THE REPORT

#***********************************************************************************

#QUESTION E

#Daniel is primarily concerned with finding the under engineered masts as these are the ones
#that cause outages, so incorrectly 'scoring' a mast as under when is it okay is not as bad as
#incorrectly 'scoring' a mast as okay when it is under; you can take the ratio here of
#misclassification 'costs' as 1:h, where h = {8, 16, 24}, i.e. h can take a value of 8, 16 or 24.
#Redo your modelling using your best model above and comment on your new results.

# SOLUTION

#missclassification

#misclassification error on test data
(tab <- table(test.model$Eng_Class,rf_feature_predict))
1-sum(diag(tab))/sum(tab)
#the miss classification is 0.09
(tab <- table(test.model$Eng_Class,rf_alternate_predict))
1-sum(diag(tab))/sum(tab)
#the miss classification is 0.1038

#h={8,16,24}.
#I will be consediring value24 here

# setting conditions for trainControl
set.seed(214)
# setting different values of mtry for the model 
man_grid1 <-  expand.grid(mtry = c(1:24))
# doing the grid search                                                              
set.seed(214)
# building the model                                  
rf_tuning <- train(Eng_Class ~ ., data = train.model, 
                             method = "rf", 
                             ## Specify which metric to optimize, by default, this is the accuracy
                             metric = "ROC",
                             trControl = fitControl,
                             verbose = FALSE,
                             tuneGrid = man_grid1)

rf_tuning
plot(rf_tuning, xlab = 'mtry')
rf_tuning$bestTune    # mtry = 6

set.seed(214)
hyperparams1 <- expand.grid(mtry=6)

set.seed(214)
rf_alternate_tuning2 <- train(Eng_Class ~ ., data = train.model, 
                              method = 'rf', 
                              tuneGrid = hyperparams1,
                              metric = "ROC",
                              trControl = fitControl,
                              verbose = F)
rf_alternate_tuning2
summary(rf_alternate_tuning2)

varimp7 <- varImp(rf_alternate_tuning2)
plot(varimp7, main="Variable Importance with RF")


rf_predict <- predict(rf_alternate_tuning2, test.model)
confusionMatrix(test.model$Eng_Class,rf_predict)

(tab <- table(test.model$Eng_Class,rf_predict))
1-sum(diag(tab))/sum(tab)
#the miss classification is 0.1053

#*************************************************************************************
#QUESTION F
 
#Using the scoring data set provided predict whether these radio masts will be okay or under
#engineered using your best model to part d) and comment.

# SOLUTION 

#Loading the scoring dataset
score<-read_excel("F:/semester2/DA&V/DV2/RF_ScoringDatasetA_Final.xlsx")
#checking the clas of scoring dataset
class(score)
#converting in to data frame
score<-as.data.frame(score)
#converting all categorical data into factors
score<-mutate_if(score,is.character,as.factor)
#structure of score 
str(score)

## setting seed
set.seed(214)
#checking the dimensions of score dataset
dim(score)
glimpse(score)

#Remove from dataframe as they will not be used in classification
score[c("Antennafilename1", "Antennafilename2", "RFDBid")] <- list(NULL)

# Remove Zero and Near Zero-Variance Predictors
nzv1 <- nearZeroVar(score)
#Remove near zero variance from the score dataframe and create a new dataframe called score 3
score1 <- score[, -nzv1]
#Look at the dimensions of the model now it has decreased to 61 variables
dim(score1)

#Converting every categorical variables to numerical using dummy variables
dum2 <- dummyVars(" ~ .", data = score1,fullRank = T)

# Convert to dataframe and Create the dummy variables using predict.
score_trans <- data.frame(predict(dum2, newdata = score1))

#setting seed value
set.seed(214)
#predicting for scoring dataset
pred_score<-predict.train(rf_alternate_tuning1,newdata=score_trans,type="raw")
table(pred_score)
pred_score#printing the predicted values of scoring data

#*********************************************************************************
#QUESTION G

# ANSWERED G(i)  and G(iii)TEXT ANALYTICS

#PLEASE LOOK INTO THE REPORT

#************************************************



