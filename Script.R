########################################################
#                                                      #
#               Loading packages                       #
#                                                      #
########################################################

if(!require(tinytex)) install.packages("tinytex")
if(!require(float)) install.packages("float")
if(!require(caret)) install.packages("caret")
if(!require(data.table)) install.packages("data.table")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(lubridate)) install.packages("lubridate")
if(!require(stringr)) install.packages("stringr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(GGally)) install.packages("GGally")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(stringr)) install.packages("stringr")
if(!require(lattice)) install.packages("lattice")
if(!require(rpart)) install.packages("rpart")
if(!require(knitr)) install.packages("knitr")
if(!require(rmarkdown)) install.packages("rmarkdown")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(corrplot)) install.packages("corrplot")
if(!require(reshape2)) install.packages("reshape2")
if(!require(dslabs)) install.packages("dslabs")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(randomForest)) install.packages("randomForest")
if(!require(Rborist)) install.packages("Rborist")
if(!require(tree)) install.packages("tree")
if(!require(devtools)) install.packages("devtools", type = "win.binary")
if(!require(plotrix)) install.packages("plotrix")
if(!require(reprtree)) install.packages("reprtree")

library(tinytex)
library(float)
library(caret)
library(data.table)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(GGally)
library(ggthemes)
library(stringr)
library(lattice)
library(rpart)
library(knitr)
library(rmarkdown)
library(gridExtra)
library(ggrepel)
library(corrplot)
library(reshape2)
library(dslabs)
library(rpart.plot)
library(randomForest)
library(Rborist)
library(tree)
library(devtools)
library(plotrix)
library(reprtree)


########################################################
#                                                      #
#               Loading dataset                        #
#                                                      #
########################################################

dataset<- read.csv("https://github.com/Paul-Ndn/Diabetes_Project/blob/2e6b49bb9a48b2763da0dd9f9b3f74744bbb3b9a/diabetes.csv", sep = ",")


#####################################################
#                                                   #
#   Part I - Data structure and data reprocessing   #
#                                                   #
#####################################################

# Chapter 1 - Analyse the data structure
  ## We will observe the structure of the data set
dataset<- as_tibble(dataset)

str(dataset)
head(dataset)

save(dataset, file = "object_dataset.Rdata")

  ## We will make sure the the edx data set haven't NA's values
apply(dataset, 2, function(x) {
  any(is.na(x))
})

# Chapter 2 - Data reprocessing

  ## We will create a new data frame, whose name is "dataset_reprocessing" in order to save our reprocessing
dataset_reprocessing<- dataset

  ## We will reprocess the "Pregnancies" variable

    ### We are looking for inconsistent data by making a box plot
dataset %>% ggplot(aes(Pregnancies)) + geom_boxplot(outlier.color = "red") + theme_classic()

  ## We will reprocess the "Glucose" variable

    ### We are looking for inconsistent data by making a box plot
dataset %>% ggplot(aes(Glucose)) + geom_boxplot(outlier.color = "red") + theme_classic()

    ### We will highlight the observations which correspond to the outlier
dataset[which(dataset$Glucose == boxplot.stats(dataset$Glucose)$out),]

    ### We will replace 0 by NA
dataset_reprocessing$Glucose[dataset_reprocessing$Glucose == 0]<- NA

  ## We will reprocess the "bloodpressure" variable
    
      ### We are looking for inconsistent data by making a box plot
dataset %>% ggplot(aes(BloodPressure)) + geom_boxplot(outlier.color = "red") + theme_classic()

    ### We will highlight the observations which correspond to the outlier
dataset[which(dataset$BloodPressure == boxplot.stats(dataset$BloodPressure)$out),]

    ### We will replace 0 by NA
dataset_reprocessing$BloodPressure[dataset_reprocessing$BloodPressure == 0]<- NA

  ## We will reprocess the "SkinThickness" variable

    ### We are looking for inconsistent data by making a box plot
dataset %>% ggplot(aes(SkinThickness)) + geom_boxplot(outlier.color = "red") + theme_classic()

    ### We will highlight the observations which correspond to the outlier
dataset[which(dataset$SkinThickness == boxplot.stats(dataset$SkinThickness)$out),]

    ### We will replace 0 by NA
dataset_reprocessing$SkinThickness[dataset_reprocessing$SkinThickness == 0]<- NA

  ## We will reprocess the "insulin" variable

    ### We are looking for inconsistent data by making a box plot
dataset %>% ggplot(aes(Insulin)) + geom_boxplot(outlier.color = "red") + theme_classic()

    ### We will highlight the observations which correspond to the outlier
dataset[which(dataset$Insulin == boxplot.stats(dataset$Insulin)$out),]

    ### We will replace 0 by NA
dataset_reprocessing$Insulin[dataset_reprocessing$Insulin == 0]<- NA

  ## We will reprocess the "BMI" variable

    ### We are looking for inconsistent data by making a box plot
dataset %>% ggplot(aes(BMI)) + geom_boxplot(outlier.color = "red") + theme_classic()

    ### We will highlight the observations which correspond to the outlier
dataset[which(dataset$BMI == boxplot.stats(dataset$BMI)$out),]

    ### We will replace 0 by NA
dataset_reprocessing$BMI[dataset_reprocessing$BMI == 0]<- NA

  ## We will reprocess the "DiabetesPedigreeFunction" variable

    ### We are looking for inconsistent data by making a box plot
dataset %>% ggplot(aes(DiabetesPedigreeFunction)) + geom_boxplot(outlier.color = "red") + theme_classic()

  ## We will reprocess the "age" variable

    ### We are looking for inconsistent data by making a box plot
dataset %>% ggplot(aes(Age)) + geom_boxplot(outlier.color = "red") + theme_classic()

  ## We will reprocess the "outcome" variable

    ### We will tranform the data of the "outcome" variable into factor value in order to use classification model
dataset<- dataset %>% mutate(Outcome= as.factor(Outcome))
dataset_reprocessing<- dataset_reprocessing %>% mutate(Outcome= as.factor(Outcome))


invisible(invisible(gc())) # for cleaning memory

#####################################################
#                                                   #
#          Part II - Data visualization             #
#                                                   #
#####################################################

# Chapter 1 - Pregnancies data visualization

  ## We will analyse the principal statistical data of the "pregnancies" variable

as.data.frame(quantile(dataset_reprocessing$Pregnancies, c(0.1, .25, 0.5, 0.75, .9, .95))) # Evaluate some quantiles and deciles 

data.frame(min.= round(min(dataset_reprocessing$Pregnancies),2), median= round(median(dataset_reprocessing$Pregnancies),2),
           mean= round(mean(dataset_reprocessing$Pregnancies),2),
           standard_deviation= round(sd(dataset_reprocessing$Pregnancies),2), max.= round(max(dataset_reprocessing$Pregnancies),2)) # Evaluate min, max, median, mean and standard deviation

  ## we will represent a density plot of the number of pregnancies

x_plot<- dataset_reprocessing %>% ggplot(aes(Pregnancies)) + geom_density(kernel= "optcosine", adjust= 1.5, fill= "blue", alpha= .1) + 
  scale_x_continuous(breaks = seq(0,17,2)) +
  labs(title = "Fig. 1.1 - Density curve of the number of pregnancies",x= "Number of pregnancies", y= "Frequency") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

  ## We will plot an histogram of the number of pregnancies
x<- dataset_reprocessing %>% group_by(Pregnancies) %>% summarize(number= n())

x_hist<- dataset_reprocessing %>% ggplot(aes(Pregnancies)) + geom_histogram(bins = 50, color= "black", fill= "steelblue", alpha= .1, binwidth = 1) + 
  scale_y_continuous(breaks = seq(0,150,25)) + scale_x_continuous(breaks = seq(0,18,1)) +
  labs(title = "Fig. 1.3 - Histogram of the number of pregnancies",x= "Number of pregnancies", y= "Sum of pregnancies")  + theme_classic() + theme(plot.title = element_text(hjust = 0.5))

x_nratings<- tableGrob(x, rows = NULL, cols = c("Number of pregnancies", "Sum of pregnancies"), theme = ttheme_minimal())

a<- grid.arrange(x_hist,x_nratings, ncol= 2, widths= c(7,3))

grid.arrange(x_plot, a)

rm(x_plot, x, x_hist, x_nratings, a)

invisible(invisible(gc())) # for cleaning memory

# Chapter 2 - Glucose distribution

  ## We will analyse the principal statistical data of the "Glucose" variable

as.data.frame(quantile(dataset_reprocessing$Glucose, c(0.1, .25, 0.5, 0.75, .9, .95), na.rm = TRUE)) # Evaluate some quantiles and deciles 

data.frame(min.= round(min(dataset_reprocessing$Glucose, na.rm = TRUE),2), median= round(median(dataset_reprocessing$Glucose, na.rm = TRUE),2),
           mean= round(mean(dataset_reprocessing$Glucose, na.rm = TRUE),2),
           standard_deviation= round(sd(dataset_reprocessing$Glucose, na.rm = TRUE),2),
           max.= round(max(dataset_reprocessing$Glucose, na.rm = TRUE),2)) # Evaluate min, max, median, mean and standard deviation

  ## we will represent a density plot of the quantities of glucose by woman

x_plot<- dataset_reprocessing %>% ggplot(aes(Glucose)) + geom_density(kernel= "optcosine", adjust= 1.5, fill= "blue", alpha= .1) + 
  scale_x_continuous(breaks = seq(0,200,25)) +
  labs(title = "Fig. 2.1 - Density curve of the quantities of glucose by woman",x= "Quantities of glucose", y= "Frequency") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

  ## We will plot an histogram of the number of pregnancies

x_hist<- dataset_reprocessing %>% ggplot(aes(Glucose)) + geom_histogram(bins = 100, color= "black", fill= "steelblue", alpha= .1, binwidth = 1) + 
  scale_y_continuous(breaks = seq(0,20,2)) + scale_x_continuous(breaks = seq(0,200,25)) +
  labs(title = "Fig. 2.3 - Histogram of the quantities of glucose by woman",x= "Quantities of glucose", y= "Number of woman")  + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))


grid.arrange(x_plot, x_hist)

rm(x_plot, x_hist)

invisible(invisible(gc())) # for cleaning memory

# Chapter 3 - BloodPressure distribution

  ## We will analyse the principal statistical data of the "Blood pressure" variable

as.data.frame(quantile(dataset_reprocessing$BloodPressure, c(0.1, .25, 0.5, 0.75, .9, .95), na.rm = TRUE)) # Evaluate some quantiles and deciles 

data.frame(min.= round(min(dataset_reprocessing$BloodPressure, na.rm = TRUE),2), median= round(median(dataset_reprocessing$BloodPressure, na.rm = TRUE),2),
           mean= round(mean(dataset_reprocessing$BloodPressure, na.rm = TRUE),2),
           standard_deviation= round(sd(dataset_reprocessing$BloodPressure, na.rm = TRUE),2),
           max.= round(max(dataset_reprocessing$BloodPressure, na.rm = TRUE),2)) # Evaluate min, max, median, mean and standard deviation

  ## we will represent a density plot of the blood pressure by woman

x_plot<- dataset_reprocessing %>% ggplot(aes(BloodPressure)) + geom_density(kernel= "optcosine", adjust= 1.5, fill= "blue", alpha= .1) + 
  scale_x_continuous(breaks = seq(0,125,25)) +
  labs(title = "Fig. 3.1 - Density curve of the diastolic blood pressure by woman",x= "Diastolic blood pressure (mm Hg)", y= "Frequency") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

  ## We will plot an histogram of the blood pressure by woman

x_hist<- dataset_reprocessing %>% ggplot(aes(BloodPressure)) + geom_histogram(bins = 100, color= "black", fill= "steelblue", alpha= .1, binwidth = 1) + 
  scale_y_continuous(breaks = seq(0,50,5)) + scale_x_continuous(breaks = seq(0,125,25)) +
  labs(title = "Fig. 3.2 - Histogram of the diastolic blood pressure by woman",x= "Diastolic blood pressure (mm Hg)", 
       y= "Number of woman")  + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))


grid.arrange(x_plot, x_hist)

rm(x_plot, x_hist)

invisible(invisible(gc())) # for cleaning memory

# Chapter 4 - Triceps skinfold thickness

  ## We will analyse the principal statistical data of the "SkinThickness" variable
as.data.frame(quantile(dataset_reprocessing$SkinThickness, c(0.1, .25, 0.5, 0.75, .9, .95), na.rm = TRUE)) # Evaluate some quantiles and deciles 

data.frame(min.= round(min(dataset_reprocessing$SkinThickness, na.rm = TRUE),2), median= round(median(dataset_reprocessing$SkinThickness, na.rm = TRUE),2),
           mean= round(mean(dataset_reprocessing$SkinThickness, na.rm = TRUE),2),
           standard_deviation= round(sd(dataset_reprocessing$SkinThickness, na.rm = TRUE),2),
           max.= round(max(dataset_reprocessing$SkinThickness, na.rm = TRUE),2)) # Evaluate min, max, median, mean and standard deviation

  ## we will represent a density plot of the Triceps skinfold thickness by woman

x_plot<- dataset_reprocessing %>% ggplot(aes(SkinThickness)) + geom_density(kernel= "optcosine", adjust= 1.5, fill= "blue", alpha= .1) + 
  scale_x_continuous(breaks = seq(0,100,25)) +
  labs(title = "Fig. 4.1 - Density curve of the Triceps skinfold thickness by woman",
       x= "Triceps skinfold thickness (mm)", y= "Frequency") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

  ## We will plot an histogram of the Triceps skinfold thickness by woman

x_hist<- dataset_reprocessing %>% ggplot(aes(SkinThickness)) + geom_histogram(bins = 100, color= "black", fill= "steelblue", alpha= .1, binwidth = 1) + 
  scale_y_continuous(breaks = seq(0,200,25)) + scale_x_continuous(breaks = seq(0,100,25)) +
  labs(title = "Fig. 4.2 - Histogram of the Triceps skinfold thickness by woman",x= "Triceps skinfold thickness (mm)", 
       y= "Number of woman")  + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))


grid.arrange(x_plot, x_hist)

rm(x_plot, x_hist)

invisible(invisible(gc())) # for cleaning memory

# Chapter 5 - Insulin level distribution

  ## We will analyse the principal statistical data of the "Insulin" variable
as.data.frame(quantile(dataset_reprocessing$Insulin, c(0.1, .25, 0.5, 0.75, .9, .95), na.rm = TRUE)) # Evaluate some quantiles and deciles 

data.frame(min.= round(min(dataset_reprocessing$Insulin, na.rm = TRUE),2), median= round(median(dataset_reprocessing$Insulin, na.rm = TRUE),2),
           mean= round(mean(dataset_reprocessing$Insulin, na.rm = TRUE),2),
           standard_deviation= round(sd(dataset_reprocessing$Insulin, na.rm = TRUE),2), 
           max.= round(max(dataset_reprocessing$Insulin, na.rm = TRUE),2)) # Evaluate min, max, median, mean and standard deviation

  ## we will represent a density plot of the insulin level by woman

x_plot<- dataset_reprocessing %>% ggplot(aes(Insulin)) + geom_density(kernel= "optcosine", adjust= 1.5, fill= "blue", alpha= .1) + 
  scale_x_continuous(breaks = seq(0,800,50)) +
  labs(title = "Fig. 5.1 - Density curve of insulin level by woman",
       x= "Insulin level (mu U/ml)", y= "Frequency") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

  ## We will plot an histogram of the insulin level by woman

x_hist<- dataset_reprocessing %>% ggplot(aes(Insulin)) + geom_histogram(bins = 100, color= "black", fill= "steelblue", alpha= .1, binwidth = 1) + 
  scale_y_continuous(breaks = seq(0,350,50)) + scale_x_continuous(breaks = seq(0,800,50)) +
  labs(title = "Fig. 5.2 - Histogram of the insulin level by woman",x= "Insulin level (mu U/ml)", 
       y= "Number of woman")  + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))


grid.arrange(x_plot, x_hist)

rm(x_plot, x_hist)

invisible(invisible(gc())) # for cleaning memory

#Chapter 6 - BMI distribution

## We will analyse the principal statistical data of the "Insulin" variable
as.data.frame(quantile(dataset_reprocessing$BMI, c(0.1, .25, 0.5, 0.75, .9, .95), na.rm = TRUE)) # Evaluate some quantiles and deciles 

data.frame(min.= round(min(dataset_reprocessing$BMI, na.rm = TRUE),2), median= round(median(dataset_reprocessing$BMI, na.rm = TRUE),2),
           mean= round(mean(dataset_reprocessing$BMI, na.rm = TRUE),2),
           standard_deviation= round(sd(dataset_reprocessing$BMI, na.rm = TRUE),2), 
           max.= round(max(dataset_reprocessing$BMI, na.rm = TRUE),2)) # Evaluate min, max, median, mean and standard deviation

## we will represent a density plot of the BMI by woman

x_plot<- dataset_reprocessing %>% ggplot(aes(BMI)) + geom_density(kernel= "optcosine", adjust= 1.5, fill= "blue", alpha= .1) + 
  scale_x_continuous(breaks = seq(0,60,5)) +
  labs(title = "Fig. 6.1 - Density curve of the BMI by woman",
       x= "BMI", y= "Frequency") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

## We will plot an histogram of the insulin level by woman

x_hist<- dataset_reprocessing %>% ggplot(aes(BMI)) + geom_histogram(bins = 100, color= "black", fill= "steelblue", alpha= .1, binwidth = 1) + 
  scale_y_continuous(breaks = seq(0,60,5)) + scale_x_continuous(breaks = seq(0,60,5)) +
  labs(title = "Fig. 6.2 - Histogram of the BMI by woman",x= "BMI", 
       y= "Number of woman")  + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))


grid.arrange(x_plot, x_hist)

rm(x_plot, x_hist)

invisible(invisible(gc())) # for cleaning memory

# Chapter 7 - Diabetes pedigree function distibution

  ## We will analyse the principal statistical data of the "DiabetesPedigreeFunction" variable
as.data.frame(quantile(dataset_reprocessing$DiabetesPedigreeFunction, c(0.1, .25, 0.5, 0.75, .9, .95))) # Evaluate some quantiles and deciles 

data.frame(min.= round(min(dataset_reprocessing$DiabetesPedigreeFunction),2), median= round(median(dataset_reprocessing$DiabetesPedigreeFunction),2),
           mean= round(mean(dataset_reprocessing$DiabetesPedigreeFunction),2),
           standard_deviation= round(sd(dataset_reprocessing$DiabetesPedigreeFunction),2), max.= round(max(dataset_reprocessing$DiabetesPedigreeFunction),2)) # Evaluate min, max, median, mean and standard deviation

  ## we will represent a density plot of the diabetes pedigree function by woman

dataset_reprocessing %>% ggplot(aes(DiabetesPedigreeFunction)) + geom_density(kernel= "optcosine", adjust= 1.5, fill= "blue", alpha= .1) + 
  scale_x_continuous(breaks = seq(0,2.5,.25)) +
  labs(x= "diabetes pedigree function", y= "Frequency") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))


invisible(invisible(gc())) # for cleaning memory

# Chapter 8 - Age distribution

## We will analyse the principal statistical data of the "Age" variable
as.data.frame(quantile(dataset_reprocessing$Age, c(0.1, .25, 0.5, 0.75, .9, .95))) # Evaluate some quantiles and deciles 

data.frame(min.= round(min(dataset_reprocessing$Age),2), median= round(median(dataset_reprocessing$Age),2),
           mean= round(mean(dataset_reprocessing$Age),2),
           standard_deviation= round(sd(dataset_reprocessing$Age),2), max.= round(max(dataset_reprocessing$Age),2)) # Evaluate min, max, median, mean and standard deviation

## we will represent a density plot of the age by woman

x_plot<- dataset_reprocessing %>% ggplot(aes(Age)) + geom_density(kernel= "optcosine", adjust= 1.5, fill= "blue", alpha= .1) + 
  scale_x_continuous(breaks = seq(20,80,5)) +
  labs(title = "Fig. 8.1 - Density curve of the age by woman",
       x= "Age", y= "Frequency") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

## We will plot an histogram of the age by woman

x_hist<- dataset_reprocessing %>% ggplot(aes(Age)) + geom_histogram(color= "black", fill= "steelblue", alpha= .1, binwidth = 1) + 
  scale_y_continuous(breaks = seq(0,70,5)) + scale_x_continuous(breaks = seq(20,80,5)) +
  labs(title = "Fig. 8.2 - Histogram of the age by woman",x= "Age", 
       y= "Number of woman")  + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))


grid.arrange(x_plot, x_hist)

rm(x_plot, x_hist)

invisible(invisible(gc())) # for cleaning memory

# Chapter 9 - Outcomes distribution

  ## We will plot an histogram of the outcomes
x<- dataset_reprocessing %>% group_by(Outcome) %>% summarize(number= n())

x_hist<- dataset_reprocessing %>% ggplot(aes(Outcome)) + geom_bar(stat = "count", color= "black", fill= "steelblue", alpha= .1) +
  scale_y_continuous(breaks = seq(0,500,50)) + 
  labs(x= "Outcomes", y= "Number of outcomes") + theme_classic()

x_nratings<- tableGrob(x, rows = NULL, cols = c("Diabetes", "Number of women"), theme = ttheme_minimal())

grid.arrange(x_nratings, x_hist, nrow= 2, heights= c(2,10))

rm(x, x_hist, x_nratings)

invisible(invisible(gc())) # for cleaning memory


# Chapter 10 - Analysis of correlation between variables

# we will convert the "outcome" variable into a numeric value and the data set into a matrix

data_cor_2<- dataset_reprocessing %>% mutate(Outcome= as.numeric(Outcome)) %>% as.matrix()

# We will implement the correlation and plot it

cor_var_2<- data_cor_2 %>% cor(use = "pairwise.complete.obs")

corr_2<- corrplot(cor_var_2, p.mat = cor.mtest(data_cor_2, conf.level= 0.95)$p, sig.level = 0.01, method = "number", type = "upper",
                  title = "Assumption 2 - dataset with NA's values")


###########################################################################################
#                                                                                         #
#                      Part III - Predictive algorithm models                             #
#                                                                                         #
###########################################################################################

# We will split the data set into a training test and a validation test
# The training test will be used to train our predictive algorithms and select the best parameters which we will permits us to minimize the RMSE
# The test set will be used to evaluate our predictive algorithms

## Validation set will be 20% of data set
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = dataset_reprocessing$Outcome, times = 1, p = 0.2, list = FALSE)
train_set_2 <- dataset_reprocessing[-test_index,]
test_set_2 <- dataset_reprocessing[test_index,]

rm(test_index) # remove object

invisible(invisible(gc())) # for cleaning memory

test_set_2<- na.omit(test_set_2)

#####################################################
#                                                   #
#          Generalized Linear model                 #
#                                                   #
#####################################################

# We will train the predictive algorithm
train_glm_2<- train(Outcome ~ ., method= "glm", data = train_set_2, na.action = na.omit)

## We will predict outcome from the test set
pred_glm_2<- predict(train_glm_2, test_set_2, na.action = na.omit)


## We will use "confusionMatrix" to evaluate the model
acc_glm_2<- confusionMatrix(pred_glm_2, test_set_2$Outcome)

## We represent the prediction vs outcomes contained in the data set

acc_glm_2$table

## We will save the results of the model into a data frame
results_model_2<- tibble(Model = "Generalized Linear model", Accuracy= acc_glm_2$overall[[1]], F1_score= acc_glm_2$byClass[[7]],
                         Prevalence= acc_glm_2$byClass[[8]], 
                         Precision= acc_glm_2$byClass[[5]], Recall= acc_glm_2$byClass[[6]], P_value= acc_glm_2$overall[[6]])

results_model_2

invisible(invisible(gc())) # for cleaning memory

#####################################################
#                                                   #
#     Generalized Additive Model using LOESS        #
#                                                   #
#####################################################


## We will train the predictive algorithm and select the best tune parameters
train_gamloess_2<- train(Outcome ~ ., method= "gamLoess", data = train_set_2,
                         tuneGrid= data.frame(span= seq(1, 5, .25), degree= 1),
                         trControl= trainControl(method = "cv", number = 10, p= 0.9), na.action = na.omit)

## We will represent the best tune parameter
plot_gamloess_2<- ggplot(train_gamloess_2, highlight = TRUE)  + scale_x_continuous(breaks = seq(1,5, .5))  + theme_classic()

btp_gamloess_2<- tableGrob(train_gamloess_2$bestTune[[1]], cols = "Best tune parameter", rows = NULL, theme = ttheme_minimal())

grid.arrange(btp_gamloess_2,plot_gamloess_2, heights= c(1,12))

## We will predict outcome from the test set
pred_gamloess_2<- predict(train_gamloess_2, test_set_2)

## We will use "confusionMatrix" to evaluate the model
acc_gamloess_2<- confusionMatrix(pred_gamloess_2, test_set_2$Outcome)

## We represent the prediction vs outcomes contained in the data set

acc_gamloess_2$table

## We will save the results of the model into a data frame
results_model_2<- bind_rows(results_model_2, tibble(Model = "Generalized Additive Model using LOESS",
                                                    Accuracy= acc_gamloess_2$overall[[1]], F1_score= acc_gamloess_2$byClass[[7]],
                                                    Prevalence= acc_gamloess_2$byClass[[8]], 
                                                    Precision= acc_gamloess_2$byClass[[5]], 
                                                    Recall= acc_gamloess_2$byClass[[6]], P_value= acc_gamloess_2$overall[[6]]))

results_model_2

invisible(invisible(gc())) # for cleaning memory

#####################################################
#                                                   #
#                    kNN Model                      #
#                                                   #
#####################################################

## We will train the predictive algorithm and select the best tune parameters
set.seed(1988, sample.kind="Rounding")
train_knn_2<- train(Outcome ~ ., method= "knn", data = train_set_2,
                    tuneGrid= data.frame(k= seq(1,50,1)),
                    trControl= trainControl(method = "cv", number = 10, p= 0.9), na.action = na.omit)

## We will represent the best tune parameter
plot_knn_2<- ggplot(train_knn_2, highlight = TRUE)  + scale_x_continuous(breaks = seq(0,50, 5))  + theme_classic()

btp_knn_2<- tableGrob(train_knn_2$bestTune[[1]], cols = "Best tune parameter", rows = NULL, theme = ttheme_minimal())

grid.arrange(btp_knn_2,plot_knn_2, heights= c(1,12))

## We will predict outcome from the test set
pred_knn_2<- predict(train_knn_2, test_set_2)

## We will use "confusionMatrix" to evaluate the model
acc_knn_2<- confusionMatrix(pred_knn_2, test_set_2$Outcome)

## We represent the prediction vs outcomes contained in the data set

acc_knn_2$table


## We will save the results of the model into a data frame
results_model_2<- bind_rows(results_model_2, 
                            tibble(Model = "kNN Model", Accuracy= acc_knn_2$overall[[1]], 
                                   F1_score= acc_knn_2$byClass[[7]],
                                   Prevalence= acc_knn_2$byClass[[8]], 
                                   Precision= acc_knn_2$byClass[[5]], Recall= acc_knn_2$byClass[[6]], 
                                   P_value= acc_knn_2$overall[[6]]))
results_model_2

invisible(invisible(gc())) # for cleaning memory

#####################################################
#                                                   #
#           Classification tree model               #
#                                                   #
#####################################################

## We will train the predictive algorithm and select the best tune parameters
set.seed(1988, sample.kind="Rounding")
train_rpart_2<- train(Outcome ~ ., method= "rpart", data = train_set_2,
                      tuneGrid= data.frame(cp = seq(0, 0.05, 0.002)),
                      trControl= trainControl(method = "cv", number = 10, p= 0.9), na.action = na.omit)

## We will represent the best tune parameter
plot_rpart_2<- ggplot(train_rpart_2, highlight = TRUE) + theme_classic()

btp_rpart_2<- tableGrob(train_rpart_2$bestTune[[1]], cols = "Best tune parameter", rows = NULL, theme = ttheme_minimal())

grid.arrange(btp_rpart_2,plot_rpart_2, heights= c(1,12))

## We will represent the decision tree of this model
rpart.plot(train_rpart_2$finalModel, type = 3, clip.right.labs = FALSE)

## We will predict outcome from the test set
pred_rpart_2<- predict(train_rpart_2, test_set_2)

## We will use "confusionMatrix" to evaluate the model
acc_rpart_2<- confusionMatrix(pred_rpart_2, test_set_2$Outcome)

## We represent the prediction vs outcomes contained in the data set

acc_rpart_2$table

## We will save the results of the model into a data frame
results_model_2<- bind_rows(results_model_2,
                            tibble(Model = "Classification tree model", 
                                   Accuracy= acc_rpart_2$overall[[1]], F1_score= acc_rpart_2$byClass[[7]],
                                   Prevalence= acc_rpart_2$byClass[[8]], Precision= acc_rpart_2$byClass[[5]], 
                                   Recall= acc_rpart_2$byClass[[6]], P_value= acc_rpart_2$overall[[6]]))

results_model_2

#####################################################
#                                                   #
#               Random Forest model                 #
#                                                   #
#####################################################

## We will train the predictive algorithm and select the best tune parameters
set.seed(1988, sample.kind="Rounding")
train_rf_2<- train(Outcome ~ ., method= "rf", data = train_set_2,
                   ntree= 100,
                   tuneGrid= data.frame(mtry= seq(1:50)),
                   trControl= trainControl(method = "cv", number = 10, p= 0.9), na.action = na.omit)

## We will represent the best tune parameter
plot_rf_2<- ggplot(train_rf_2, highlight = TRUE) + theme_classic()

btp_rf_2<- tableGrob(train_rf_2$bestTune[[1]], cols = "Best tune parameter", rows = NULL, theme = ttheme_minimal())

grid.arrange(btp_rf_2,plot_rf_2, heights= c(1,12))

## We will predict outcome from the test set
pred_rf_2<- predict(train_rf_2, test_set_2)

## We will use "confusionMatrix" to evaluate the model
acc_rf_2<- confusionMatrix(pred_rf_2, test_set_2$Outcome)

## We represent the prediction vs outcomes contained in the data set

acc_rf_2$table

## We will save the results of the model into a data frame
results_model_2<- bind_rows(results_model_2,
                            tibble(Model = "Random Forest model", 
                                   Accuracy= acc_rf_2$overall[[1]], F1_score= acc_rf_2$byClass[[7]],
                                   Prevalence= acc_rf_2$byClass[[8]], Precision= acc_rf_2$byClass[[5]], 
                                   Recall= acc_rf_2$byClass[[6]], P_value= acc_rf_2$overall[[6]]))

results_model_2

#####################################################
#                                                   #
#    Random Forest model with Rborist method        #
#                                                   #
#####################################################

## We will train the predictive algorithm and select the best tune parameters
set.seed(1988, sample.kind="Rounding")
train_rborist_2<- train(Outcome ~ ., method= "Rborist", data = train_set_2,
                        tuneGrid= data.frame(minNode= seq(1:10), predFixed= seq(1:50)),
                        trControl= trainControl(method = "cv", number = 10, p= 0.9), na.action = na.omit)

## We will represent the best tune parameter
train_rborist_2$finalModel$tuneValue

## We will predict outcome from the test set
pred_rborist_2<- predict(train_rborist_2, test_set_2)

## We will use "confusionMatrix" to evaluate the model
acc_rborist_2<- confusionMatrix(pred_rborist_2, test_set_2$Outcome)

## We represent the prediction vs outcomes contained in the data set

acc_rborist_2$table

## We will save the results of the model into a data frame
results_model_2<- bind_rows(results_model_2,
                            tibble(Model = "Random Forest model with Rborist method ", 
                                   Accuracy= acc_rborist_2$overall[[1]], F1_score= acc_rborist_2$byClass[[7]],
                                   Prevalence= acc_rborist_2$byClass[[8]], Precision= acc_rborist_2$byClass[[5]], 
                                   Recall= acc_rborist_2$byClass[[6]], P_value= acc_rborist_2$overall[[6]]))

results_model_2

