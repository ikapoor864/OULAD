#########################################Making a better leaning platform#################
#Installing packages

#install.packages("dplyr")
#install.packages("Rcpp")
#install.packages("RInside")
#install.packages("ggplot2")
#install.packages("party")
#install.packages("caret")
#install.packages("e1071")
#install.packages("gbm")
#install.packages("randomForest")
#install.packages("h2o")

#Loading libraries#

library(dplyr)
library(Rcpp)
library(RInside)
library(ggplot2)


####Reading the data###

courses <-
  read.csv("C:\\Users\\ikapo\\OneDrive\\Desktop\\OULAD\\courses.csv",
           stringsAsFactors = FALSE)
assessments <-
  read.csv(
    "C:\\Users\\ikapo\\OneDrive\\Desktop\\OULAD\\assessments.csv",
    stringsAsFactors = FALSE
  )
student_as <-
  read.csv(
    "C:\\Users\\ikapo\\OneDrive\\Desktop\\OULAD\\studentAssessment.csv",
    stringsAsFactors = FALSE
  )
students <-
  read.csv(
    "C:\\Users\\ikapo\\OneDrive\\Desktop\\OULAD\\studentInfo.csv",
    stringsAsFactors = FALSE
  )
student_reg <-
  read.csv(
    "C:\\Users\\ikapo\\OneDrive\\Desktop\\OULAD\\studentRegistration.csv",
    stringsAsFactors = FALSE
  )
student_vle <-
  read.csv("C:\\Users\\ikapo\\OneDrive\\Desktop\\OULAD\\studentVle.csv",
           stringsAsFactors = FALSE)
vle <-
  read.csv("C:\\Users\\ikapo\\OneDrive\\Desktop\\OULAD\\Vle.csv",
           stringsAsFactors = FALSE)


xx <-  students %>% distinct(id_student,age_band) 
xx1 <- xx %>% select(id_student,age_band) %>% group_by(age_band) %>% summarize(count= n())

#REASONS FOR STUDENT PERFORMANCE
#1) Course available
#2) age group
#3) Disabilty
#4) Region
#5) No. of credits taken by a student
#6) Education



##Finding the number of uniques students##
length(unique(students$id_student))
#There are a total of 28785 unique students in the class
#THe unique identifier of the table is studentid, code_module and code_presentation

##################FINDING THE % of students who failed/passed/withdrew based on the course module###################

###Number of students passed/failed/withdrawn

num <-
  students %>% select(code_module, code_presentation, final_result) %>%
  group_by(code_module, code_presentation, final_result) %>% summarize(count =
                                                                         n())

num_1 <- students %>% select(code_module, code_presentation) %>%
  group_by(code_module, code_presentation) %>% summarize(total = n())

#Merging the two datsets to find the percentage

nums <-
  merge(num, num_1, by = c("code_module", "code_presentation"))

nums$rate <- (nums$count / nums$total) * 100
#Key take aways:
#1) We can observe a maximum wuthdrawl % in course modeule CCC for both feb and october presentations.
#2) Maximum pass rate is observed in AAA for both feb and october
#3) The lowest withdrawl rate is obtained in GGG, with a lowest fail % in AAA
#4) However GGG has the highest failure rate

#From the above few observations, we can say that people are performing mucch better in the course AAA.

#Since CCC has the maximum withdrawl rate, lets look at the course. We look at the weightage of exams for all the
#courses

#From the VLE, we can find the number of resources provided for each course

vle_c <- vle %>% select(code_module, code_presentation) %>%
  group_by(code_module, code_presentation) %>% summarize(count_l = n())

#When we look at the above results, we see that the minimum no. of resources are present in GGG. So, having
#less resources might be a possible reason for more failure

#However, FFF is the one with the highest resources but the pass rate is ~40%. So, resources is not the only reason
#for good or bad performance

#We can look at the number of students, who looked at the material before the classes began.

clicks <-
  student_vle  %>% filter(date < 0) %>% select(code_module, code_presentation, sum_click) %>%
  group_by(code_module, code_presentation) %>% summarize(tot_clicks = sum(sum_click))

#Even when we look at the number of clicks for the courses, it is the least for GGG.
#more clicks can mean that the course is more diificult.

####################Weightage of each exam for all the course module####################

weights <-
  assessments %>% select(code_module, code_presentation, weight) %>%
  group_by(code_module, code_presentation) %>% summarize(weight_tot = sum(weight))

#From the above table, we can see that GGG has a total weight of 100 and CCC has a weight of 300.
#Have to look at these two courses


weights_1 <-
  assessments %>% filter(code_module %in% c("CCC", "GGG")) %>% select(code_module, code_presentation, assessment_type, weight) %>%
  group_by(code_module, code_presentation, assessment_type) %>% summarize(weight_tot = sum(weight))

#We can observe that CCc has a weightage 200% given to exams and GGG has 0 weightage given to the TMA and CMA.

#################################AGE BAND AND PERFORMANCE##################################


age_c <- students %>% select(age_band, final_result) %>%
  group_by(age_band, final_result) %>% summarize(results = n())


age_tot <- students %>% select(age_band) %>%
  group_by(age_band) %>% summarize(results_l = n())

age_n <- merge(age_c, age_tot, by = c("age_band"))

age_n$rate <- (age_n$results / age_n$results_l) * 100

#The maximum pass rate is for people with age >=55
#Highest fail rate is 0-35


####################################REGION AND PERFORMANCE###################
region_c <- students %>% select(region, final_result) %>%
  group_by(region, final_result) %>% summarize(results = n())


region_tot <- students %>% select(region) %>%
  group_by(region) %>% summarize(results_l = n())

region_n <- merge(region_c, region_tot, by = c("region"))

region_n$rate <- (region_n$results / region_n$results_l) * 100

#Highest pass rate is from ireland
#Highest withdrawl rate is from 	North Western Region
#WAles : Highest fail rate


########################Credits and number of students#######################
counts <- table(students$studied_credits)
barplot(counts, main = "credit distribution",
        xlab = "Credits")

studs_c <- students %>% select(studied_credits) %>%
  group_by(studied_credits) %>% summarize(results = n())

#Max number of students have 60 credits. Mostly students have taken less than 150 credits.

####################################Credits AND PERFORMANCE###################

credits_c <- students %>% select(studied_credits, final_result) %>%
  group_by(studied_credits, final_result) %>% summarize(results = n())


credits_tot <- students %>% select(studied_credits) %>%
  group_by(studied_credits) %>% summarize(results_l = n())

credits_n <-
  merge(credits_c, credits_tot, by = c("studied_credits"))

credits_n$rate <- (credits_n$results / credits_n$results_l) * 100

#########################Gender and performance###########################

gender_c <- students %>% select(gender, final_result) %>%
  group_by(gender, final_result) %>% summarize(results = n())


gender_tot <- students %>% select(gender) %>%
  group_by(gender) %>% summarize(results_l = n())

gender_n <- merge(gender_c, gender_tot, by = c("gender"))

gender_n$rate <- (gender_n$results / gender_n$results_l) * 100

#There is not much difference between male and female when it comes to performance

###############Education and performance######################


edu_p <- students %>% select(highest_education, final_result) %>%
  group_by(highest_education, final_result) %>% summarize(results = n())


edu_tot <- students %>% select(highest_education) %>%
  group_by(highest_education) %>% summarize(results_l = n())

edu_n <- merge(edu_p, edu_tot, by = c("highest_education"))

edu_n$rate <- (edu_n$results / edu_n$results_l) * 100

#highest withdrawl and fail rate is from students with no prior formal quals
#Highest pass rate= HE Qualification, A Level or Equivalent


#############################Attempts and performance###################

attempts_p <-
  students %>% select(num_of_prev_attempts, final_result) %>%
  group_by(num_of_prev_attempts, final_result) %>% summarize(results = n())


attempts_tot <- students %>% select(num_of_prev_attempts) %>%
  group_by(num_of_prev_attempts) %>% summarize(results_l = n())

attempts_n <-
  merge(attempts_p, attempts_tot, by = c("num_of_prev_attempts"))

attempts_n$rate <- (attempts_n$results / attempts_n$results_l) * 100

#maximum attempt taken by a student is 6,
#maximum withdrawl is from students who have taken 6 attempts
#maximum failure from students with attempts = 5
#Maximum pass rate for students with attempt = 0(1st attempt)


######################Disability and performance####################################

counts <- table(students$disability)
barplot(counts, main = "credit distribution",
        xlab = "Credits")
#Smalll ratio of students are disable

dis_p <- students %>% select(disability, final_result) %>%
  group_by(disability, final_result) %>% summarize(results = n())


dis_tot <- students %>% select(disability) %>%
  group_by(disability) %>% summarize(results_l = n())

dis_n <- merge(dis_p, dis_tot, by = c("disability"))

dis_n$rate <- (dis_n$results / dis_n$results_l) * 100

#The % for pass/fail is similar in both the cases.


######################################Region and Course########################
reg_code <-
  students %>% select(code_module, region, final_result) %>%
  group_by(code_module, region, final_result) %>% summarize(count = n())

reg_code_1 <- students %>% select(code_module, region) %>%
  group_by(code_module, region) %>% summarize(total = n())

reg_c_r <-
  merge(reg_code, reg_code_1, by = c("code_module", "region"))

reg_c_r$rate <- (reg_c_r$count / reg_c_r$total) * 100

#Max withdrawl is ireland for GGG, North Western Region for CCC, max failure is GGG for NWR.

##########################Date registered and performance##############################

#merging student id and student_reg

merge_sreg <-
  merge(students,
        student_reg,
        by = c("code_module", "code_presentation", "id_student"))

#A student, having a value under unregistered column should have a whithdrawl result.


errors <-
  merge_sreg %>% filter(date_unregistration != "?" &
                          final_result != "Withdrawn")
#There are 9 such cases with final result as fail but the student has withdrawn.An equal
#distribution of BBB,DDD and FFF


##########mERging students and clicks, to get the avg no. of clicks as per the performance########
avg_cl <-
  student_vle %>% select(code_module, code_presentation, id_student, sum_click) %>%
  group_by(code_module, code_presentation, id_student) %>% summarise(clicks = mean(sum_click))

merge_stu_c <-
  merge(students,
        avg_cl,
        by = c("code_module", "code_presentation", "id_student"))

write.csv(merge_stu_c, "clicks.csv")

#FRom the clicks, we can see that students with distinction and pass result are the one's have more avg per click.
#Therefore we can say that online course plays an important role.


################################MODELLING#############################



#AFter an extensive EDA, lets test how the model is performing

#A multi class classification will be the best method to find if the student will pass/fail
##IMportant factors from our EDA:

#1) vle - no. of resources
#2) Date of registeration
#3) Clicks
#4) Score of students
#5 Area where they live

#We will combine the 5 datasets, student_vle, student_registered, student_assesment, assessments and student_info

#Step 1) Clean the dataset
#STUDENT
#Replacing NA's with 0
students[students == "?"] <- "0"
#THE IMD band has the NA, Replacing NA's with 0

#Student assessment
student_as[student_as == "?"] <- NA
sum(is.na(student_as$score))
#173 NA's
#Score has NA values, replacing it with 0's
student_as[is.na(student_as)] <- 0

#Student registeration
student_reg[student_reg == "?"] <- NA
sum(is.na(student_reg))
#REmoving this column as it has too many Na's
student_reg$date_unregistration <- NULL

##Assesments

assessments[assessments == "?"] <- NA
sum(is.na(assessments))
assessments[is.na(assessments)] <- 0

##STudent_vle
student_vle[student_vle == "?"] <- NA
sum(is.na(student_vle))
#No NA's


#Our 1st model will be to predict the final result based on number of average clicks per 
#day before the end of assesment

#Now that the data is clean: Lets merge them to get a single dataset:

#1)
stu_as <- merge(assessments, student_as, by = "id_assessment")
#2)
stu_as_1 <-
  merge(student_vle,
        stu_as,
        by = c("code_module", "code_presentation", "id_student"))

stu_as_2 <- stu_as_1 %>% filter(date_x <= date_y) %>%
  select(code_module,
         code_presentation,
         id_student,
         assessment_type,
         sum_clicks) %>%
  group_by(code_module, code_presentation, id_student, assessment_type) %>% summarize(clicks = sum(sum_clicks))

#Finding the average clicks

stu_as_2$click_rate <-  stu_as_2$clicks / stu_as_2$date_submitted

#Faced m/m issue, ran this code on python
stu_as_2 <-
  read.csv("C:\\Users\\ikapo\\OneDrive\\Desktop\\OULAD\\stu_as_1.csv",
           stringsAsFactors = FALSE)

#Combining with students table to get other information

stu_as_2_s <-
  merge(stu_as_2,
        students,
        by = c("code_module", "code_presentation", "id_student"))
stu_as_2_s[stu_as_2_s == "?"] <- "0"

#Picking up relevant variables to predict the result of student

x_data <-
  stu_as_2_s %>% select(
    code_module,
    assessment_type,
    sum_click_rate,
    highest_education,
    age_band,
    gender,
    region,
    num_of_prev_attempts,
    imd_band,
    disability,
    final_result
  )


#Making all categorical variables a factor
x_data$code_module <- as.numeric(as.factor(x_data$code_module))
x_data$assessment_type <-
  as.numeric(as.factor(x_data$assessment_type))
x_data$highest_education <-
  as.numeric(as.factor(x_data$highest_education))
x_data$age_band <- as.numeric(as.factor(x_data$age_band))
x_data$imd_band <- as.numeric(as.factor(x_data$imd_band))
x_data$region <- as.numeric(as.factor(x_data$region))
x_data$disability <- as.numeric(as.factor(x_data$disability))
x_data$gender <- as.numeric(as.factor(x_data$gender))
x_data$final_result <- as.factor(x_data$final_result)

#Getting all relevant libraries

library(party)
library(caret)
library(e1071)
library(gbm)
library(randomForest)


#Spliting the data into train and test
set.seed(1234)
# partition data into training and testing
inTrain <-
  createDataPartition(y = x_data$final_result,
                      # outcome variable
                      p = .80,
                      # % of train data you want
                      list = FALSE)

train <- x_data[inTrain, ]
test <- x_data[-inTrain, ]
str(train) # 13370 obs. of  49 variables (training data)
str(test)

table(x_data$final_result)
#Distinction        Fail        Pass   Withdrawn
#5900               9038       23262        6364

############## Train Control Design ##################
ctrl <-
  trainControl(
    method = "cv",
    # cross-validation set approach to use
    number = 5,
    # k number of times to do k-fold
    classProbs = T,
    summaryFunction = mnLogLoss,
    allowParallel = T
  )

################## Balance Classes #################
library(DMwR)
model_weights <- ifelse(
  train$final_result == 'Distinction',
  (1 / table(train$final_result)[1]) * 0.25,
  ifelse(
    train$final_result == 'Fail',
    (1 / table(train$final_result)[2]) * 0.25,
    ifelse(
      train$final_result == 'Pass',
      (1 / table(train$final_result)[3]) *
        0.25,
      (1 / table(train$final_result)[4]) *
        0.25
    )
  )
)


#Checking relevant features through logit back propogation

LogitBackward = function(train, pval = 0.1)
{
  train2 = train
  pmax = 1
  worstdf = data.frame()
  while (pmax > pval) {
    logit_model = train(
      final_result ~ .,
      data = train2,
      trControl = ctrl,
      method = "multinom"
    )
    temp = data.frame(summary(logit_model)$coefficients)
    print(temp)
    worst = temp[temp$Pr...z.. == max(summary(logit_model)$coefficients[, 4]), ]
    print(worst)
    if (worst$Pr...z.. > 0.1) {
      col1 = rownames(worst)
      pmax = worst$Pr...z..
      worstdf = rbind(worstdf, worst)
      train2[col1] = NULL
    } else {
      pmax = 0
    }
  }
  return (list(data = train2, removed = worstdf))
}
try1 = LogitBackward(train, pval = 0.1)


#Now that we know that all the selected variables are relevant. We then start our modelling with the basic logit

################LOGIT################

# library(DMwR)
#
# set.seed(9560)
# smote_train <- SMOTE(final_result ~ ., data  = train)
# table(smote_train$final_result)
# table(train$final_result)

logit <- train(
  final_result ~ .,
  # model specification
  data = train,
  # training set used to build model
  method = "multinom",
  # type of model you want to build
  trControl = ctrl,
  weights = model_weights,
  # how you want to learn
  metric = "logLoss"
)     # performance measure

logit

# logit model preds
trP_logit <- predict(logit, newdata = train, type = 'prob')[, 1]
trC_logit <- predict(logit, newdata = train)
teP_logit <- predict(logit, newdata = test, type = 'prob')[, 1]
teC_logit <- predict(logit, newdata = test)

# logit confusion matrix
(trcm_logit <-
    confusionMatrix(data = trC_logit, train$final_result))
(tecm_logit <- confusionMatrix(data = teC_logit, test$final_result))





################ Random Forest ################

rf <- train(
  final_result ~ .,
  # model specification
  data = train,
  # training set used to build model
  method = "rf",
  # type of model you want to build
  trControl = ctrl,
  # how you want to learn
  metric = "logLoss",
  weights = model_weights,
  # performance measure
  tuneLength = 7,
  # tries 15 combinations of tuning parameters
  tuneGrid = expand.grid(mtry = c(3:9)) # tuning parameters to try
)

rf

# random forest model preds
trP_rf <- predict(rf, newdata = train, type = 'prob')[, 1]
trC_rf <- predict(rf, newdata = train)
teP_rf <- predict(rf, newdata = test, type = 'prob')[, 1]
teC_rf <- predict(rf, newdata = test)

# rf confusion matrix
(trcm_rf <- confusionMatrix(data = trC_rf, train$final_result))
(tecm_rf <- confusionMatrix(data = teC_rf, test$final_result))


################ GBM ################

gbm <-
  train(
    final_result ~ .,
    # model specification
    data = train,
    # training set used to build model
    method = "gbm",
    # type of model you want to build
    trControl = ctrl,
    # how you want to learn
    metric = "logLoss",
    weights = model_weights,
    # performance measure
    tuneLength = 80,
    # boosts 15 times
    tuneGrid = expand.grid(
      n.trees = c(1, 2, 3, 4, 5, 6, 7, 8),
      # no. of classifiers
      interaction.depth = c(2, 3, 4, 5, 6, 7, 8),
      # max depth of tree
      shrinkage = c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5),
      # shrinkage parameter
      n.minobsinnode = c(5, 6, 7, 8, 9, 10, 11, 12)
    )
  )
gbm

# gbm model preds
trP_gbm <- predict(gbm, newdata = train, type = 'prob')[, 1]
trC_gbm <- predict(gbm, newdata = train)
teP_gbm <- predict(gbm, newdata = test, type = 'prob')[, 1]
teC_gbm <- predict(gbm, newdata = test)


# gbm radial
(trcm_gbm <- confusionMatrix(data = trC_gbm, train$final_result))
(tecm_gbm <- confusionMatrix(data = teC_gbm, test$final_result))


############################## Create Best Model #######################

# AutoML to get the best performing model
#
library(h2o)
h2o.init()
x_data1 <- as.h2o(x_data)
y <- "final_result"
x <- setdiff(names(x_data1), y)

split <- h2o.splitFrame(x_data1, ratios = 0.8, seed = 42)
train <- split[[1]]
test <- split[[2]]
auto <-
  h2o.automl(
    x,
    y,
    train,
    max_runtime_secs = 1200,
    max_models = 50,
    balance_classes = T,
    exclude_algos = 'DeepLearning'
  )

lb <- h2o.get_leaderboard(auto, extra_columns = "ALL")
print(lb)
auto_champion = auto@leader
auto_champion
pred_class <- h2o.predict(auto_champion, test)
h2o.performance(auto_champion, train)
h2o.performance(auto_champion, test)


##################################################################THE END#####################################################################
