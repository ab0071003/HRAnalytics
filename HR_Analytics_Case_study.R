#HR Analytics Case Study
#Brief: Binary classification Model in the domain of HR Analytics

#Steps followed during the execution of this Case Study/Project:
#[I] Business Understanding
#[II] Data Understanding
#[III] Data Exploration and Preparation 
#[IV] Model Building
#[V] Model Evaluation, Interpretation of results and Deployment


# [I] Business Understanding:
#A large company 'XYZ', provides employment for approximately 4000 individuals. However, in recent years it is faced with the challenge of high annual attrition rate of approximately 15%.
#This adversely impacts the business and reputation of the organization in the following ways:
#[1] It causes delay in completion of projects formerly undertaken by employees who later attrition making it challenging to meet timelines. This harms the reputation of the organization leading to loss in customer and market share.
#[2] Investment of resources and manpower to maintain a department focused on sourcing and hiring new talent to fill abruptly vacated positions in the organization.  
#[3] Requirement to train newly acquired talent to perform effectively in the organization. Delays caused due to time allocated for newly acquired talent for acclimatizing in the new work environment.

# Goals of the Case Study
#[1] Determine and understand the driver variables influencing high attrition rate within the organization.
#[2] Build a model to estimate the probability of an employee to attrition.
#[3] Provide actionable insights for inducing changes within the organization so as to curb attrition and encourage employees to stay.

# Install and Load the Required Packages
#install.packages("MASS")
#install.packages("carData")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")

library(MASS)
library(carData)
library(e1071)
library(caret)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(caTools)
library(GGally)
library(gridExtra)
#Setting Working Directory

#setwd("D:/upgrad/HR Analytics Case Study")

##[II] Data Understanding
#Importing the datasets into R-environment
general_data <- read.csv("general_data.csv")
employee_survey <- read.csv("employee_survey_data.csv")
manager_survey <- read.csv("manager_survey_data.csv")
in_time <- read.csv("in_time.csv")
out_time <- read.csv("out_time.csv")

#There are five tables available for analysis details of which are given below:
#Each record can be traced through a unique EmployeeID which will serve as the primary key for these table.
#[1] general_data<- This contains personal details, demographics, educational data, career relevant information and wages for each employee. 
#[2] employee_survey<- This contains information regarding a few scaled categorical parameters which were filled by the employees within the organization. 
#[3] manager_survey<- This contains information regarding scaled parameters used by the managers within the organization to rate the employees
#[4] in_time<- This contains time and date information for the year 2015 registered when the employee began work for the day
#[5] out_time<- This contains time and date information for the year 2015 registered when the employee ended work for the day

#Understanding the structure of the dataset:
str(general_data)    #4410 obs. of  24 variables including the target variable Attrition
str(employee_survey) #4410 obs. of  4 variables
str(manager_survey)  #4410 obs. of  3 variables
#The structure of in_punch and out_punch reveal that the first column in Employee ID followed by date and time information for the working days between 1st January 2015 to 19th May 2015.
#They each contain 4410 obs. with 261 days of timestamp data.
colnames(in_time)[1]<- "EmployeeID"
colnames(out_time)[1]<- "EmployeeID"

#Refer Data Dictionary given below:
#[1] Age=Age of the employee
#[2] Attrition=Whether the employee left in the previous year or not
#[3] BusinessTravel=How frequently the employees travelled for business purposes in the last year
#[4] Department=Department in company
#[5] DistanceFromHome=Distance from home in kms
#[6] Education=Education Level : 1=Below College | 2=College| 3=Bachelor| 4=Master| 5=Doctor
#[7] EducationField=Field of education
#[8] EmployeeCount=Employee count
#[9] EmployeeNumber=Employee number/id
#[10] EnvironmentSatisfaction=Work Environment Satisfaction Level : 1=Low | 2=Medium | 3=High | 4=Very High
#[11] Gender=Gender of employee
#[12] JobInvolvement=Job Involvement Level
#[13] JobLevel=Job level at company on a scale of 1 to 5
#[14] JobRole=Name of job role in company
#[15] JobSatisfaction=Job Satisfaction Level : 1=Low | 2=Medium | 3=High | 4=Very High
#[16] MaritalStatus=Marital status of the employee
#[17] MonthlyIncome=Monthly income in rupees per month
#[18] NumCompaniesWorked=Total number of companies the employee has worked for
#[19] Over18=Whether the employee is above 18 years of age or not
#[20] PercentSalaryHike=Percent salary hike for last year
#[21] PerformanceRating=Performance rating for last year: 1=Low | 2=Good | 3=Excellent | 4=Outstanding
#[22] RelationshipSatisfaction=Relationship satisfaction level : 1=Low | 2=Medium | 3=High | 4=Very High
#[23] StandardHours=Standard hours of work for the employee
#[24] StockOptionLevel=Stock option level of the employee
#[25] TotalWorkingYears=Total number of years the employee has worked so far
#[26] TrainingTimesLastYear=Number of times training was conducted for this employee last year
#[27] WorkLifeBalance=Work life balance level : 1=Bad | 2=Good | 3=Better | 4=Best
#[28] YearsAtCompany=Total number of years spent at the company by the employee
#[29] YearsSinceLastPromotion=Number of years since last promotion
#[30] YearsWithCurrManager=Number of years under current manager

#Checking if EmployeeID is the primary key for all 5 tables
if(length(unique(employee_survey$EmployeeID))==nrow(employee_survey)){paste("EmployeeID is primary key for employee_survey Table")}
if(length(unique(general_data$EmployeeID))==nrow(general_data)){paste("EmployeeID is primary key for general_data Table")}
if(length(unique(manager_survey$EmployeeID))==nrow(manager_survey)){paste("EmployeeID is primary key for manager_survey Table")}
if(length(unique(in_time$EmployeeID))==nrow(in_time)){paste("EmployeeID is primary key for in_punch Table")}
if(length(unique(out_time$EmployeeID))==nrow(out_time)){paste("EmployeeID is primary key for out_punch Table")}

setdiff(general_data$EmployeeID, employee_survey$EmployeeID)
setdiff(general_data$EmployeeID, manager_survey$EmployeeID)
setdiff(general_data$EmployeeID, in_time$EmployeeID)
setdiff(general_data$EmployeeID, out_time$EmployeeID)
#Since Employee_ID is the primary key across all the tables and the setdiff() returns the value zero implying that the same set of 4410 Employee_ID are present in all the 5 tables.

#Checking for Missing Values or NA
sapply(employee_survey, function(x) sum(is.na(x)))
#There are 83 missing values in the employee_survey dataset 
sapply(general_data, function(x) sum(is.na(x)))
#There are 28 missing values in the general_survey dataset specifically in the NumCompaniesWorked and TotalWorkingYears columns.
sapply(manager_survey, function(x) sum(is.na(x)))
#There are no missing values in the manager_survey dataset

# Working on in_time and out_time #
# Lets check whether all column names match in both in_time and out_time are same
which(!colnames(in_time) == colnames(out_time))
# 0

# Now we have data of dates matched in in_time and out_time data frame

# Check uniqueness of employee IDs in both in_time and out_time
nrow(in_time) == length(unique(in_time$EmployeeID))
# TRUE
nrow(out_time) == length(unique(out_time$EmployeeID))
# TRUE

# Now lets check for which all days have NAs in in_time and out_time
# in_time data set
colnames(in_time)[which(sapply(in_time,function(x) sum(is.na(x))) == nrow(in_time))]
# [1] "2015.01.01" "2015.01.14" "2015.01.26" "2015.03.05" "2015.05.01" "2015.07.17" "2015.09.17" "2015.10.02" "2015.11.09" "2015.11.10"
# [11] "2015.11.11" "2015.12.25"
# out_time data set
colnames(out_time)[which(sapply(out_time,function(x) sum(is.na(x))) == nrow(out_time))]
# [1] "2015.01.01" "2015.01.14" "2015.01.26" "2015.03.05" "2015.05.01" "2015.07.17" "2015.09.17" "2015.10.02" "2015.11.09" "2015.11.10"
# [11] "2015.11.11" "2015.12.25"

#Data cleaning for in_out file
#Remove the holiday columns from "in_time" and "out_time" dataset
remove_holiday <- function(x){
  time_data <- subset(x, select = -c(X2015.01.01,
                                     X2015.01.14,
                                     X2015.01.26,
                                     X2015.03.05,
                                     X2015.05.01,
                                     X2015.07.17,
                                     X2015.09.17,
                                     X2015.10.02,
                                     X2015.11.09,
                                     X2015.11.10,
                                     X2015.11.11,
                                     X2015.12.25))
  return(time_data)
}

in_time <- remove_holiday(in_time)
out_time <- remove_holiday(out_time)

#----------------------------------------------------------------------------------#
#Data Cleaning
# Removing unnecessary columns as it carries the same data for all rows.
general_data$EmployeeCount <- NULL
general_data$Over18 <- NULL
general_data$StandardHours <- NULL

# Remove NA values from NumCompaniesWorked

# NAs have been replaced by correct values depending on the data.
# If the data doesn't match, then we have assumed that NumberCompaniesWorked is 1.
nas <- which(is.na(general_data$NumCompaniesWorked))
len <- length(nas)
for (i in 1:len)
{
  if(general_data$TotalWorkingYears[nas[i]] == general_data$YearsAtCompany[nas[i]] )
  {
    general_data$NumCompaniesWorked[nas[i]] = 0
  }
  else{
    general_data$NumCompaniesWorked[nas[i]] = 1
  }
}
sum(is.na(general_data))

nas <- which(is.na(general_data$TotalWorkingYears))
len <- length(nas)

for (i in 1:len)
{
  
  general_data$TotalWorkingYears[nas[i]] = general_data$YearsAtCompany[nas[i]]
  
}

sum(is.na(general_data))

#Data cleaning for Employee_survey data
# Getting NA positions in the below 3 vectors for the 3 columns.
nas_EnvSatisfy <- which(is.na(employee_survey$EnvironmentSatisfaction))
nas_JobSatisfy <- which(is.na(employee_survey$JobSatisfaction))
nas_WrkBal <- which(is.na(employee_survey$WorkLifeBalance))


# Verifying if the rows with NA values in the dataset has not more than one NA.
intersect(nas_EnvSatisfy,nas_JobSatisfy)
intersect(nas_EnvSatisfy,nas_WrkBal)
intersect(nas_JobSatisfy,nas_WrkBal)

len <- length(nas_EnvSatisfy)

for (i in 1: len)
{
  employee_survey$EnvironmentSatisfaction[nas_EnvSatisfy[i]] <- round((employee_survey$JobSatisfaction[nas_EnvSatisfy[i]] + employee_survey$WorkLifeBalance[nas_EnvSatisfy[i]])/2)
  
}
sum(is.na(employee_survey$EnvironmentSatisfaction))
# Removed the NAs from Environment Satisfaction column


len <- length(nas_JobSatisfy)
for (i in 1: len)
{
  employee_survey$JobSatisfaction[nas_JobSatisfy[i]] <- round((employee_survey$EnvironmentSatisfaction[nas_JobSatisfy[i]] + employee_survey$WorkLifeBalance[nas_JobSatisfy[i]])/2)
  
}
sum(is.na(employee_survey$JobSatisfaction))
# Removed the NAs from Job Satisfaction column


len <- length(nas_WrkBal)
for (i in 1: len)
{
  employee_survey$WorkLifeBalance[nas_WrkBal[i]] <- round((employee_survey$EnvironmentSatisfaction[nas_WrkBal[i]] + employee_survey$JobSatisfaction[nas_WrkBal[i]])/2)
  
}
sum(is.na(employee_survey$WorkLifeBalance))

sum(is.na(employee_survey))

# Converting data from in_time & out_time to timestamp.


for (i in 2: ncol(in_time))
{
  in_time[,i] <- as.POSIXct(as.character(in_time[,i]), format = "%Y-%m-%d %H:%M:%S")
}

for (i in 2: ncol(out_time))
{
  out_time[,i] <- as.POSIXct(as.character(out_time[,i]), format = "%Y-%m-%d %H:%M:%S")
}


#-------------------------------------------------------------------------------------#
#Derived columns and additional data cleaning
#Finding the No of hours for each day from in_time and out_time data


in_out <- data.frame()
in_out <- out_time - in_time
in_out$EmployeeID <- NULL
in_out <- cbind(in_time$EmployeeID,in_out)
str(in_out)
# Renaming the column name to EmployeeID
names(in_out)[names(in_out) == 'in_time$EmployeeID'] <- 'EmployeeID'

#Replacing NAs by 0 in in_out
for (i in 1:ncol(in_out)) {
  in_out[which(is.na(in_out[i])),i] <- 0
}

# Converting to numeric and rounding off.
for (i in 2:ncol(in_out)) {
  in_out[,i] <- as.numeric(in_out[,i])
  in_out[,i] <- round(in_out[,i],2)
}


#Renaming the column headers for in_out data frame
col_names <- colnames(in_time)
colnames(in_out)<- c(col_names)
str(in_out)
#Calculating Average time for each employee
in_out$Average_time_spent<-round(rowMeans(in_out[,2:ncol(in_out)]),2)

#Calculating no of working days & leaves

# Removed the Average timespent columns
in_out_2 <- in_out[,-c(251)]

# First column is EmployeeID, hence starting from 2
in_out_2[2:ncol(in_out_2)] <- ifelse(in_out_2[2:ncol(in_out_2)] >=8,1, ifelse(in_out_2[1:ncol(in_out_2)] >=4 & in_out_2[1:ncol(in_out_2)] <8, 0.5,0))


#Calculating Average time & leaves for each employee
in_out_2$Total_working_days <- ncol(in_out_2)-1 # -1 due to removal of EmployeeID
in_out_2$Total_days_worked <-rowSums(in_out_2[,2:(ncol(in_out_2)-1)])
in_out_2$Leaves <- in_out_2$Total_working_days - in_out_2$Total_days_worked
in_out_2 <- in_out_2[,c("EmployeeID","Total_days_worked","Leaves")]

in_out_final <- in_out[,c("EmployeeID","Average_time_spent")]
in_out_final <- merge(in_out_final,in_out_2,by="EmployeeID",all = T)

# Collate the data together in one single file
length(unique(general_data$EmployeeID))    # 4410, confirming EmployeeID is key 
length(unique(tolower(employee_survey$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(manager_survey$EmployeeID))) # 4410, confirming EmployeeID is key

setdiff(general_data$EmployeeID,employee_survey$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey$EmployeeID) # Identical customerID across these datasets

#Merging all data into single data frame
HR_data <- merge(general_data,employee_survey, by="EmployeeID", all = T)
HR_data <- merge(HR_data,manager_survey, by="EmployeeID", all = T)
HR_data <- merge(HR_data,in_out_final,by="EmployeeID",all = T)

### Data Preparation & Exploratory Data Analysis

# Understanding the structure of the collated file
str(HR_data) #4410 obs. of 29 variables;

sum(is.null(HR_data)) # Verified that the data contains no null values


# DistanceFromHome, MonthlyIncome, NumCompaniesWorked, PercentSalaryHike, TotalWorkingYears, TrainingTimesLastYear
# YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager are continuous
# PerformanceRating,RelationshipSatisfaction,WorkLifeBalance,JobSatisfaction, JobInvolvement, EnvironmentSatisfaction
# Education need to be changed from integer to categorical

# Barcharts for categorical features with stacked attrition information

p1 <- ggplot(HR_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar() + xlab("Business Travel")+ylab("Count") + ggtitle("Attrition & Business Travel")+theme(plot.title = element_text(hjust = 0.5)) + labs(fill="Attrition") + geom_text(stat='count', aes(label=..count..), vjust=-1) + theme_economist()
p2 <- ggplot(HR_data, aes(x=EducationField,fill=Attrition))+ geom_bar() + xlab("Education Field")+ylab("Count") + ggtitle("Attrition & Education Field")+theme(plot.title = element_text(hjust = 0.5)) + labs(fill="Attrition") + geom_text(stat='count', aes(label=..count..), vjust=-1) + theme_economist()
p3 <- ggplot(HR_data, aes(x=Gender,fill=Attrition))+ geom_bar() + xlab("Gender")+ylab("Count") + ggtitle("Attrition & Gender")+theme(plot.title = element_text(hjust = 0.5)) + labs(fill="Attrition") + geom_text(stat='count', aes(label=..count..), vjust=-1) + theme_economist()
p4 <- ggplot(HR_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar() + xlab("Marital Status")+ylab("Count") + ggtitle("Attrition & Marital Status")+theme(plot.title = element_text(hjust = 0.5)) + labs(fill="Attrition") + geom_text(stat='count', aes(label=..count..), vjust=-1) + theme_economist()

grid.arrange(p1,p2,p3,p4, nrow =2)

p5 <- ggplot(HR_data, aes(x=JobRole,fill=Attrition))+ geom_bar() + xlab("Job Role")+ylab("Count") + ggtitle("Attrition & Job Role")+theme(plot.title = element_text(hjust = 0.5)) + labs(fill="Attrition") + geom_text(stat='count', aes(label=..count..), vjust=-1) + theme_economist()
p6 <- ggplot(HR_data, aes(x=Department,fill=Attrition))+ geom_bar()+ xlab("Department")+ylab("Count") + ggtitle("Attrition & Department")+theme(plot.title = element_text(hjust = 0.5)) + labs(fill="Attrition") + geom_text(stat='count', aes(label=..count..), vjust=-1) + theme_economist()
grid.arrange(p5,p6, nrow =2)


#users with Travel_rarely(Business travel), Education field as Life Sciences & medical, Marital Status as Single,
#Job role as Research Scientist/Lab Technician/Sales Executive & Dept as Reasearch & Development are bound to Attrite more.

# Boxplots of numeric variables relative to Attrition status

p7 <- ggplot(HR_data, aes(x=Attrition,y=MonthlyIncome,fill=Attrition))+ geom_boxplot(width=0.1) +xlab("Attrition")+ylab("Monthly Income") + ggtitle("Attrition & Monthly Income")+theme(plot.title = element_text(hjust = 0.5)) + theme_economist()
p8 <- ggplot(HR_data, aes(x=Attrition,y=NumCompaniesWorked,fill=Attrition))+ geom_boxplot(width=0.1) +xlab("Attrition")+ylab("No. of Companies Worked") + ggtitle("Attrition & No. of Companies")+theme(plot.title = element_text(hjust = 0.5)) + theme_economist()
p9 <- ggplot(HR_data, aes(x=Attrition,y=PercentSalaryHike,fill=Attrition))+ geom_boxplot(width=0.1) +xlab("Attrition")+ylab("Percent Salary Hike") + ggtitle("Attrition & Percentage of Salary Hike")+theme(plot.title = element_text(hjust = 0.5)) + theme_economist()

grid.arrange(p7,p8,p9, ncol=3)

p10 <- ggplot(HR_data, aes(x=Attrition,y=YearsAtCompany,fill=Attrition))+ geom_boxplot(width=0.1) +xlab("Attrition")+ylab("Years at Company") + ggtitle("Attrition & Years at Company")+theme(plot.title = element_text(hjust = 0.5)) + theme_economist()
p11 <- ggplot(HR_data, aes(x=Attrition,y=YearsSinceLastPromotion,fill=Attrition))+ geom_boxplot(width=0.1) +xlab("Attrition")+ylab("Years since Last Promotion") + ggtitle("Attrition & Years since Last Promotion")+theme(plot.title = element_text(hjust = 0.5)) + theme_economist()
p12 <- ggplot(HR_data, aes(x=Attrition,y=YearsWithCurrManager,fill=Attrition))+ geom_boxplot(width=0.1) +xlab("Attrition")+ylab("Years with Current Manager") + ggtitle("Attrition & Years with Current Manager")+theme(plot.title = element_text(hjust = 0.5)) + theme_economist()

grid.arrange(p10,p11,p12, ncol=3)

# Correlation between numeric variables

ggpairs(HR_data[, c("MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike", "YearsAtCompany" , "YearsSinceLastPromotion" ,  "YearsWithCurrManager")])

#YearsAtCompany and YearsSinceLastPromotion are highly corelated : 0.618

################################################################
### Data Preparation

# Bringing the variables in the correct format for Education column
HR_data$Education[which(HR_data$Education==1)] <- "Below College"
HR_data$Education[which(HR_data$Education==2)] <- "College"
HR_data$Education[which(HR_data$Education==3)] <- "Bachelor"
HR_data$Education[which(HR_data$Education==4)] <- "Master"
HR_data$Education[which(HR_data$Education==5)] <- "Doctor"

# Bringing the variables in the correct format for EnvironmentSatisfaction column
HR_data$EnvironmentSatisfaction[which(HR_data$EnvironmentSatisfaction==1)] <- "Low"
HR_data$EnvironmentSatisfaction[which(HR_data$EnvironmentSatisfaction==2)] <- "Medium"
HR_data$EnvironmentSatisfaction[which(HR_data$EnvironmentSatisfaction==3)] <- "High"
HR_data$EnvironmentSatisfaction[which(HR_data$EnvironmentSatisfaction==4)] <- "Very High"

# Bringing the variables in the correct format for JobInvolvement column
HR_data$JobInvolvement[which(HR_data$JobInvolvement==1)] <- "Low"
HR_data$JobInvolvement[which(HR_data$JobInvolvement==2)] <- "Medium"
HR_data$JobInvolvement[which(HR_data$JobInvolvement==3)] <- "High"
HR_data$JobInvolvement[which(HR_data$JobInvolvement==4)] <- "Very High"

# Bringing the variables in the correct format for JobSatisfaction column
HR_data$JobSatisfaction[which(HR_data$JobSatisfaction==1)] <- "Low"
HR_data$JobSatisfaction[which(HR_data$JobSatisfaction==2)] <- "Medium"
HR_data$JobSatisfaction[which(HR_data$JobSatisfaction==3)] <- "High"
HR_data$JobSatisfaction[which(HR_data$JobSatisfaction==4)] <- "Very High"

# Bringing the variables in the correct format for PerformanceRating column
HR_data$PerformanceRating[which(HR_data$PerformanceRating==1)] <- "Low"
HR_data$PerformanceRating[which(HR_data$PerformanceRating==2)] <- "Good"
HR_data$PerformanceRating[which(HR_data$PerformanceRating==3)] <- "Excellent"
HR_data$PerformanceRating[which(HR_data$PerformanceRating==4)] <- "Outstanding"

# Bringing the variables in the correct format for WorkLifeBalance column
HR_data$WorkLifeBalance[which(HR_data$WorkLifeBalance==1)] <- "Bad"
HR_data$WorkLifeBalance[which(HR_data$WorkLifeBalance==2)] <- "Good"
HR_data$WorkLifeBalance[which(HR_data$WorkLifeBalance==3)] <- "Better"
HR_data$WorkLifeBalance[which(HR_data$WorkLifeBalance==4)] <- "Best"

################################################################
# Feature standardisation

# Normalising continuous features 
HR_data$Age <- scale(HR_data$Age)
HR_data$DistanceFromHome <- scale(HR_data$DistanceFromHome)
HR_data$MonthlyIncome <- scale(HR_data$MonthlyIncome)
HR_data$MonthlyIncome <- scale(HR_data$MonthlyIncome)
HR_data$NumCompaniesWorked <- scale(HR_data$NumCompaniesWorked)
HR_data$PercentSalaryHike <- scale(HR_data$PercentSalaryHike)
HR_data$TotalWorkingYears <- scale(HR_data$TotalWorkingYears)
HR_data$TrainingTimesLastYear <- scale(HR_data$TrainingTimesLastYear)
HR_data$YearsAtCompany <- scale(HR_data$YearsAtCompany)
HR_data$YearsSinceLastPromotion <- scale(HR_data$YearsSinceLastPromotion)
HR_data$YearsWithCurrManager <- scale(HR_data$YearsWithCurrManager)
HR_data$Total_days_worked <- scale(HR_data$Total_days_worked)
HR_data$Leaves <- scale(HR_data$Leaves)
HR_data$Average_time_spent <- scale(HR_data$Average_time_spent)

# converting target variable attrition from No/Yes character to factorwith levels 0/1 
HR_data$Attrition <- ifelse(HR_data$Attrition=="Yes",1,0)

#Checking Attrition rate of prospect Employee
Attrition_rate <- sum(HR_data$Attrition)/nrow(HR_data)*100
Attrition_rate #16.12% Attrition Rate

#Converting categorical variables with 2 levels to 0s and 1s
HR_data$Gender<- ifelse(HR_data$Gender=="Male",1,0)#Setting male to 1 and female to 0

# creating a dataframe of categorical features
Hr_data_chr<- HR_data[,c(4,5,7,8,10,11,12,16,22,23,24,25,26)]
str(Hr_data_chr)

# converting categorical attributes to factor
Hr_data_chr<- data.frame(sapply(Hr_data_chr, function(x) factor(x)))
str(Hr_data_chr)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(Hr_data_chr, 
                            function(x) data.frame(model.matrix(~x-1,data =Hr_data_chr))[,-1]))

# Final dataset
HR_data_final<- cbind(HR_data[,-c(4,5,7,8,10,11,12,16,22,23,24,25,26)],dummies) 
View(HR_data_final) #7032 obs. of  31 variables
write.csv(HR_data_final,"HR_data_final.csv")

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(HR_data_final$Attrition, SplitRatio = 0.7)

train = HR_data_final[indices,]

test = HR_data_final[!(indices),]

########################################################################
# Logistic Regression: 

#Getting the corelation matrix for reference
Corelation <- cor(HR_data_final)
write.csv(Corelation,"Corelation_matrix.csv")

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2135.7....31 coeff..nullDev 2728.0...resDev 2021.7

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)

#Excluding JobSatisfaction.xMedium

model_3 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_time_spent + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xBelow.College + Education.xCollege + 
                 EducationField.xMarketing + EducationField.xOther + JobLevel.x2 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
               data = train)

summary(model_3)
vif(model_3)

#Excluding EducationField.xMarketing because of high P value

model_4 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_time_spent + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xBelow.College + Education.xCollege + 
                 EducationField.xOther + JobLevel.x2 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
               data = train)

summary(model_4)
vif(model_4)

#Excluding Education.xBelow.College because of High P value

model_5 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                Average_time_spent + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.xCollege + 
                EducationField.xOther + JobLevel.x2 + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
              data = train)

summary(model_5)
vif(model_5)

##Excluding DistanceFromHome because of High P value

model_6 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                Average_time_spent + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.xCollege + 
                EducationField.xOther + JobLevel.x2 + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
              data = train)

summary(model_6)
vif(model_6)

##Excluding EducationField.xOther because of High P value

model_7 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_time_spent + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xCollege + 
                 JobLevel.x2 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
               data = train)

summary(model_7)
vif(model_7)

##Excluding StockOptionLevel.x1 because of High P value

model_8 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_time_spent + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xCollege + 
                 JobLevel.x2 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
               data = train)

summary(model_8)
vif(model_8)

##Excluding MonthlyIncome because of High P value

model_9 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_time_spent + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xCollege + 
                 JobLevel.x2 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
               data = train)

summary(model_9)
vif(model_9)

##Excluding YearsAtCompany because of High P value and pretty high VIF 4.858480

model_10 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_time_spent + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xCollege + 
                 JobLevel.x2 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
               data = train)

summary(model_10)
vif(model_10)

##Excluding Education.xCollege because of High P value

model_11 <- glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_time_spent + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobLevel.x2 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_11)
vif(model_11)

##Excluding EnvironmentSatisfaction.xVery.High because of High P value

model_12 <- glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_time_spent + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobLevel.x2 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_12)
vif(model_12)

##Excluding JobRole.xLaboratory.Technician because of High P value

model_13 <- glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_time_spent + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobLevel.x2 + 
                  JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_13)
vif(model_13)

##Excluding JobRole.xResearch.Scientist because of High P value

model_14 <- glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_time_spent + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobLevel.x2 + 
                  JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_14)
vif(model_14)

##Excluding JobLevel.x2 because of High P value

model_15 <- glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_time_spent + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales +
                  JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_15)
vif(model_15)

##Excluding JobRole.xResearch.Director because of High P value

model_16 <- glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_time_spent + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales +
                  JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_16)
vif(model_16)

##Excluding JobRole.xSales.Executive because of High P value

model_17 <- glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_time_spent + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_17)
vif(model_17)

##Excluding WorkLifeBalance.xBest because of High P value

model_18 <- glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_time_spent + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_18)
vif(model_18)

##Excluding WorkLifeBalance.xGood because of High P value

model_19 <- glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_time_spent + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter, family = "binomial", 
                data = train)

summary(model_19)
vif(model_19)

##Excluding WorkLifeBalance.xBetter because of High P value

model_20 <- glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_time_spent + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High, family = "binomial", 
                data = train)

summary(model_20)
vif(model_20)

#Can't remove BusinessTravel.xTravel_Frequently or BusinessTravel.xTravel_Rarely even though it has 
#high VIF value since they both have very high negative corelation -0.753091732

########################################################################
# With 15 significant variables in the model

final_model<- model_20

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Churn 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])
#if type='response' is noy used the output will be in log odds. since we need output in probability we are using this

# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_Attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_Attrition,test_pred_Attrition)


#######################################################################

test_pred_Attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

install.packages("e1071")
library(e1071)
install.packages("RcppRoll")
install.packages("ddalpha")
install.packages("DEoptimR")
install.packages("dimRed")
install.packages("gower")
install.packages("caret",
                 repos = "http://cran.r-project.org", 
                 dependencies = c("Depends", "Imports", "Suggests"))
library(caret)
test_conf <- confusionMatrix(test_actual_Attrition,test_pred_Attrition, positive = "Yes")
test_conf
#######################################################################

