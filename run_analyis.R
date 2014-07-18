#Loading and preprocessing the data

#Checking if the file activity.csv or activity.zip exist in the working directory
#If all of them are missing then it will print the message
#If only "activity.zip" is found it will be unzipped

if (!file.exists("activity.csv")) {
  if (!file.exists("activity.zip")) {
    stop("Please make sure the file 'activity.csv' or 'activity.zip' file is in the working directory")
  } else {
    unzip("activity.zip")
  }
}

#Loading the data
activity <- read.csv("activity.csv", colClasses=c(NA,"Date",NA))

#Checking the loaded the data
head(activity)

#Checking the dimension of the dat
dim(activity)

#Check the structure of the data
str(activity)

#What is mean total number of steps taken per day?

#The codes below will check if the package is installed and load, and if it is not installed it is going 
#download plyr, install and load it
#
if(require("plyr")){
  print("plyr is loaded correctly")
} else {
  print("trying to install plyr")
  install.packages("plyr")
  if(require(plyr)){
    print("plyr installed and loaded")
  } else {
    stop("could not install plyr")
  }
}



total_steps_day <- ddply(activity, .(date), summarise,
                      total_s = sum(steps))
head(total_steps_day)
summary(total_steps_day$total_s)


#Histogram of the total number of steps taken each day
hist(total_steps_day$total_s, 
     main = "Total number of steps taken each day",
     xlab = "Total Steps")


#The mean and median total number of steps taken per day
#mean_median_steps_day <- ddply(activity, .(date), summarise,
#                        mean_s = mean(steps) ,  
#                        median_s = median(steps))

#The mean and median total number of steps taken per day
#summary(total_steps_day$total_s)

summary(total_steps_day$total_s)

#What is the average daily activity pattern?

#1. What is mean total number of steps taken per day?

#Make a time series plot (i.e. type = "l") of the 5-minute interval 
#(x-axis) and the average number of steps taken, averaged across all 
#days (y-axis)
      
average_steps_day <- ddply(activity, .(interval), 
                           summarise,
                         average_s = mean(steps, na.rm = TRUE))


with(data=average_steps_day, 
      plot(interval, average_s, type="l",
      main = "mean total number of steps taken per day",
      ylab = "Average Steps"))


       

#2. Which 5-minute interval, on average across all the days in the dataset, 
#   contains the maximum number of steps?

mx <- max(average_steps_day$average_s)
#ttt <- average_steps_day[average_steps_day$average_s==mx,]
ttt <- average_steps_day[average_steps_day$average_s==mx,1]
ttt

#abline(v=max(average_steps_day$average_s))          
# Vertical line at x = 0
abline(v=max(average_steps_day[average_steps_day$average_s==mx,1]),
       col="blue") 


#Imputing missing values

# Visualizing missing values with "VIM" package
# Idea borrowed from 
#http://www.unt.edu/rss/class/Jon/Benchmarks/MissingValueImputation_JDS_Nov2010.pdf

if(require("VIM")){
  print("VIM is loaded correctly")
} else {
  print("trying to install VIM")
  install.packages("VIM")
  if(require(VIM)){
    print("plyr installed and loaded")
  } else {
    stop("could not install VIM")
  }
}


a <- aggr(activity)
a
 

#Counting the occurrence of NA
sum(is.na(activity))

## Counting incomplete cases, (rows of a data frame where one or more columns contain NA)
#Borrowed from http://forums.psy.ed.ac.uk/R/P01582/essential-10/

#Count of complete cases in a data frame named 'data'
sum(complete.cases(activity))

# Count of incomplete cases
sum(!complete.cases(activity))

# Which cases (row numbers) are incomplete?
which(!complete.cases(activity))

#3. Create a new dataset that is equal to the original dataset 
#but with the missing data filled in.

#Imputing NA's with mean of the interval
#Using impute() from package Hmisc and ddply from package plyr
#

#Loading the Hmisc package
if(require("Hmisc")){
  print("Hmisc is loaded correctly")
} else {
  print("trying to install Hmisc")
  install.packages("Hmisc")
  if(require(Hmisc)){
    print("Hmisc installed and loaded")
  } else {
    stop("could not install Hmisc")
  }
}

#df2 <- ddply(df, "site", mutate, imputed.value = impute(value, mean))

activity_imp <- ddply(activity, "interval", mutate, imputed.steps = impute(steps, mean))

#Sorting Data and arranging the column
#http://www.statmethods.net/management/sorting.html
activity_imp <- activity_imp[order(activity_imp$date),c(4,2,3)]
head(activity_imp)

#Make a histogram of the total number of steps taken each day and 
#Calculate and report the mean and median total number of steps 
#taken per day. Do these values differ from the estimates from 
#the first part of the assignment? What is the impact of imputing 
#missing data on the estimates of the total daily number of steps?

imp_total_steps_day <- ddply(activity_imp, .(date), summarise,
                         imp_total_s = sum(imputed.steps))
head(imp_total_steps_day)
summary(imp_total_steps_day$imp_total_s)

#Histogram of the total number of steps taken each day
hist(imp_total_steps_day$imp_total_s,
     main = "Total number of steps taken each day for Imputed Data",
     xlab = "Total Steps")

#Ploting the two histograms side by side
par(mfrow=c(1,2))
#Histogram of the total number of steps taken each day
hist(total_steps_day$total_s, 
     main = "NA's Ignored Data",
     xlab = "Total Steps")

#Histogram of the total number of steps taken each day for imputed data
hist(imp_total_steps_day$imp_total_s,
     main = "Imputed Data",
     xlab = "Total Steps")