# Path to directory

    setwd(C:\\Users\\me-ch\Downloads\\Graduate Courses\\Miscellaneous\\Data Science Specialization\\Reproducible Research)

## Loading and preprocessing the data

          library(ggplot2)               
          library(dplyr)
          library(knitr)
          library(lubridate) 

# <font size="1">Reading the downloaded csv file and Assigning it to a function</font>

    act <- read.csv("C://Users//me-ch//Downloads//Graduate Courses//Miscellaneous//Data Science Specialization//Reproducible Research//activity//activity.csv")

# <font size="1">Using knitr to change global default in order to overwrite individual chunk headers</font>

    knitr::opts_chunk$set(echo = TRUE, warning = FALSE, fig.width = 10, fig.height = 5,
                      fig.keep = 'all' ,fig.path = 'figures\ ', dev = 'png')


              tsteps <- act %>%
              group_by(date) %>%
              summarise(dsteps = sum(steps, na.rm = TRUE))
              
## What is mean total number of steps taken per day?
              
              ggplot(tsteps, aes(dsteps)) + geom_histogram(color="red", fill="white",binwidth = 2000) +
              xlab("Total Nmber of Steps Taken per Day") + 
              ylab("Frequency")

![Plot 1.](C:\\Users\\me-ch\Downloads\\Graduate Courses\\Miscellaneous\\Data Science Specialization\\Reproducible Research\\Rplot.png)


#<font size="1">Calculating mean for total number of steps taken per day</font>
              
    mean = mean(tsteps$dsteps, na.rm=TRUE)
  
#<font size="1">Calculating median for total number of steps taken per day</font>
  
    median = median(tsteps$dsteps, na.rm=TRUE)
  
## What is the average daily activity pattern?

#<font size="1">Creating a histogram of the total number of steps taken each day</font>
  
    intsteps <- act %>% 
    group_by(interval) %>%
    summarise(steps = mean(steps, na.rm =TRUE))
  
    ggplot(data=intsteps, aes(x=interval, y=steps)) +
    geom_line() + xlab("5-minute intervals") + ylab("Average number of steps taken")
  
![Plot 2.](C:\\Users\\me-ch\Downloads\\Graduate Courses\\Miscellaneous\\Data Science Specialization\\Reproducible Research\\Rplot.2.png)
## Imputing missing values
  
#<font size="1">Reporting the total number of missing values in the dataset</font>
 
    missing <- !complete.cases(act)

    impute_act <- act %>%
    mutate(steps = case_when(
    is.na(steps) ~ intsteps$steps[match(act$interval, intsteps$interval)], TRUE ~ as.numeric(steps)
    ))
 
    impute_tsteps <- impute_act %>% group_by(date) %>% summarise(dsteps = sum(steps))
  
    ggplot(impute_tsteps, aes(dsteps)) + 
    geom_histogram(color="blue", fill="white",binwidth = 2000) + 
    xlab("Total number of steps taken each day") + 
    ylab("Frequency")
  
![Plot 3.](C:\\Users\\me-ch\Downloads\\Graduate Courses\\Miscellaneous\\Data Science Specialization\\Reproducible Research\\Rplot.3.png)
  
#<font size="1">Calculating mean for total steps taken per day</font>
  
    imputemean = mean(impute_tsteps$dsteps, na.rm=TRUE)
  
#<font size="1">Calculating median for total steps taken per day</font>
  
    imputemedian = median(impute_tsteps$dsteps, na.rm=TRUE)
  
#<font size="1">Calculating the difference of the means and medians between imputed and original data to check if these values differ from the estimates from the first part of the assignment</font>
  
    mean_diff <- imputemean - mean
  
    median_diff <- imputemedian - median
  
#    <font size="1">The impact of imputing missing data on the estimates of the total daily number of steps is that the imputation switched the frequencies of the missing values, setting them around the means of the intervals across the day.</font>
  
## Are there differences in activity patterns between weekdays and weekends?
  
    day_of_week <- impute_act %>%
    mutate(
    date = ymd(date),
    weekday_or_weekend = case_when(wday(date) %in% 2:6 ~ "Weekday", wday(date) %in% c(1,7) ~ "Weekend")) %>% 
    select(-date) %>%
    group_by(interval, weekday_or_weekend) %>%
    summarise(steps = mean(steps)
    )
 
    ggplot(day_of_week, aes(interval, steps)) + 
    geom_line() + 
    facet_wrap(~weekday_or_weekend, nrow = 2) +
    xlab("5-Minute Intervals") + 
    ylab("Average Number of Steps Taken")
    
![Plot 4.](C:\\Users\\me-ch\Downloads\\Graduate Courses\\Miscellaneous\\Data Science Specialization\\Reproducible Research\\Rplot.4.png)
