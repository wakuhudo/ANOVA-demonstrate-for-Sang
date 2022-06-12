##This document demonstrates the 1st section of Statistic (difference between size ~ day + isolate + temp)

##We have 3 independent variables, namely Temp, Date and Isolate, two-way (or in fact 3-way) ANOVA can be used to determine
  # 1) if means of factor Temp has no difference 
  # 2) if means of factor Date has no difference 
  # 3) if means of factor Isolate has no difference 
  # 4) if there is no interaction between factor Temp and Date 
  # 5) ..........................................Temp and Isolate 
  # 6) etc. interaction between any pair of factors 

# Using dataset "average 1.xlsx
# datasheet can be converted to master spreadsheet by incorporating Isolate into a column, by simply reading 5 files, and rbind them  
my_data1 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
my_data1$isolate <- "A"

my_data2 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
my_data2$isolate <- "B"

my_data3 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
my_data3$isolate <- "C"

my_data4 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
my_data4$isolate <- "D"

my_data5 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
my_data5$isolate <- "E"

my_data <- rbind(my_data1, my_data2, my_data3, my_data4, my_data5)

write.csv(my_data, file ="/Users/Apple/Desktop/mydata.csv")
## check class for each variable, make independent variable character, dependent variable numerical 

class(my_data$x1)
my_data$x1 <- as.character(my_data$x1)

class(my_data$x2)

class(my_data$y)

class(my_data$isolate)

## check means for your data w/ interaction in factors: 

library("dplyr")
group_by(my_data, x1, x2, isolate) %>%
  summarise(
    count = n(),
    mean = mean(y, na.rm = TRUE),
    sd = sd(y, na.rm = TRUE)
  )


## run 2-way ANOVA

#this test for independently if any facotr is related to means
anov_indi <- aov(y ~ x1 + x2 + isolate, data = my_data)

summary(anov_indi)
# we see significance in x1 (Date),x2(temp), and isolate 

##look for interaction between any factors by: 
anov_test_inter <- aov(y ~ x1 + x2 + isolate + x1:x2 + x1:isolate + x2:isolate , data = my_data)

summary(anov_test_inter)

## we see significance in x1:x2 (Date and temp), x2:isolate (temp and isolate)

## now recheck means w/ above section or use base R:
model.tables(anov_test_inter, type="means", se = TRUE)

## hence we can conclude that the mean of growth rates is related to date, temp, isolate, interactions between date and temp, and date and isolate 
## we fail to conclude that there is an optimal temp across date and isolate 

