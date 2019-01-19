rm(list=ls())
# Set Working directory
setwd("C:/Users/jiere/Dropbox/Spring 2019/ECON 613/ECON613_HW")

# Reading the CSV files
datstu <- read.csv("datstu.csv",header = TRUE,na.strings=c("", "NA"))
datsss <- read.csv("datsss.csv",header = TRUE,na.strings=c("", "NA"))
datjsu <- read.csv("datjss.csv",header = TRUE,na.strings=c("", "NA"))

# Exercise 1 ----
# Number of student
stuNum <-length(datstu$X)

# Number of school 
school <- as.vector(as.matrix(datstu[,5:10]))
schoolNum <- length(na.omit(unique(school)))

# Number of program
program <- as.vector(as.matrix(datstu[,11:16]))
programNum <- length(na.omit(unique(program)))


# Number of Choice
choice <- na.omit(unique(cbind(school, program)))
choice <- as.data.frame(choice)
choiceNum <- length(choice)


# Number of NA's in the score
missing <- sum(is.na(datstu$score))

# Apply to the same school
index <- rep(1:stuNum,6) # creating a index column, since "school" goes by person
app <- na.omit(unique(cbind(index,school))) #combine with school directly,and take out repeat and NA's 
app <- as.data.frame(app) #conforming the format
# app <- app[order(app[,2]),] # if you want to rank the result
sameSchool <- table(app[,2])

# Num of Students who apply for less than 6
lessThan_6 <- sum(is.na(datstu$schoolcode6)) #by observing, only need to look for student who didn't fill the schoolcode6

# Exercise 2 ----
# Cleaning datsss
datsss$X <- NULL

# removing those repeating school names with numbers and NAs
datsss_u <- datsss[order(datsss[,1]),]
datsss_u <- datsss_u[!grepl("[[:digit:]]{5}",datsss_u$schoolname),]
datsss_u <- na.omit(datsss_u)
datsss_u <- datsss_u[!duplicated(datsss_u$schoolcode),]

# Merging
choice <- choice[order(choice[,1]),] # ranking for observation
schoolLevel <- merge(choice,datsss_u,by.x="school",by.y="schoolcode")

# Adding admission result (I noticed that there are programs who did't admit student)
datstu_c <- datstu[!(is.na(datstu$rankplace)|datstu$rankplace == 99),] # clean NA and 99 in datstu
datstu_c$resultSchool <- datstu_c[cbind(seq_along(datstu_c$rankplace),datstu_c$rankplace+4)]
datstu_c$resultProgram <- datstu_c[cbind(seq_along(datstu_c$rankplace),datstu_c$rankplace+10)]
datstu_c$resultSchool <- as.integer(datstu_c$resultSchool)


# tapply function
quality <- aggregate(datstu_c$score,list(datstu_c$resultSchool,datstu_c$resultProgram),mean)
cutoff <- aggregate(datstu_c$score,list(datstu_c$resultSchool,datstu_c$resultProgram),min)
size <- aggregate(datstu_c$score,list(datstu_c$resultSchool,datstu_c$resultProgram),length)


schoolLevel <- merge(schoolLevel,quality,by.x=c("school","program"),by.y=c("Group.1","Group.2"))
schoolLevel <- merge(schoolLevel,cutoff,by.x=c("school","program"),by.y=c("Group.1","Group.2"))
schoolLevel <- merge(schoolLevel,size,by.x=c("school","program"),by.y=c("Group.1","Group.2"))

names(schoolLevel)[7] <- "quality"
names(schoolLevel)[8] <- "cutoff"
names(schoolLevel)[9] <- "size"

# Exercise 3 ----
# clean datjsu
datjsu <- na.omit(datjsu)
datjsu$X <- NULL

# adding the jss/sss 's long and lat
datstu_c <- merge(datstu_c,datsss_u,by.x="resultSchool",by.y="schoolcode")
datstu_c <- merge(datstu_c,datjsu,by.x ="jssdistrict",by.y = "jssdistrict")
names(datstu_c)[names(datstu_c)=="schoolname"] <- "highSchoolName"
names(datstu_c)[names(datstu_c)=="point_x"] <- "jsslong"
names(datstu_c)[names(datstu_c)=="point_y"] <- "jsslat"

#Calculating the dist
distance<- function(ssslong,ssslat,jsslong,jsslat){
  dist <- sqrt((69.172*(ssslong - jsslong)*cos(jsslat/57.3))^2+ (69.172*(ssslat-jsslat))^2)
  return(dist)
  }
datstu_c$dist <- distance(datstu_c$ssslong,datstu_c$ssslat,datstu_c$jsslong,datstu_c$jsslat)

# Exercise 4 ----
# For each ranked choice (can not understand the meaning of the question)
rankedDescrip <- aggregate(datstu_c$score,list(datstu_c$rankplace),min)
rankedDescrip <- aggregate(datstu_c$score,list(datstu_c$rankplace),mean)
rankedDescrip <- aggregate(datstu_c$score,list(datstu_c$rankplace),min)

#For each score quantiles





# levels(datstu$jssdistrict)




