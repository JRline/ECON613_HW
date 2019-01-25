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
choice <- as.data.frame(na.omit(unique(cbind(school, program))))
choiceNum <- nrow(choice)

# Number of NA's in the score
missing <- sum(is.na(datstu$score))

# Apply to the same school
app <- na.omit((cbind(rep(1:stuNum,6),school))) #combine with school directly,and take out repeat and NA's 
# app <- app[order(app[,2]),] # if you want to rank the result
sameSchool <- as.data.frame(table(app[,2]))

# Num of Students who apply for less than 6
lessThan_6 <- sum(apply(datstu[,c(5:10)],1,is.na))

# Exercise 2 ----
# Cleaning datsss
datsss$X <- NULL

# removing those repeating school names with numbers and NAs
datsss <- datsss[order(datsss[,1]),]
datsss <- na.omit((datsss[!grepl("[[:digit:]]{5}",datsss$schoolname),])) # take out those school names with school code mixed in
datsss <- datsss[!duplicated(datsss$schoolcode),] # take out NA and repeat

# Merging (add s.distric and s.location to choice)
choice <- choice[order(choice[,1]),] # ranking for observation
schoolLevel <- merge(choice,datsss,by.x="school",by.y="schoolcode")

# Adding admission result (NOTE: there are schools not admitting students,like 10123)
datstu_c <- datstu[!(is.na(datstu$rankplace)|datstu$rankplace == 99),] # clean NA and 99 in datstu 
# (datstu_c only has admitted students) (selecing 99 only should use which)
datstu_c$resultSchool <- datstu_c[cbind(seq_along(datstu_c$rankplace),datstu_c$rankplace+4)] # add result school
datstu_c$resultProgram <- datstu_c[cbind(seq_along(datstu_c$rankplace),datstu_c$rankplace+10)] # add result program
datstu_c$resultSchool <- as.integer(datstu_c$resultSchool) # change the school code from character to integer for future comparison


# tapply function (qualtiy, cutoff, size by school and program)
quality <- aggregate(datstu_c$score,list(datstu_c$resultSchool,datstu_c$resultProgram),mean) # same as tapply but has output as data.frame
cutoff <- aggregate(datstu_c$score,list(datstu_c$resultSchool,datstu_c$resultProgram),min)
size <- aggregate(datstu_c$score,list(datstu_c$resultSchool,datstu_c$resultProgram),length)

# Merging the result back to schoollevel
schoolLevel <- merge(schoolLevel,quality,by.x=c("school","program"),by.y=c("Group.1","Group.2"))
schoolLevel <- merge(schoolLevel,cutoff,by.x=c("school","program"),by.y=c("Group.1","Group.2"))
schoolLevel <- merge(schoolLevel,size,by.x=c("school","program"),by.y=c("Group.1","Group.2"))

names(schoolLevel)[7] <- "quality"
names(schoolLevel)[8] <- "cutoff"
names(schoolLevel)[9] <- "size"

# Exercise 3 ----
# Cleaning datjsu
datjsu <- na.omit(datjsu)
datjsu$X <- NULL

# adding the jss/sss 's long and lat
datstu_c <- merge(datstu_c,datsss,by.x="resultSchool",by.y="schoolcode")
datstu_c <- merge(datstu_c,datjsu,by.x ="jssdistrict",by.y = "jssdistrict") # one student don't have jss input
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
school_c <- as.vector(as.matrix(datstu_c[,7:12]))
program_c <- as.vector(as.matrix(datstu_c[,13:18]))

rankedChoice<- as.data.frame(unique(cbind(school_c, program_c,datstu_c$dist,rep(1:6,each = length(datstu_c$X)))))
names(rankedChoice)[3] <- "dist"
names(rankedChoice)[4] <- "rank"
rankedChoice <- merge(rankedChoice, schoolLevel[,c(1:2,7:9)], by.x=c("program_c","school_c"),by.y=c("program","school"),sort=FALSE)
rankedChoice$dist <- as.numeric(as.character(rankedChoice$dist)) # Data formating issue

#calculate desciptive
Descriptive1 <- aggregate(rankedChoice$cutoff,list(rankedChoice$rank),mean)
names(Descriptive1)[2] <- "Cutoff_Average"
Descriptive1$Cutoff_sd <- aggregate(rankedChoice$cutoff,list(rankedChoice$rank),sd)[,2]
Descriptive1$Quality_Average <- aggregate(rankedChoice$quality,list(rankedChoice$rank),mean)[,2]
Descriptive1$Quality_sd <- aggregate(rankedChoice$quality,list(rankedChoice$rank),sd)[,2]
Descriptive1$Dist_Average <- aggregate(as.numeric(rankedChoice$dist),list(rankedChoice$rank),mean)[,2]
Descriptive1$Dist_sd <- aggregate(as.numeric(rankedChoice$dist),list(rankedChoice$rank),sd)[,2]

# For each score quantiles
# Add quantiles
datstu_c$score_quantile <- cut(datstu_c$score, quantile(datstu_c$score),labels = c("1", "2", "3", "4"),include.lowest=TRUE)
datstu_c <- merge(datstu_c, schoolLevel[,c(1:2,7:8)], by.x=c("resultProgram","resultSchool"),by.y=c("program","school"),sort=FALSE)

#calculate desciptive 
Descriptive2 <- aggregate(datstu_c$cutoff,list(datstu_c$score_quantile),mean)
names(Descriptive2)[2] <- "Cutoff_Average"
Descriptive2$Cutoff_sd <- aggregate(datstu_c$cutoff,list(datstu_c$score_quantile),sd)[,2]
Descriptive2$Quality_Average <- aggregate(datstu_c$quality,list(datstu_c$score_quantile),mean)[,2]
Descriptive2$Quality_sd <- aggregate(datstu_c$quality,list(datstu_c$score_quantile),sd)[,2]
Descriptive2$Dist_Average <- aggregate(datstu_c$dist,list(datstu_c$score_quantile),mean)[,2]
Descriptive2$Dist_sd <- aggregate(datstu_c$dist,list(datstu_c$score_quantile),sd)[,2]

# Exercise 5 ----
#By selectivity (I can't obtain the selectivity info for school who are not admitting, so they left out as NA)
schoolLevel$selectivity_cutoff <- cut(schoolLevel$cutoff, quantile(schoolLevel$cutoff,prob = seq(0, 1, length = 11), type = 5),
                                      labels = c("1","2","3","4","5","6","7","8","9","10"),include.lowest=TRUE)

for (i in 1:6) {
  datstu_c <- merge(datstu_c,schoolLevel[,c(1:2,10)],by.x=c(paste("schoolcode",i,sep = ""),paste("choicepgm",i,sep = "")),
                    by.y = c("school","program"),all.x = TRUE,sort = FALSE)
  names(datstu_c)[ncol(datstu_c)]<- paste("selectivity_c",i,sep = "")
}
datstu_c$groups_cutoff <- apply(datstu_c[,c((ncol(datstu_c)-5):ncol(datstu_c))],1,function(x)length(unique(na.omit(x))))

# "Redo this, by student test score (quantile)" 
schoolLevel$selectivity_quality <- cut(schoolLevel$quality, quantile(schoolLevel$quality,prob = seq(0, 1, length = 11), type = 5),
                                      labels = c("1","2","3","4","5","6","7","8","9","10"),include.lowest=TRUE)

for (i in 1:6) {
  datstu_c <- merge(datstu_c,schoolLevel[,c(1:2,11)],by.x=c(paste("schoolcode",i,sep = ""),paste("choicepgm",i,sep = "")),
                    by.y = c("school","program"),all.x = TRUE,sort = FALSE)
  names(datstu_c)[ncol(datstu_c)]<- paste("selectivity_q",i,sep = "")
}
datstu_c$groups_quality <- apply(datstu_c[,c((ncol(datstu_c)-5):ncol(datstu_c))],1,function(x)length(unique(na.omit(x))))
