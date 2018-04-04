getwd()
setwd("D:\\r\\think42 labs")

#Reading the dataset
studentp<-read.csv("student-performance.csv")

#Viewing the dataset
View(head(studentp))

#checking the dataset variables 
summary(studentp)

#checking the variables for missing values
apply(studentp,2,function(x){ sum(is.na(x))})
#dataset has no missing values

#checking the distribution of total marks
hist(studentp$Total.Marks)

#box plot for checking outliers
for (i in 1:ncol(studentp1)) {
if(is.numeric(studentp1[,i]))
{
  boxplot(studentp1[,i],main=paste("Boxplot of",names(studentp1)[i]))
}
}
#outliers are checked and nothing could be found

#removing variables which does not contribute based on hindsite
unique(studentp$Grade.Levels) #contains only one grade
levels(studentp$Nationality)  #contains only one nationality

studentp1<-studentp[,-c(2,3,4,5)]
#as this is a small dataset of only one section SectionID,EducationalStages,Grade-level ,Nationalityis removed

#checking the new dataset after removing the variables
View(studentp1)

#checking the correlation with numerical variables
a<-c(5,6,7,8,12)
cor(studentp1[,a])
#total marks has high corellation with visited resources and raised hands 

#there is a small typo in the gender and that makes it show the mm variable into 4 levels
#changing into just 2 levels male and female
levels(studentp1$mm)
studentp1$mm[studentp1$mm=="Female,"]="Female"
studentp1$mm[studentp1$mm=="Male,"]="Male"
#removing the  unused levels  in the view to avoid confusion
studentp1$mm<-factor(studentp1$mm)

#checking the mean of marks Genderwise
meang<-by(studentp1$Total.Marks,studentp1$mm,FUN = mean)
meang
barplot(meang,main = "Mean comparison between genders")
#we can see that the Female has a higher scoring percentage than males

#checking the mean of marks by topic
levels(studentp1$Topic)
meant<-by(studentp1$Total.Marks,studentp1$Topic,FUN = mean)
meant
barplot(meant,main = "Total marks scored topic wise")
max(meant)
meant1<-as.matrix(meant)
meant1
#From the initial analysis we can say Geology and Biology are high scoring topics

#checking the miniminum and maximum marks by topics
mint<-by(studentp1$Total.Marks,studentp1$Topic,FUN = min)
maxt<-by(studentp1$Total.Marks,studentp1$Topic,FUN = max)
mint<-as.matrix(mint)
mint
maxt<-as.matrix(maxt)
maxt
#After seeing the minimum marks of students of each topic is almost the same except geology
#which is atleast 20 marks more than the other topics
#So from initial analysis we can say people who took geology generally score more

#checking the mean of marks by Parent.responsible
levels(studentp1$Parent.responsible.for.student)
meanp<-by(studentp1$Total.Marks,studentp1$Parent.responsible.for.student,FUN = mean)
meanp
barplot(meanp,main = "Total marks scored topic wise")
meantp<-as.matrix(meanp)
meantp
#By initial analysis we can say students who have mom as their parents responsible tend to score more than the 
#students who had their fathers responsible

#now we devide raised hand into 4 catogories and check their means
#first is student who raised hand less than 25 times
d<-length(studentp1$Total.Marks[studentp1$Raised.hand<25])
mean(studentp1$Total.Marks[studentp1$Raised.hand<25])
min(studentp1$Total.Marks[studentp1$Raised.hand<25])
max(studentp1$Total.Marks[studentp1$Raised.hand<25])
#student who raised hand >25 & <50
length(studentp1$Total.Marks[studentp1$Raised.hand>=25& studentp1$Raised.hand<50])
mean(studentp1$Total.Marks[studentp1$Raised.hand>=25& studentp1$Raised.hand<50])
min(studentp1$Total.Marks[studentp1$Raised.hand>=25& studentp1$Raised.hand<50])
max(studentp1$Total.Marks[studentp1$Raised.hand>=25& studentp1$Raised.hand<50])
#student who raised hand >50 & <75
length(studentp1$Total.Marks[studentp1$Raised.hand>=50 & studentp1$Raised.hand<75])
mean(studentp1$Total.Marks[studentp1$Raised.hand>=50 & studentp1$Raised.hand<75])
min(studentp1$Total.Marks[studentp1$Raised.hand>=50 & studentp1$Raised.hand<75])
max(studentp1$Total.Marks[studentp1$Raised.hand>=50 & studentp1$Raised.hand<75])
#student who raised hand >=50 & <100
length(studentp1$Total.Marks[studentp1$Raised.hand>=75 & studentp1$Raised.hand<=100])
mean(studentp1$Total.Marks[studentp1$Raised.hand>=75 & studentp1$Raised.hand<=100])
min(studentp1$Total.Marks[studentp1$Raised.hand>=75 & studentp1$Raised.hand<=100])
max(studentp1$Total.Marks[studentp1$Raised.hand>=75 & studentp1$Raised.hand<=100])

#From this we can say that Students who ask more questions tend to score more marks
#and majority of students raised hands more than 75 times almost 35%



#we do the same analysis using visited resources

length(studentp1$Total.Marks[studentp1$Visited.resources<25])
mean(studentp1$Total.Marks[studentp1$Visited.resources<25])
min(studentp1$Total.Marks[studentp1$Visited.resources<25])
max(studentp1$Total.Marks[studentp1$Visited.resources<25])
#student who visited resources >25 & <50
length(studentp1$Total.Marks[studentp1$Visited.resources>=25& studentp1$Visited.resources<50])
mean(studentp1$Total.Marks[studentp1$Visited.resources>=25& studentp1$Visited.resources<50])
min(studentp1$Total.Marks[studentp1$Visited.resources>=25& studentp1$Visited.resources<50])
max(studentp1$Total.Marks[studentp1$Visited.resources>=25& studentp1$Visited.resources<50])
#student who visited resources >50 & <75
length(studentp1$Total.Marks[studentp1$Visited.resources>=50& studentp1$Visited.resources<75])
mean(studentp1$Total.Marks[studentp1$Visited.resources>=50& studentp1$Visited.resources<75])
min(studentp1$Total.Marks[studentp1$Visited.resources>=50& studentp1$Visited.resources<75])
max(studentp1$Total.Marks[studentp1$Visited.resources>=50& studentp1$Visited.resources<75])
#student who visited resources >=50 & <100
length(studentp1$Total.Marks[studentp1$Raised.hand>=75 & studentp1$Raised.hand<=100])
mean(studentp1$Total.Marks[studentp1$Raised.hand>=75 & studentp1$Raised.hand<=100])
min(studentp1$Total.Marks[studentp1$Raised.hand>=75 & studentp1$Raised.hand<=100])
max(studentp1$Total.Marks[studentp1$Raised.hand>=75 & studentp1$Raised.hand<=100])
#Students who raised hands more performed well


#In hindsight viewing announcements and discussion groups have very less corellation 
#but still we can do bivariate analysis between Viewing announcements to check what we can get out of the data
length(studentp1$Total.Marks[studentp1$Viewing.announcements<25])
mean(studentp1$Total.Marks[studentp1$Viewing.announcements<25])
min(studentp1$Total.Marks[studentp1$Viewing.announcements<25])
max(studentp1$Total.Marks[studentp1$Viewing.announcements<25])
#student who visited resources >25 & <50
length(studentp1$Total.Marks[studentp1$Viewing.announcements>=25& studentp1$Viewing.announcements<50])
mean(studentp1$Total.Marks[studentp1$Viewing.announcements>=25& studentp1$Viewing.announcements<50])
min(studentp1$Total.Marks[studentp1$Viewing.announcements>=25& studentp1$Viewing.announcements<50])
max(studentp1$Total.Marks[studentp1$Viewing.announcements>=25& studentp1$Viewing.announcements<50])
#student who visited resources >50 & <75
length(studentp1$Total.Marks[studentp1$Viewing.announcements>=50& studentp1$Viewing.announcements<75])
mean(studentp1$Total.Marks[studentp1$Viewing.announcements>=50& studentp1$Viewing.announcements<75])
mean(studentp1$Total.Marks[studentp1$Viewing.announcements>=50& studentp1$Viewing.announcements<75])
min(studentp1$Total.Marks[studentp1$Viewing.announcements>=50& studentp1$Viewing.announcements<75])
max(studentp1$Total.Marks[studentp1$Viewing.announcements>=50& studentp1$Viewing.announcements<75])
#student who visited resources >=50 & <100
length(studentp1$Total.Marks[studentp1$Viewing.announcements>=75 & studentp1$Viewing.announcements<=100])
mean(studentp1$Total.Marks[studentp1$Viewing.announcements>=75 & studentp1$Viewing.announcements<=100])
min(studentp1$Total.Marks[studentp1$Viewing.announcements>=75 & studentp1$Viewing.announcements<=100])
max(studentp1$Total.Marks[studentp1$Viewing.announcements>=75 & studentp1$Viewing.announcements<=100])


#checking the analysis wit parent school satisfaction
levels(studentp1$ParentschoolSatisfaction)
meanps<-by(studentp1$Total.Marks,studentp1$ParentschoolSatisfaction,FUN = mean)
meanps
barplot(meanps,main = "Total marks scored topic wise")
meantps<-as.matrix(meanps)
meantps
#well parents whose children scored less marks had a low satisfaction

#checking the analysis wit parent student absent 
levels(studentp1$StudentAbsenceDays)
meanabs<-by(studentp1$Total.Marks,studentp1$StudentAbsenceDays,FUN = mean)
meanabs
barplot(meanabs,main = "Total marks scored with absentism")
meanabs<-as.matrix(meanabs)
meanabs
#more often absent students scored less marks



#now lets build a linear Regression model based on the features so that we can predict if a student will score how many marks
step(lm(Total.Marks~.,data = studentp1),direction = "backward")
step(lm(Total.Marks~.,data = studentp1),direction = "forward")
step(lm(Total.Marks~.,data = studentp1),direction = "both")
#after doing stepwiseselection we take into consideration 7 variables which are significant
fit1<-lm(Total.Marks ~ mm + Parent.responsible.for.student + Raised.hand + 
           Visited.resources + Discussion.groups + Parent.Answering.Survey + 
           StudentAbsenceDays,data = studentp1)

plot(fit1)
summary(fit1)

library(car)
library(DAAG)
fit1cv<-cv.lm(fit1,data = studentp1,m=10)
summary(fit1cv)
class(fit1cv)
View(fit1cv)
fit1cv$totcat<-ifelse(fit1cv$Total.Marks>=0 & fit1cv$Total.Marks<=69,"Low-Level", 
                      ifelse(fit1cv$Total.Marks>=70 & fit1cv$Total.Marks<=89,"Middle-Level","High-Level"))

fit1cv$cvpredcat<-ifelse(fit1cv$cvpred>=0 & fit1cv$cvpred<=69,"Low-Level", 
ifelse(fit1cv$cvpred>=70 & fit1cv$cvpred<=89,"Middle-Level","High-Level"))

library(caret)
confusionMatrix(data = fit1cv$totcat,fit1cv$cvpredcat)
# the overall is accuracy is predicted by categorising the variables into 3 categories after prediction

