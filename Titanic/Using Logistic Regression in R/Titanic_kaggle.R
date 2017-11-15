
train_titan<-read.csv("train.csv",header = TRUE)
test_titan<-read.csv("test.csv",header = TRUE)


train_titan$Survived <- as.factor(train_titan$Survived)

train_titan$Pclass <- as.factor(train_titan$Pclass)

train_titan$Name <- as.character(train_titan$Name)

train_titan$Ticket <- as.character(train_titan$Ticket)

train_titan$Cabin <- as.character(train_titan$Cabin)

#------------------

test_titan$Survived <- as.factor(test_titan$Survived)

test_titan$Pclass <- as.factor(test_titan$Pclass)

test_titan$Name <- as.character(test_titan$Name)

test_titan$Ticket <- as.character(test_titan$Ticket)

test_titan$Cabin <- as.character(test_titan$Cabin)

#--------------

table(train_titan$Survived)

prop.table(table(train_titan$Survived))

summary(train_titan$Age)

#-------------------Using Logistic Regression---------------

model1 <- glm(Survived ~ Pclass+Sex+Age,data = train_titan,family = binomial)

summary(model1)

test_titan$Survived <- predict(model1,newdata = test_titan,type = "response")

test_titan$Survival <- ifelse(test_titan$Survived>=0.5,1,0)

#There are many NA values in AGE column in both Train and Test datasets
#In this case, let us replace all the NA values by 0
test_titan$Survival[is.na(test_titan$Survival)]<-0

gender_submission<-data.frame(PassengerId=test_titan$PassengerId,Survived=test_titan$Survival)

write.csv(gender_submission, file = "submit1.csv", row.names = FALSE)



