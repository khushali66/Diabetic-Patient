#R_PROJECT (GROUP - KHUSHALI & SONALI)

# random forest algorithm - CLASSIFICATION and/or REGRESSION


library("ggplot2")
# Diabetes dataset to predict the patient is diabetic or not
path = "C:\\Users\\User\\Documents\\R_project\\diabetic_data.csv"
# diab = read.csv(d1, header=T, stringsAsFactors = T)
diab = read.csv(path, header=T)

View(diab)
nrow(diab)
ncol(diab)
head(diab$diag_1)
# check datatypes
str(diab)

# check for Nulls, Zeroes and blanks for all columns
col_name = colnames(diab) [apply(diab, 2, function(n) any(is.na(n)))]
#any is used for any value of nullin any position
if(length(col_name) > 0) print("NULLs present") else print("No NULLs")

col_name = colnames(diab) [apply(diab, 2, function(n) any(n == ""))]
if(length(col_name) > 0) print("Blanks present") else print("No Blanks")

col_name = colnames(diab) [apply(diab, 2, function(n) any(n==0))]
print(col_name)
if(length(col_name) > 0)
{
  print("Zeroes present in columns : ")
  print(col_name)
} else 
  print("No Zeroes")


col_name = colnames(diab) [apply(diab, 2, function(n) any(n=='?'))]
print(col_name)
if(length(col_name) > 0)
{
  print("? present in columns : ")
  print(col_name)
} else 
  print("No ?")


100*prop.table(table(diab$weight=='?'))

100*prop.table(table(diab$race=='?'))

# EDA 
#Race column
ggplot(diab,aes(x=diab$race,fill=diab$gender))+geom_bar()
levels(diab$race)
count(diab,'race')

fem=nrow(diab[diab$race=="Caucasian",])
tot=nrow(diab[diab$race,])


nrow(diab[diab$race=="?",])

#payer_code column
library(plyr)

levels(diab$payer_code)
table(diab$payer_code)
100*prop.table(table(diab$payer_code))

levels(diab$medical_specialty)
100*prop.table(table(diab$medical_specialty))


#Check count of '?'
c =0; i=1
while(i > 101766)
{
  if(diab$payer_code[i] == '?')
    c = c + 1
    #diab$payer_code[i] = 'BC'
    i = i+1
}
print(c)

#Replace payer_code with different levels

table(diab$payer_code)

c =0; i=1
while(i <= 101766)
{
  if(diab$payer_code[i] == '?'){
    c = c + 1
    if(c <= 6710){
      diab$payer_code[i] = 'BC'
    }else if(c > 6710 & c <= 18418){
      diab$payer_code[i] = 'HM'
    }else if(c > 18418 & c <= 40418){
      diab$payer_code[i] = 'CP'
    }else if(c > 40418 & c <= 75000){
      diab$payer_code[i] = 'SP'
    }else if(c > 75000 & c <= 101766){
      diab$payer_code[i] = 'MC'
    }
  }
  i = i+1
}

View(diab)
print(c)

#Replace medical_speciality with different levels

c =0; i=1
while(i <= 101766)
{
  if(diab$medical_specialty[i] == '?'){
    c = c + 1
    if(c <= 6710){
      diab$medical_specialty[i] = 'Cardiology'
    }else if(c > 6710 & c <= 18418){
      diab$medical_specialty[i] = 'Family/GeneralPractice'
    }else if(c > 18418 & c <= 40418){
      diab$medical_specialty[i] = 'Emergency/Trauma'
    }else if(c > 40418 & c <= 75000){
      diab$medical_specialty[i] = 'InternalMedicine'
    }else if(c > 75000 & c <= 101766){
      diab$medical_specialty[i] = 'Surgery-General'
    }
  }
  i = i+1
}

print(c)
View(diab)
table(diab$medical_specialty)

#Replace race with different levels

c =0; i=1
while(i <= 101766)
{
  if(diab$race[i] == '?'){
    c = c + 1
    if(c <= 50010){
      diab$race[i] = 'AfricanAmerican'
    }else if(c > 50010 & c <= 101766){
      diab$race[i] = 'Caucasian'
    }
  }
  i = i+1
}

print(c)
View(diab)
table(diab$race)

View(diab)
#Replace diag_1 with different levels

diab$diag_1 = as.character(diab$diag_1)

#for decimals
i=1;c=0
for(i in 1:length(diab$diag_1))
{
  if(nchar(diab$diag_1[i]) >= 5){
    sub=diab$diag_1[i]
    sub1 = substr(sub,1,3)
    diab$diag_1[i] = sub1
  }
}  

print(c)
table(diab$diag_1)

#for "V"
i=1;c=0
for(i in 1:length(diab$diag_1))
{
  sub=diab$diag_1[i]
  rep = "V"
  #str = gsub("V","",sub)
    if(grepl(rep,sub)){
      diab$diag_1[i] = gsub(rep,"",sub)
    }
}  

print(c)

#for "E"
i=1;c=0
for(i in 1:length(diab$diag_1))
{
  sub=diab$diag_1[i]
  rep = "E"
  #str = gsub("E","",sub)
  if(grepl(rep,sub)){
    diab$diag_1[i] = gsub(rep,"",sub)
  }
} 


str(diab)


#Replace "?" in  diag_1 with different levels
diab$diag_1 = as.factor(diab$diag_1)

c =0; i=1
while(i <= 101766)
{
  if(diab$diag_1[i] == '?'){
    diab$diag_1[i] = 272
  }
  i = i+1
}

table(diab$diag_1)

#Replace diag_2 with different levels

table(diab$diag_2)

diab$diag_2 = as.character(diab$diag_2)
#for decimals
i=1
for(i in 1:length(diab$diag_2))
{
  if(nchar(diab$diag_2[i]) >= 5){
    sub=diab$diag_2[i]
    sub1 = substr(sub,1,3)
    diab$diag_2[i] = sub1
  }
}  

print(c)
table(diab$diag_2)

#for "V"
i=1
for(i in 1:length(diab$diag_2))
{
  sub=diab$diag_2[i]
  rep = "V"
  #str = gsub("V","",sub)
  if(grepl(rep,sub)){
    diab$diag_2[i] = gsub(rep,"",sub)
  }
}  

print(c)

#for "E"
i=1;c=0
for(i in 1:length(diab$diag_2))
{
  sub=diab$diag_2[i]
  rep = "E"
  #str = gsub("E","",sub)
  if(grepl(rep,sub)){
    diab$diag_2[i] = gsub(rep,"",sub)
  }
} 


str(diab)


#Replace "?" in  diag_2 with different levels
diab$diag_2 = as.factor(diab$diag_2)

i=1
while(i <= 101766)
{
  if(diab$diag_2[i] == '?'){
    diab$diag_2[i] = 162
  }
  i = i+1
}

table(diab$diag_2)

#Replace diag_3 with different levels

table(diab$diag_3)

diab$diag_3 = as.character(diab$diag_3)
#for decimals
i=1
for(i in 1:length(diab$diag_3))
{
  if(nchar(diab$diag_3[i]) >= 5){
    sub=diab$diag_3[i]
    sub1 = substr(sub,1,3)
    diab$diag_3[i] = sub1
  }
}  

print(c)
table(diab$diag_3)

#for "V"
i=1
for(i in 1:length(diab$diag_3))
{
  sub=diab$diag_3[i]
  rep = "V"
  #str = gsub("V","",sub)
  if(grepl(rep,sub)){
    diab$diag_3[i] = gsub(rep,"",sub)
  }
}  

print(c)

#for "E"
i=1
for(i in 1:length(diab$diag_3))
{
  sub=diab$diag_3[i]
  rep = "E"
  #str = gsub("E","",sub)
  if(grepl(rep,sub)){
    diab$diag_3[i] = gsub(rep,"",sub)
  }
} 


str(diab)


#Replace "?" in  diag_3 with different levels
diab$diag_3 = as.factor(diab$diag_3)

i=1;c=0
while(i <= 101766)
{
  if(diab$diag_3[i] == '?'){
    c = c + 1
    if(c <= 50010){
      diab$diag_3[i] = 205
    }else if(c > 50010 & c <= 101766){
      diab$diag_3[i] = 238
    }
  }
  i = i+1
}

table(diab$diag_3)

#Drop weight column--> more than 50% '?'
diab$weight = NULL
diab$encounter_id = NULL
diab$patient_nbr = NULL
diab$admission_type_id = NULL
diab$admission_source_id = NULL
diab$discharge_disposition_id = NULL

#convert to numeric -- > more than 50 levels
str(diab)
diab$medical_specialty = as.numeric(diab$medical_specialty)
diab$diag_1 = as.numeric(diab$diag_1)
diab$diag_2 = as.numeric(diab$diag_2)
diab$diag_3 = as.numeric(diab$diag_3)

#Model Building --- > RANDOM FOREST
#---------------------------------------
  
library(caret) # for confusion matrix  
library(randomForest)

# randomly shuffle the dataset as there is a bias
grp = runif(nrow(diab))
diab = diab[order(grp),]

#Model - 1 -----> Accuracy (58.66%)

# split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind = sample(seq_len(nrow(diab)), floor(nrow(diab)*0.7))
train = diab[ind,]
test = diab[-ind,]

nrow(train)
nrow(test)
ncol(diab)
str(diab)  

# to check the count of each value of a factor variable against the Y-variable
# ----------------------------------------------------------------------------
100*prop.table(table(train$readmitted))

100*prop.table(table(test$readmitted))

100*prop.table(table(diab$readmitted)) #actual % of <30, >30 and NO

colnames(train);colnames(test)

#  call the randomforest() for Classification
# ---------------------------------------------------------------------
train_x = train[,1:43]
train_y = train[,44]
rf1 = randomForest(train_x, factor(train_y)) #Build model 
rf1

importance(rf1)
summary(rf1)
# ntree=500 is the default. More the better.

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf1 = predict(rf1, test)
pdct_rf1

table(predicted = pdct_rf1 , actual = test$readmitted)
confusionMatrix(pdct_rf1 ,test$readmitted, positive = "NO")

#MOdel-2 -------> Accuracy(55%)

diab_M2 = data.frame(diab$race,diab$gender,diab$number_outpatient,diab$number_emergency,
                     diab$A1Cresult,diab$metformin,diab$nateglinide,diab$glimepiride.pioglitazone,
                     diab$glyburide.metformin,diab$readmitted)

View(diab_M2)
# split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind2 = sample(seq_len(nrow(diab_M2)), floor(nrow(diab_M2)*0.7))
train2 = diab_M2[ind2,]
test2 = diab_M2[-ind2,]

nrow(train2)
nrow(test2)
ncol(diab_M2)
str(diab_M2) 

length(test2$diab.readmitted)
100*prop.table(table(test2$diab.readmitted))

#  call the randomforest() for Classification
# ---------------------------------------------------------------------
train_x2 = train2[,1:9]
train_y2 = train2[,10]
rf2 = randomForest(train_x2, factor(train_y2)) #Build model 
rf2

importance(rf2)
summary(rf2)

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf2 = predict(rf2, test2)
pdct_rf2

table(predicted = pdct_rf2 , actual = test2$diab.readmitted)
confusionMatrix(pdct_rf2 ,test2$diab.readmitted, positive = "NO")

#MOdel-3 ------> Accuracy(55.13%)

diab_M3 = data.frame(diab$race,diab$gender,diab$number_outpatient,diab$number_emergency,
                     diab$A1Cresult,diab$metformin,diab$glipizide,diab$glyburide,
                     diab$tolazamide,diab$readmitted)

View(diab_M3)
# split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind3 = sample(seq_len(nrow(diab_M3)), floor(nrow(diab_M3)*0.7))
train3 = diab_M3[ind3,]
test3 = diab_M3[-ind3,]

nrow(train3)
nrow(test3)
ncol(diab_M3)
str(diab_M3) 

length(test3$diab.readmitted)
100*prop.table(table(test3$diab.readmitted))

#  call the randomforest() for Classification
# ---------------------------------------------------------------------
train_x3 = train3[,1:9]
train_y3 = train3[,10]
rf3 = randomForest(train_x3, factor(train_y3)) #Build model 
rf3

importance(rf3)
summary(rf3)

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf3 = predict(rf3, test3)
pdct_rf3

table(predicted = pdct_rf3 , actual = test3$diab.readmitted)
confusionMatrix(pdct_rf3 ,test3$diab.readmitted, positive = "NO")

#Model-4 -----> Accuracy(55.51%)

diab_M4 = data.frame(diab$race,diab$gender,diab$number_outpatient,diab$number_emergency,
                     diab$A1Cresult,diab$metformin,diab$glipizide,diab$glyburide,
                     diab$readmitted)

# split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind4 = sample(seq_len(nrow(diab_M4)), floor(nrow(diab_M4)*0.7))
train4 = diab_M4[ind4,]
test4 = diab_M4[-ind4,]

nrow(train4)
nrow(test4)
ncol(diab_M4)
str(diab_M4) 

length(test4$diab.readmitted)
100*prop.table(table(test4$diab.readmitted))

#  call the randomforest() for Classification
# ---------------------------------------------------------------------
train_x4 = train4[,1:8]
train_y4 = train4[,9]
rf4 = randomForest(train_x4, factor(train_y4)) #Build model 
rf4

importance(rf4)
summary(rf4)

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf4 = predict(rf4, test4)
pdct_rf4

table(predicted = pdct_rf4, actual = test4$diab.readmitted)
confusionMatrix(pdct_rf4 ,test4$diab.readmitted, positive = "NO")


#Model-5 -------> Accuracy(55.11%)

diab_M5 = data.frame(diab$race,diab$gender,diab$number_outpatient,diab$number_emergency,
                     diab$A1Cresult,diab$metformin,diab$glipizide,diab$glyburide,
                     diab$nateglinide,diab$glimepiride.pioglitazone,
                     diab$glyburide.metformin,diab$readmitted)

# split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind5 = sample(seq_len(nrow(diab_M5)), floor(nrow(diab_M5)*0.7))
train5 = diab_M5[ind5,]
test5 = diab_M5[-ind5,]

nrow(train5)
nrow(test5)
ncol(diab_M5)
str(diab_M5) 

length(test5$diab.readmitted)
100*prop.table(table(test5$diab.readmitted))

#  call the randomforest() for Classification
# ---------------------------------------------------------------------
train_x5 = train5[,1:11]
train_y5 = train5[,12]
rf5 = randomForest(train_x5, factor(train_y5)) #Build model 
rf5

importance(rf5)
summary(rf5)

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf5 = predict(rf5, test5)
pdct_rf5

table(predicted = pdct_rf5, actual = test5$diab.readmitted)
confusionMatrix(pdct_rf5 ,test5$diab.readmitted, positive = "NO")


#MOdel-6 ---- > Accurcay(55.27%)

diab_M6 = data.frame(diab$race,diab$gender,diab$number_outpatient,diab$number_emergency,
                     diab$A1Cresult,diab$metformin,diab$nateglinide,diab$glimepiride.pioglitazone,
                     diab$glyburide.metformin,diab$readmitted)

View(diab_M6)
# split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind6 = sample(seq_len(nrow(diab_M6)), floor(nrow(diab_M6)*0.7))
train6 = diab_M6[ind6,]
test6 = diab_M6[-ind6]

nrow(train2)
nrow(test2)
ncol(diab_M2)
str(diab_M2) 

length(test2$diab.readmitted)
100*prop.table(table(test2$diab.readmitted))

#  call the randomforest() for Classification
# ---------------------------------------------------------------------
train_x6 = train6[,1:9]
train_y6 = train6[,10]
rf6 = randomForest(train_x6, factor(train_y6)) #Build model 
rf6

importance(rf6)
summary(rf6)

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf6 = predict(rf6, test6)
pdct_rf6

table(predicted = pdct_rf6 , actual = test6$diab.readmitted)
confusionMatrix(pdct_rf6 ,test6$diab.readmitted, positive = "NO")

# always use rpart.rplot with these parameters to get a good visual output
windows()
rpart.plot(diab, type=4, extra=101,
           box.palette = "GnBu",
           branch.lty = 3,
           shadow.col = "gray",
           nn=T)
