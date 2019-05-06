rm(list = ls())
options(scipen = 100,digits = 4)
library(data.table)
library(tidyverse)
library(DataExplorer)
library(lubridate)
library(timetk)
library(timeDate)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(caret)
library(randomForest)
library(dummies)
library(xgboost)
library(catboost)

fread("train.csv",na.strings = c(NA,''))%>%data.frame()->mahn_tr
fread("test.csv",na.strings = c(NA,''))%>%data.frame()->mahn_te


mahn_tr%>%select(reservation_id,amount_spent_per_room_night_scaled)->target
as.factor(target$reservation_id)->target$reservation_id
mahn_tr$amount_spent_per_room_night_scaled<-NULL

mahn_tr$type<-"train"
mahn_te$type<-"test"

rbind(mahn_tr,mahn_te)->mahn

#Converting character variables to factor.
data.frame(lapply(mahn, function(v) {
  if (is.character(v)) return(as.factor(v))
  else return(v)
}))->mahn
#str(mahn)

#Converting integer variables to factor.
data.frame(lapply(mahn, function(v) {
  if (is.integer(v)) return(as.factor(v))
  else return(v)
}))->mahn
#str(mahn)

mahn->mahn_bkp
mahn_bkp->mahn
#---------------------------------------------------------------------------------------

#Extracting time based features.

mahn$booking_date<-as.Date(mahn$booking_date,format="%d/%m/%y")
mahn$checkin_date<-as.Date(mahn$checkin_date,format="%d/%m/%y")
mahn$checkout_date<-as.Date(mahn$checkout_date,format="%d/%m/%y")

as.numeric(mahn$checkin_date-mahn$booking_date)->mahn$booking_checkin
as.numeric(mahn$checkout_date-mahn$booking_date)->mahn$booking_checkout
as.numeric(mahn$checkout_date-mahn$checkin_date)->mahn$checkout_checkin


#Changed the year from 2018 to 2012
mahn%>%filter(booking_checkin<0)%>%select(reservation_id)->id
#mahn[which(mahn[,"reservation_id"]%in%id$reservation_id),"booking_date"]
year(mahn[which(mahn[,"reservation_id"]%in%id$reservation_id),"checkin_date"])<-2018
year(mahn[which(mahn[,"reservation_id"]%in%id$reservation_id),"checkout_date"])<-2018


mahn%>%filter(booking_checkin<0)%>%select(reservation_id)->id
#mahn[which(mahn[,"reservation_id"]%in%id$reservation_id),"booking_date"]
month(mahn[which(mahn[,"reservation_id"]%in%id$reservation_id),"booking_date"])<-03

mahn$booking_date<-as.Date(mahn$booking_date,format="%d/%m/%y")
mahn$checkin_date<-as.Date(mahn$checkin_date,format="%d/%m/%y")
mahn$checkout_date<-as.Date(mahn$checkout_date,format="%d/%m/%y")

as.numeric(mahn$checkin_date-mahn$booking_date)->mahn$booking_checkin
as.numeric(mahn$checkout_date-mahn$booking_date)->mahn$booking_checkout
as.numeric(mahn$checkout_date-mahn$checkin_date)->mahn$checkout_checkin


mahn%>%filter(booking_checkin<0)%>%select(reservation_id)->id
#mahn[which(mahn[,"reservation_id"]%in%id$reservation_id),"booking_date"]
day(mahn[which(mahn[,"reservation_id"]%in%id$reservation_id),"booking_date"])<-01

mahn$booking_date<-as.Date(mahn$booking_date,format="%d/%m/%y")
mahn$checkin_date<-as.Date(mahn$checkin_date,format="%d/%m/%y")
mahn$checkout_date<-as.Date(mahn$checkout_date,format="%d/%m/%y")

as.numeric(mahn$checkin_date-mahn$booking_date)->mahn$booking_checkin
as.numeric(mahn$checkout_date-mahn$booking_date)->mahn$booking_checkout
as.numeric(mahn$checkout_date-mahn$checkin_date)->mahn$checkout_checkin

#Checking for the same day checkin and booking

ifelse(mahn$booking_checkin==0,"yes","no")->mahn$sameday_checkin
as.factor(mahn$sameday_checkin)->mahn$sameday_checkin
#summary(mahn)

#----------------------------------------------------------------------------------------------

#Feature Engineering on the data.
#length(which(mahn[,"total_pax"]!=(as.numeric(mahn$numberofadults)+as.numeric(mahn$numberofchildren))))

#Taking total number of people travelling.
#Number of adults can't be 0, imputing with 17.

ifelse(mahn$numberofadults=="0","17",as.character(mahn$numberofadults))->mahn$numberofadults
as.factor(mahn$numberofadults)->mahn$numberofadults

as.numeric(as.character(mahn$numberofadults))->mahn$numberofadults
as.numeric(as.character(mahn$numberofchildren))->mahn$numberofchildren
as.numeric(as.character(mahn$total_pax))->mahn$total_pax

mahn$numberofadults+mahn$numberofchildren->mahn$total_travelling


#Ratio of number people and roombooked.
ifelse(mahn$roomnights=="-45","45",as.character(mahn$roomnights))->mahn$roomnights
ifelse(mahn$roomnights=="0","42",as.character(mahn$roomnights))->mahn$roomnights
as.numeric(as.character(mahn$roomnights))->mahn$roomnights

as.numeric(as.character(mahn$total_travelling))/as.numeric(as.character(mahn$roomnights))->mahn$room_ppl_ratio
ifelse(mahn$room_ppl_ratio<1,"yes","no")->mahn$extra_room_booked
as.factor(mahn$extra_room_booked)->mahn$extra_room_booked

as.numeric(as.character(mahn$roomnights))/as.numeric(as.character(mahn$total_travelling))->mahn$ppl_room_ratio

(mahn$checkout_checkin/mahn$total_travelling)->mahn$days_per_person
(mahn$checkout_checkin/mahn$roomnights)->mahn$days_per_room
#Lets handle the 2 id fields, via Memberid and via resort id

fn <- funs(mean, min, var,max, sum, n_distinct, .args = list(na.rm = TRUE))

mahn%>%select(memberid,total_travelling,total_pax,roomnights,room_ppl_ratio,days_per_person,days_per_room,
              ppl_room_ratio,booking_checkin,booking_checkout,checkout_checkin)->member_df
sum_member_df<-member_df%>%
  group_by(memberid)%>%summarise_all(fn)%>%left_join(
    mahn%>%group_by(memberid)%>%summarise(memberid_cnt=n()),by="memberid")

colnames(sum_member_df)<-sprintf('memberid_%s',colnames(sum_member_df))
colnames(sum_member_df)[1]<-"memberid"
colnames(sum_member_df)[ncol(sum_member_df)]<-"memberid_cnt"

nm <- names(sum_member_df)[colSums(is.na(sum_member_df)) != 0]
# sum_member_df[,(nm) :=lapply(nm,function(x){
#   x<-get(x)
#   x[is.na(x)]<-mean(x,na.rm = TRUE)
#   x
#   }),by=memberid]

sum_member_df[,nm]->sum_member_df_var
for(i in 1:ncol(sum_member_df_var)){
  sum_member_df_var[is.na(sum_member_df_var[,i]), i] <- min(sum_member_df_var[,i], na.rm = TRUE)
}
nm <- names(sum_member_df)[colSums(is.na(sum_member_df)) == 0]
sum_member_df[,nm]->sum_member_df
cbind(sum_member_df,sum_member_df_var)->sum_member_df

mahn%>%select(resort_id,total_travelling,total_pax,roomnights,room_ppl_ratio,days_per_person,
              ppl_room_ratio,booking_checkin,booking_checkout,checkout_checkin)->resort_df
sum_resort_df<-resort_df%>%group_by(resort_id)%>%summarise_all(fn)%>%left_join(
  mahn%>%group_by(resort_id)%>%summarise(resort_id_cnt=n()),by="resort_id")

colnames(sum_resort_df)<-sprintf('resort_id_%s',colnames(sum_resort_df))
colnames(sum_resort_df)[1]<-"resort_id"
colnames(sum_resort_df)[ncol(sum_resort_df)]<-"resort_id_cnt"


#Taking the count how the member booked the each resort.
paste(mahn$memberid,mahn$resort_id,"_")->mahn$member_to_resort
as.factor(mahn$member_to_resort)->mahn$member_to_resort

mahn%>%group_by(member_to_resort)%>%
  summarise(member_to_resort_cnt=n())->member_resort_df

mahn%>%
  left_join(sum_member_df,by="memberid")%>%
  left_join(sum_resort_df,by="resort_id")%>%
  left_join(member_resort_df,"member_to_resort")->mahn

#--------------------------------------------------------------------------------------------------------

#Persontravellingid

ifelse(mahn$persontravellingid%in%c("46","47"),"46-47",as.character(mahn$persontravellingid))->mahn$persontravellingid
ifelse(mahn$persontravellingid%in%c("4752","4753","4995"),"48",as.character(mahn$persontravellingid))->mahn$persontravellingid
as.factor(mahn$persontravellingid)->mahn$persontravellingid

#Room Type Booked Code
ifelse(mahn$room_type_booked_code%in%c("1","5","6"),"1",as.character(mahn$room_type_booked_code))->mahn$room_type_booked_code
as.factor(mahn$room_type_booked_code)->mahn$room_type_booked_code

#State Code Resort
ifelse(mahn$state_code_resort%in%c("3","5"),"3-5",as.character(mahn$state_code_resort))->mahn$state_code_resort
ifelse(mahn$state_code_resort%in%c("10","13"),"10-13",as.character(mahn$state_code_resort))->mahn$state_code_resort
ifelse(mahn$state_code_resort%in%c("4","11"),"4-11",as.character(mahn$state_code_resort))->mahn$state_code_resort
as.factor(mahn$state_code_resort)->mahn$state_code_resort

#Member Age Buckets
ifelse(mahn$member_age_buckets%in%c("A","I","J"),"A",as.character(mahn$member_age_buckets))->mahn$member_age_buckets
ifelse(mahn$member_age_buckets%in%c("G","H"),"G",as.character(mahn$member_age_buckets))->mahn$member_age_buckets
as.factor(mahn$member_age_buckets)->mahn$member_age_buckets

#Cluster Code
ifelse(mahn$cluster_code%in%c("B","C"),"B-C",as.character(mahn$cluster_code))->mahn$cluster_code
as.factor(mahn$cluster_code)->mahn$cluster_code

#reservationstatusid code

ifelse(mahn$reservationstatusid_code%in%c("C","D"),"C",as.character(mahn$reservationstatusid_code))->mahn$reservationstatusid_code
as.factor(mahn$reservationstatusid_code)->mahn$reservationstatusid_code

#Season Holidayed Code

ggplot(mahn,aes(season_holidayed_code))+
  geom_bar(stat = "count")

ifelse(is.na(mahn$season_holidayed_code)==TRUE,"1",as.character(mahn$season_holidayed_code))->mahn$season_holidayed_code
as.factor(mahn$season_holidayed_code)->mahn$season_holidayed_code

#via State Code Residence

ggplot(mahn,aes(state_code_residence))+
  geom_bar(stat = "count")
ifelse(is.na(mahn$state_code_residence)==TRUE,"3",as.character(mahn$state_code_residence))->mahn$state_code_residence


case_when(
  mahn$state_code_residence%in%c("3","6","10","12","15") ~ "3",
  mahn$state_code_residence%in%c("11","13","14","16","18","19","20","21","22",
                                 "23","24","25","26","27","28","29",
                                 "30","31","32","33","34","35","36","37","38") ~ "6",
  TRUE ~ as.character(mahn$state_code_residence)
)->mahn$state_code_residence_grp
as.factor(mahn$state_code_residence_grp)->mahn$state_code_residence_grp

ggplot(mahn,aes(state_code_residence_grp))+
  geom_bar(stat = "count")

mahn%>%select(-c(memberid,resort_id,member_to_resort,state_code_residence))->mahn

#Removing zero varaince varaible.
zero_varaince<-function(data){
  out<-lapply(data, function(x) length(unique(x)))
  want<-which(!out>1)
  unlist(want)
}

mahn[,-zero_varaince(mahn)]->mahn

rm(mahn_te,mahn_tr,sum_member_df,sum_resort_df,resort_df,member_df,sum_member_df_var);gc()


#-----------------------------------------------------------------------------------------------------------

mahn%>%filter(type=="train")->mahn_tr

mahn%>%filter(type=="test")->mahn_te

#----------------------------------------------------------------------------------------------------------
#Adding timeseries data to the train and test series seperatly.

mahn_tr%>%select(booking_date)%>%distinct()->booking_date

booking_date%>%
  tk_augment_timeseries_signature()%>%
  select(booking_date,diff,month.lbl,day,week,qday,yday,mweek)%>%
  left_join(mahn_tr%>%group_by(booking_date)%>%summarise(cnt=n()),by="booking_date")->booking_date
colnames(booking_date)<-sprintf('booking_date_%s',colnames(booking_date))
colnames(booking_date)[1]<-"booking_date"


booking_date[which(is.na(booking_date$booking_date_diff)==T),"booking_date_diff"]<-0

mahn_tr%>%select(checkin_date)%>%distinct()->checkin_date

checkin_date%>%
  tk_augment_timeseries_signature()%>%
  select(checkin_date,month.lbl,wday.lbl,day,week,qday,yday,mweek)%>%
  left_join(mahn_tr%>%group_by(checkin_date)%>%summarise(cnt=n()),by="checkin_date")->checkin_date
colnames(checkin_date)<-sprintf('checkin_date_%s',colnames(checkin_date))
colnames(checkin_date)[1]<-"checkin_date"


mahn_tr%>%select(checkout_date)%>%distinct()->checkout_date


checkout_date%>%
  tk_augment_timeseries_signature()%>%
  select(checkout_date,wday.lbl)%>%
  left_join(mahn_tr%>%group_by(checkout_date)%>%summarise(cnt=n()),by="checkout_date")->checkout_date
colnames(checkout_date)<-sprintf('checkout_date_%s',colnames(checkout_date))
colnames(checkout_date)[1]<-"checkout_date"


mahn_tr%>%left_join(booking_date,"booking_date")%>%
  left_join(checkin_date,by="checkin_date")%>%
  left_join(checkout_date,by="checkout_date")->mahn_tr

mahn_tr%>%select(-c(booking_date,checkin_date,checkout_date,type))->mahn_tr

as.factor(mahn_tr$reservation_id)->mahn_tr$reservation_id
mahn_tr%>%left_join(target,by="reservation_id")->mahn_tr


#----------------------------------------------------------------------------------------------------
#Adding the same to the test 

mahn_te%>%select(booking_date)%>%distinct()->booking_date

booking_date%>%
  tk_augment_timeseries_signature()%>%
  select(booking_date,diff,month.lbl,day,week,qday,yday,mweek)%>%
  left_join(mahn_te%>%group_by(booking_date)%>%summarise(cnt=n()),by="booking_date")->booking_date
colnames(booking_date)<-sprintf('booking_date_%s',colnames(booking_date))
colnames(booking_date)[1]<-"booking_date"

booking_date[which(is.na(booking_date$booking_date_diff)==T),"booking_date_diff"]<-0

mahn_te%>%select(checkin_date)%>%distinct()->checkin_date

checkin_date%>%
  tk_augment_timeseries_signature()%>%
  select(checkin_date,month.lbl,wday.lbl,day,week,qday,yday,mweek)%>%
  left_join(mahn_te%>%group_by(checkin_date)%>%summarise(cnt=n()),by="checkin_date")->checkin_date
colnames(checkin_date)<-sprintf('checkin_date_%s',colnames(checkin_date))
colnames(checkin_date)[1]<-"checkin_date"

mahn_te%>%select(checkout_date)%>%distinct()->checkout_date

checkout_date%>%
  tk_augment_timeseries_signature()%>%
  select(checkout_date,wday.lbl)%>%
  left_join(mahn_te%>%group_by(checkout_date)%>%summarise(cnt=n()),by="checkout_date")->checkout_date
colnames(checkout_date)<-sprintf('checkout_date_%s',colnames(checkout_date))
colnames(checkout_date)[1]<-"checkout_date"

mahn_te%>%left_join(booking_date,"booking_date")%>%
  left_join(checkin_date,by="checkin_date")%>%
  left_join(checkout_date,by="checkout_date")->mahn_te

mahn_te%>%select(-c(booking_date,checkin_date,checkout_date,type))->mahn_te
as.character(mahn_te$reservation_id)->mahn_te$reservation_id

rm(booking_date,checkin_date,checkout_date,mahn,id);gc()

#------------------------------------------------------------------------------------------------------

train_index<-base::sample(1:nrow(mahn_tr),nrow(mahn_tr)*0.75)



y_train<-mahn_tr[train_index,-c(1,ncol(mahn_tr))]
y_label<-unlist(mahn_tr$amount_spent_per_room_night_scaled[train_index])

x_valid<-mahn_tr[-train_index,-(c(1,ncol(mahn_tr)))]
x_label<-unlist(mahn_tr$amount_spent_per_room_night_scaled[-train_index])

which(sapply(y_train, is.factor))->cat_train_features
y_train_pool<-catboost.load_pool(data=y_train,label=y_label,cat_features =cat_train_features)

which(sapply(x_valid, is.factor))->cat_test_features
x_valid_pool<-catboost.load_pool(data = x_valid,label = x_label,cat_features = cat_test_features)

z_test<-catboost.load_pool(data = mahn_te[,-1])



params <- list(iterations=500,
               learning_rate=0.05,
               depth=9,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 50,
               od_type='Iter',
               metric_period = 50,
               od_wait=100,
               use_best_model=TRUE)

model <- catboost.train(learn_pool=y_train_pool,test_pool = x_valid_pool,params =  params)

catboost.predict(model,z_test)->prediction

mahn_te$amount_spent_per_room_night_scaled<-prediction
mahn_te%>%select(reservation_id,amount_spent_per_room_night_scaled)%>%
  write.csv(.,"catboost_last_submission.csv",row.names = F)






read.csv("xgboost_days_per_person_submission_99.29.csv")->ensemble
ensemble%>%left_join(mahn_te[,c(1,146)],by="reservation_id")->ensemble

(ensemble$amount_spent_per_room_night_scaled.x+ensemble$amount_spent_per_room_night_scaled.y)/2->ensemble$amount_spent_per_room_night_scaled

ensemble%>%select(reservation_id,amount_spent_per_room_night_scaled)%>%
  write.csv(.,"xgboost_and_catboost_ensemble.csv",row.names = F)
  
