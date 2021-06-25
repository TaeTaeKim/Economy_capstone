setwd("C:/Users/hc07c/Documents/서울시립대/5학년 1학기/캡스톤 디자인/캡스톤 논문/tour/")
require(foreign)
require(dplyr)
require(fastDummies)
require(car)
tour2019 = read.csv("tour2019.csv",encoding = "UTF-8")
tour2018 = read.spss(file = 'Data_2018년 외래관광객 실태조사_원자료.sav',to.data.frame = T,use.value.labels=F)

cor(tour2018$Q15_1,tour2018$Q16)
scatterplot(tour2018$Q15_1,tour2018$Q16,xlab='Satisfaction',ylab='Willing to Revisit')

#################만족도 회귀 분석.#######################

tour2018 = tour2018 %>% filter(Q5_1==1) %>% select(Q2A1,Q5_2A1,Q9_1A1:Q9_1A20,Q9_2A1,
                                                   Q10_1A1:Q10_1A17,Q10_1B1:Q10_1B8,MQ10_3A2,
                                                   Q10_5A1:Q10_5A5,Q10_5A7,Q10_5_1:Q10_5_5,
                                                   Q10_5_7,Q12_1A1:Q12_1A3,Q12_2A1:Q12_2A3,Q13,
                                                   Q14_1T,Q14_2_3,Q14_2_4,Q14_2_7,Q14_2_5,Q15_1,
                                                   D_MON,D_COU,D_GEN,D_AGE)

tour2019 = tour2019 %>% filter(Q5_1==1) %>% select(Q2A1,Q5_2A1,Q9_1A1:Q9_1A20,Q9_2A1,
                                                   Q10_1B1:Q10_1B17,Q10_1C1:Q10_1C8,MQ10_6A2,
                                                   Q10_5A1:Q10_5A5,Q10_5A7,Q10_5_1:Q10_5_5,
                                                   Q10_5_7,Q11_1A1:Q11_1A3,Q11_2A1:Q11_2A3,Q12,
                                                   Q14_1T,Q14_2_3,Q14_2_4,Q14_2_7,Q14_2_5,Q15_1,
                                                   D_MON,D_COU1,D_GEN,D_AGE)


####방문 도시 변수 --> 방문도시개수, 주요활동 --> 활동 개수 변수로 변환.####
tour2019 = apply(tour2019,2,as.numeric)
tour2019 = as.data.frame(tour2019)
actnum2018 = apply(!is.na(tour2018 %>% select(Q9_1A1:Q9_1A20)),MARGIN = 1,sum);tour2018 = cbind(tour2018,actnum2018)
actnum2019 = apply(!is.na(tour2019 %>% select(Q9_1A1:Q9_1A20)),MARGIN = 1,sum);tour2019 = cbind(tour2019,actnum2019)
visitcitynum18 = apply(!is.na(tour2018 %>% select(Q10_1A1:Q10_1A17)),MARGIN = 1,sum);tour2018 = cbind(tour2018,visitcitynum18)
visitcitynum19 = apply(!is.na(tour2019 %>% select(Q10_1B1:Q10_1B17)),MARGIN = 1,sum);tour2019 = cbind(tour2019,visitcitynum19)
tour2018 = tour2018 %>% select(!Q9_1A1:Q9_1A20)
tour2018 = tour2018 %>% select(!Q10_1A1:Q10_1A17)
tour2019 = tour2019 %>% select(!Q9_1A1:Q9_1A20)
tour2019 = tour2019 %>% select(!Q10_1B1:Q10_1B17)

####한국 방문 요인 코딩 변경####
tour2018$Q5_2A1 = car::recode(tour2018$Q5_2A1,"5:6=4;8=5;9=6;10=8;11=9;12=11;13=12;14=10;17=13;4=NA;15:16=NA")
####가장 만족한 활동 코딩 변경####
tour2018$Q9_2A1 = car::recode(tour2018$Q9_2A1,"5=8;7=8;8=NA;9=7;10=5;11=7;12=8;13=11;14=12;15=13;16=9;17=9;18=10;19=10;20=14;98=NA")
####방문 권역 더미 코딩.####
seoul = c();gyeongi = c();incheon=c();gangwon=c();chung = c();gyeongsang = c();jeonla=c();jeju=c()
for(i in 1:nrow(tour2018)){
  seoul[i] =ifelse(1 %in% (tour2018 %>% select(Q10_1B1:Q10_1B8))[i,],1,0)
  gyeongi[i] = ifelse(2 %in% (tour2018 %>% select(Q10_1B1:Q10_1B8))[i,],1,0)
  incheon[i] = ifelse(3 %in% (tour2018 %>% select(Q10_1B1:Q10_1B8))[i,],1,0)
  gangwon[i] = ifelse(4 %in% (tour2018 %>% select(Q10_1B1:Q10_1B8))[i,],1,0)
  chung[i] = ifelse(5 %in% (tour2018 %>% select(Q10_1B1:Q10_1B8))[i,],1,0)
  gyeongsang[i] = ifelse(6 %in% (tour2018 %>% select(Q10_1B1:Q10_1B8))[i,],1,0)
  jeonla[i] = ifelse(7 %in% (tour2018 %>% select(Q10_1B1:Q10_1B8))[i,],1,0)
  jeju[i] = ifelse(8 %in% (tour2018 %>% select(Q10_1B1:Q10_1B8))[i,],1,0)
}
tour2018 = cbind(tour2018,seoul,gyeongi,incheon,gangwon,chung,gyeongsang,jeonla,jeju)
tour2018 =tour2018 %>% select(!Q10_1B1:Q10_1B8)

seoul = c();gyeongi = c();incheon=c();gangwon=c();chung = c();gyeongsang = c();jeonla=c();jeju=c()
for(i in 1:nrow(tour2019)){
  seoul[i] =ifelse(1 %in% (tour2019 %>% select(Q10_1C1:Q10_1C8))[i,],1,0)
  gyeongi[i] = ifelse(2 %in% (tour2019 %>% select(Q10_1C1:Q10_1C8))[i,],1,0)
  incheon[i] = ifelse(3 %in% (tour2019 %>% select(Q10_1C1:Q10_1C8))[i,],1,0)
  gangwon[i] = ifelse(4 %in% (tour2019 %>% select(Q10_1C1:Q10_1C8))[i,],1,0)
  chung[i] = ifelse(5 %in% (tour2019 %>% select(Q10_1C1:Q10_1C8))[i,],1,0)
  gyeongsang[i] = ifelse(6 %in% (tour2019 %>% select(Q10_1C1:Q10_1C8))[i,],1,0)
  jeonla[i] = ifelse(7 %in% (tour2019 %>% select(Q10_1C1:Q10_1C8))[i,],1,0)
  jeju[i] = ifelse(8 %in% (tour2019 %>% select(Q10_1C1:Q10_1C8))[i,],1,0)
}
tour2019 = cbind(tour2019,seoul,gyeongi,incheon,gangwon,chung,gyeongsang,jeonla,jeju)
tour2019 =tour2019 %>% select(!Q10_1C1:Q10_1C8)



####이용한 숙박시설 종류 더미코딩.####
tour2018 = tour2018 %>% filter(Q10_5A1!=9)
tour2018 = tour2018 %>% filter(Q10_5A1!=8)
tour2018 = tour2018 %>% filter(Q10_5A1!=6)
k = dummy_cols(tour2018$Q10_5A1,remove_selected_columns = T)
colnames(k) = c('hotel','motel','guesthouse','resort',"Friend",'P.house')
tour2018=cbind(tour2018,k)
tour2018_1 = tour2018 %>% select(!Q10_5A1:Q10_5A5)
tour2018_1 = tour2018_1 %>% select(!Q10_5A7)
tour2018_1 = tour2018_1 %>% select(!Q10_5_1:Q10_5_5)
tour2018_1 = tour2018_1 %>% select(!Q10_5_7)
tour2018 = tour2018_1

tour2019$Q10_5A1 = car::recode(tour2019$Q10_5A1,"NA=0")
tour2019$Q10_5A2 = car::recode(tour2019$Q10_5A2,"NA=0;2=1")
tour2019$Q10_5A3 = car::recode(tour2019$Q10_5A3,"NA=0;3=1")
tour2019$Q10_5A4 = car::recode(tour2019$Q10_5A4,"NA=0;4=1")
tour2019$Q10_5A5 = car::recode(tour2019$Q10_5A5,"NA=0;5=1")
tour2019$Q10_5A7 = car::recode(tour2019$Q10_5A7,"NA=0;7=1")
tour2019_1 = tour2019 %>% select(!Q10_5_1:Q10_5_5)
tour2019_1 = tour2019_1 %>% select(!Q10_5_7)
tour2019 = tour2019_1

rm(list = c('tour2018_1','tour2019_1','k'))

####주요 쇼핑품목 /주요 쇼핑장소 코딩 ####
tour2018 = tour2018 %>% select(!c(Q12_1A2,Q12_1A3,Q12_2A2,Q12_2A3))

tour2019 = tour2019 %>% select(!c(Q11_1A2,Q11_1A3,Q11_2A2,Q11_2A3))



#######데이터 결합 및 최종 전처리(sclae정리)#######
tour2018 = tour2018[complete.cases(tour2018),]
tour2019 = tour2019[complete.cases(tour2019),]
colnames(tour2018) = c('compainon','tourfactor','bestact','tourday','shoppingItem','shoppingPlace','tourtype','totalPay','PayforShop','PayforFood','PayforPlay',
                       'PayforTrans','Satisfaction','Mon','Coun','Gen','Age','actnum','visitcities','seoul','gyeongi','incheon','gangwon','chung','gyeongsang','jeonla','jeju',
                       'hotel','motel','guesthouse','resort','Friend','p.house')
colnames(tour2019) =c('compainon','tourfactor','bestact','tourday','hotel','motel','guesthouse','resort','Friend','p.house','shoppingItem','shoppingPlace',
                             'tourtype','totalPay','PayforShop','PayforFood','PayforPlay','PayforTrans','Satisfaction','Mon','Coun','Gen','Age','actnum','visitcities','seoul',
                             'gyeongi','incheon','gangwon','chung','gyeongsang','jeonla','jeju')
factors = colnames(tour2019)[-c(4,14,15,16,17,18,19,24,25)]
for (i in factors){
  tour2019[,i] = as.factor(tour2019[,i])
}
factors =colnames(tour2018)[-c(4,8,9,10,11,12,13,18,19)]
for(i in factors){
  tour2018[,i] = as.factor(tour2018[,i])
}

tour = rbind(tour2018,tour2019)
for (i in c(8:12)){
  tour[,i] = log(tour[,i])
}
for (i in c(8:12)){
  for (j in 1:nrow(tour)){
    if(tour[j,i]<0){
      tour[j,i]=0
    }
  }
}
str(tour)
summary(tour)
tour = tour %>% filter(shoppingPlace ==1|shoppingPlace ==2|shoppingPlace ==3|shoppingPlace ==4|shoppingPlace ==5|
                         shoppingPlace ==6|shoppingPlace ==7|shoppingPlace ==9|shoppingPlace ==10|shoppingPlace ==13|
                         shoppingPlace ==14|shoppingPlace ==15)
####
str(tour)
tourpurpose = car::recode(tour$tourfactor,"1='food';3:5='entertain';c(6,7,9,10)='sightseeing';2='shopping';c(8,13)=NA;11:12='convenience'")
sat_act = car::recode(tour$bestact,"1='food';2='shopping';3:6='sightseeing';c(7,8,10)='entertain';c(9,11,14)=NA")
table(tour$shoppingPlace)
shopplace = car::recode(tour$shoppingPlace,"c(1,2)='dutyfree';3:7='roadshop';c(9,10)='traditionMarket';c(13,14,15,16)='largeMall'")
shopitem = car::recode(tour$shoppingItem,"1='beauty';2='food';3:6='fashion';7='healthfood';8:9='tabacco';10='traditionGoods';11='cultureGoods';12:13=NA")
country = car::recode(tour$Coun,"c(1,2,3,20)='E.Aisa';c(4,5,6,7,16,17,18,19)='ES.Asia';8='Austrailia';c(9,10)='N.America';11:14='Europe';15='MiddleEast';97='Else'")





hotel = (as.numeric(tour$hotel)-1)+(as.numeric(tour$motel)-1)+(as.numeric(tour$resort)-1)
hotel = car::recode(hotel,"2=1;3=1")
Personal = (as.numeric(tour$guesthouse)-1)+(as.numeric(tour$p.house)-1)
Personal = car::recode(Personal,"2=1")
hotel = as.factor(hotel);Personal = as.factor(Personal)
new_tour = cbind(tour,tourpurpose,sat_act,shopplace,shopitem,country)
new_tour = new_tour %>% select(-c('tourfactor','bestact','shoppingItem','shoppingPlace','Coun','hotel','motel','guesthouse','resort','p.house'))
new_tour = cbind(new_tour,hotel,Personal)
table(is.na(new_tour))
new_tour = new_tour[complete.cases(new_tour),]
str(new_tour)
new_tour$compainon = car::recode(new_tour$compainon,"1='alone';2='family';3='Friend/lover';4='colleague';5='ohters'")
new_tour$tourtype = car::recode(new_tour$tourtype,"1='indivisual';2='Air-tel';3='group'")




tour_sat = new_tour %>% select(-c('country','shopitem','shopplace'))
sat_model = lm(Satisfaction~.,data = tour_sat)
summary(sat_model)
###################################평균 비용 회귀분석###########################
tour_exp = new_tour
tour_exp = tour_exp %>% select(!c(PayforFood,PayforPlay,PayforTrans,PayforShop,Satisfaction))

exp_model = lm(totalPay~.,data=tour_exp)
summary(exp_model)
tour$bestact
str(tour_exp)

#################################만족도 회귀분석 #############################
tour_sat =new_tour
tour_sat = tour_sat %>% select(!c(Country,shopitem,shopplace))
stat_model = lm(Satisfaction~.,data=tour_sat)
summary(sat_model)
