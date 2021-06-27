setwd("C:/Users/hc07c/Documents/서울시립대/5학년 1학기/캡스톤 디자인/캡스톤 논문/tour/")
require(foreign)
require(dplyr)

t2019 = read.csv("tour2019.csv",encoding = "UTF-8")
t2018 = read.spss(file = 'Data_2018년 외래관광객 실태조사_원자료.sav',to.data.frame = T,use.value.labels=F)


t2018 = t2018 %>% filter(Q5_1==1) %>% select(D_AGE,Q2A1,D_Q13,D_MON,Q15_2A1:Q15_2A12,MQ14_2_1,MQ14_2_3:MQ14_2_8)
t2019 = t2019 %>% filter(Q5_1==1) %>% select(D_AGE,Q2A1,D_Q12,D_MON,Q15_2A1:Q15_2A12,MQ14_2_1,MQ14_2_3:MQ14_2_8)
name = c("AGE","Companion",'TourType','Mon','Sat_pass','Sat_trans','Sat_RoadResearch',
        'Sat_Accomo','Sat_food','Sat_shop','Sat_info','Sat_Sightseeing','Sat_lang',
        'Sat_Pay','Sat_Safe','Sat_internet','Pay_Accomo','Pay_shop','Pay_Food','Pay_Trans','Pay_KorCoun','Pay_Entertain','Pay_internet')
length(name)
colnames(t2018) = name
colnames(t2019) = name
write.csv(x = tdata2,file='tdata2.csv')
tdata = rbind(t2018,t2019)
tdata2 = tdata[complete.cases(tdata),]
