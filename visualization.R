getwd()
tournum = read.csv(file ='관광목적 여행객.csv',encoding='UTF-8')
tournum = tournum[,-1]
tournum =as.data.frame(t(tournum))
tournum[,'year'] = c(2011:2021)
tournum$year = as.character(tournum$year)
colnames(tournum)[1] = 'NumberOfTourist'


require(ggplot2)
p = ggplot(data=tournum,mapping = aes(x=year,y=NumberOfTourist))
p+geom_bar(stat='identity',fill='blue')+stat_smooth(col='red')+
  geom_text(aes(label=NumberOfTourist),vjust=-1,col='red')
  
str(tournum)
rownames(tournum)


require(readxl)
tourbalance = read_xls('관광수지.xls')
tourrevenue = read_xls('관광수입.xls')
tourexp = read_xls('관광지출.xls')
p = ggplot(data=tourbalance,aes(x=))
str(tourbalance)
year=c()
for( i in 1:nrow(tourbalance)){
  year[i] = substr(tourbalance$연도[i],1,5)
}
tourbalance$연도 = year
tourrevenue$연도 = year
tourexp$연도 = year
colnames(tourbalance)[1:2] = c('year','total')
colnames(tourrevenue)[1:2] = c('year','totalrevenue')
colnames(tourexp)[1:2] = c('year','totalexp')
p = ggplot(data = tourbalance[,1:2],mapping = aes(x= year,y= total))
p+geom_bar(stat='identity',fill='blue')+geom_text(aes(label=total),vjust=1,col='red')+
  ylab('관광수지(1,000USD)')+xlab('연도')

p = ggplot(data = tourrevenue[,1:2],mapping = aes(x= year,y= total))
p+geom_bar(stat='identity',fill='blue')+geom_text(aes(label=total),vjust=-1,col='red')+
  ylab('관광수입(1,000USD)')+xlab('연도')

p = ggplot(data = tourexp[,1:2],mapping = aes(x= year,y= total))
p+geom_bar(stat='identity',fill='blue')+geom_text(aes(label=total),vjust=-1,col='red')+
  ylab('관광지출출(1,000USD)')+xlab('연도')



write.csv(cbind(tourrevenue[,1:2],tourexp[,2]),file = 'sdfsdf.csv')
data = new_tour %>% select(totalPay,country)
data$totalPay = exp(data$totalPay)
write.csv(data,file = 'data.csv')
summary(new_tour)
str(new_tour)
