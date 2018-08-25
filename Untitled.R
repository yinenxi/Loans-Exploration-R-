getwd()
pf<-read.csv('pseudo_facebook.tsv',sep='\t')
names(pf)
library(ggplot2)

qplot(x=age,data=pf,binwidth=1)+
  scale_x_continuous(breaks=seq(0,120,10))
 
install.packages('gridExtra')
library(gridExtra) 
summary(pf$friend_count)
summary(log10(pf$friend_count+1))
summary(sqrt(pf$friend_count+1))

p1<-qplot(x=friend_count,data=pf)
p2<-qplot(x=log10(friend_count+1),data=pf)
p3<-qplot(x=sqrt(friend_count),data=pf)
grid.arrange(p1,p2,p3,ncol=1)

qplot(x=www_likes,data=subset(pf,!is.na(gender)),
      geom = 'freqpoly',color=gender)+
  scale_x_continuous()+
  scale_x_log10()

by(pf$www_likes,pf$gender,sum)
data(diamonds)
diamonds
?diamonds
qplot(x=price,data=diamonds)+
  facet_wrap(~cut)
by(diamonds$price,diamonds$cut,summary)

qplot(x=price/carat,data=diamonds)+
  facet_wrap(~cut)+
  scale_x_log10()

qplot(x=color,y=price,data=diamonds,geom ='boxplot')
by(diamonds$price,diamonds$color,summary)

qplot(x=color,y=price/carat,data=diamonds,geom ='boxplot')

qplot(x=carat,data=diamonds,geom ='freqpoly')+
  scale_x_continuous(breaks=seq(0,3,0.1))+
  scale_y_continuous(breaks=seq(0,15000,1000))
  
library(ggplot2)

qplot(x=age,y=friend_count,data=pf)
ggplot(aes(x=age,y=friend_count),data=pf)+
  geom_point(alpha=1/20)+
  xlim(13,90)+
  coord_trans(y='sqrt')

names(pf)
ggplot(aes(x=age,y=friend_count),data=pf)+
  geom_point(alpha=1/20,position = position_jitter(h=0),
             color='orange')+
  coord_trans(y='sqrt')+
  geom_line(stat='summary',fun.y=mean)+
  geom_line(stat='summary',fun.y = quantile,fun.args = list(probs = .1))

ggplot(aes(x=age,y=friend_count),data=pf)+
  geom_point(alpha=1/20)

install.packages('dplyr')
library(dplyr)
age_groups<-group_by(pf,age)
pf.fc_by_age<-summarise(age_groups,
                        friend_count_mean=mean(friend_count),
                        friend_count_median=median(friend_count),
                        n=n())
pf.fc_by_age<-arrange(pf.fc_by_age,age)
head(pf.fc_by_age)
head(age_groups)

ggplot(aes(x=age,y=friend_count_mean),data=pf.fc_by_age)+
  geom_line(stat = 'summary',fun.y=mean)

cor.test(pf$age,pf$friend_count,method = 'pearson')
names(pf)

ggplot(aes(x=www_likes_received,y=likes_received),data=pf)+
  geom_point()+
  xlim(0,quantile(pf$www_likes_received,0.95))+
  ylim(0,quantile(pf$likes_received,0.95))

install.packages('alr3')
library(alr3)
data(Mitchell)
?Mitchell
names(Mitchell)
ggplot(aes(x=Month/12,y=Temp),data=Mitchell)+
  geom_point()+
  scale_x_continuous(breaks=seq(0,203,12))
cor.test(Mitchell$Temp,Mitchell$Month)
summary(Mitchell)

names(pf)
pf$age_with_months<-pf$age+(12-pf$dob_month)/12
age_months_groups<-group_by(pf,age_with_months)
pf.fc_by_age_months<-summarise(age_months_groups,
                               fc_mean=mean(friend_count),
                               fc_median=median(friend_count),
                               n=n())
pf.fc_by_age_months<-arrange(pf.fc_by_age_months,age_with_months)
head(pf.fc_by_age_months)

p1<-ggplot(aes(x=age,y=friend_count_mean),data=subset(pf.fc_by_age,age<71))+
  geom_line()+
  geom_smooth()

p2<-ggplot(aes(x=age_with_months,y=fc_mean),
       data=subset(pf.fc_by_age_months,age_with_months<71))+
  geom_line()+
  geom_smooth()
  
p3<-ggplot(aes(x=round(age/5)*5,y=friend_count),data=subset(pf,age<71))+
  geom_line(stat='summary',fun.y=mean)
grid.arrange(p1,p2,p3,ncol=1)
rm(diamonds)
data(diamonds)
diamonds
names(diamonds)
?diamonds
ggplot(aes(x=price,y=x),data=diamonds)+geom_point()
cor.test(diamonds$price,diamonds$z)

ggplot(aes(x=price,y=depth),data=diamonds)+
  geom_point(alpha=1/100)+
  scale_x_continuous(limits=c(0,18000),breaks=seq(1,18000,2))
cor.test(diamonds$price,diamonds$depth)

ggplot(aes(x=price,y=carat),data=diamonds)+
  geom_point()+
  xlim(0,quantile(diamonds$prize,0.99))+
  ylim(0,quantile(diamonds$carat,0.99))
diamonds$volume<-(diamonds$x*diamonds$y*diamonds$z)
names(diamonds)  
ggplot(aes(x=price,y=volume),data=diamonds)+
  geom_point()

with(subset(diamonds,volume>0&volume<800),cor.test(volume,price))


install.packages('dplyr')
library(dplyr)
data(diamonds)
diamonds
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity,
                                      mean_price=mean(price),
                                      median_price=median(price),
                                      min_price=min(price),
                                      max_price=max(price),
                                      n=n())
diamonds_mp_by_clarity<-arrange(diamonds_mp_by_clarity,clarity)
head(diamonds_mp_by_clarity)

p1<-ggplot(aes(x=clarity,y=mean_price),data=diamonds_mp_by_clarity)+
  geom_point()
diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color,
                                  mean_price = mean(price),
                                  n=n())
diamonds_mp_by_color<-arrange(diamonds_mp_by_color,color)
p2<-ggplot(aes(x=color,y=mean_price),data=diamonds_mp_by_color)+
  geom_point()
grid.arrange(p1,p2,ncol=2)

#探索多个变量 练习3
#按照age，gender分组
#方法一
fc_by_age_gender<-group_by(subset(pf,!is.na(gender)),age,gender)
pf.fc_by_age_gender<-summarise(fc_by_age_gender,
                               mean_friend_count=mean(friend_count),
                               median_friend_count=median(friend_count),
                               n=n())
pf.fc_by_age_gender<-arrange(pf.fc_by_age_gender,age)
head(pf.fc_by_age_gender)

#方法二
pf.fc_by_age_gender2<-pf %>%
  filter(!is.na(gender)) %>%
  group_by(age,gender) %>%
  summarise(mean_friend_count=mean(friend_count),
            median_friend_count=median(friend_count),
            n=n()) %>%
  ungroup() %>%  #summarise和group_by同时使用，summarise会删除最后一层分组:gender,所以再运行一次分组删除age层
  arrange(age) 
head(pf.fc_by_age_gender2)  

ggplot(aes(x = age, y = median_friend_count), 
      data = pf.fc_by_age_gender) +
  geom_line(aes(color = gender))

#重塑数据
install.packages("tidyr")
library(tidyr)
spread(subset(pf.fc_by_age_gender, 
                             select = c('gender', 'age', 'median_friend_count')), 
                gender, median_friend_count)

install.packages("reshape2")
library(reshape2)
pf.fc_by_age_gender.wide<-dcast(pf.fc_by_age_gender,
                                age~gender,
                                value.var='median_friend_count')
head(pf.fc_by_age_gender.wide)
ggplot(aes(x=age,y=female/male),
       data=pf.fc_by_age_gender.wide)+
  geom_line()+
  geom_hline(yintercept = 1,alpha=0.3,linetype=2)

names(pf)
#练习10 切割变量
pf$year_joined<-floor(2014 - pf$tenure/365)
pf$year_joined.bucket<-cut(pf$year_joined,
                           c(2004,2009,2011,2012,2014))
table(pf$year_joined.bucket,useNA = 'ifany')

ggplot(aes(x=age,y=friend_count),
       data=subset(pf,!is.na(year_joined.bucket)))+
  geom_line(aes(color=year_joined.bucket),stat='summary',fun.y=mean)+
  geom_line(stat='summary',fun.y=mean,linetype=2)

with(subset(pf,tenure>=1),summary(friend_count/tenure))

ggplot(aes(x=tenure,y=friendships_initiated/tenure),
       data=subset(pf,tenure>=1))+
  geom_line(aes(color=year_joined.bucket),stat='summary',fun.y=mean)

ggplot(aes(x=tenure,y=friendships_initiated/tenure),
       data=subset(pf,tenure>=1))+
  geom_smooth(aes(color=year_joined.bucket))

#酸奶案例
yo<-read.csv('yogurt.csv')
yo$id<-factor(yo$id)
str(yo)
summary(yo)
qplot(x=price,data=yo)
yo<-transform(yo,all.purchases=strawberry+blueberry+pina.colada+plain+mixed.berry)
ggplot(aes(x=time,y=price),data=yo)+
  geom_jitter(alpha=1/4)
#查看家庭样本
set.seed(4230) #设置种子，以获取重复的随机抽样结果
sample.ids<-sample(levels(yo$id),16) # 随机抽出16个不同的家庭

ggplot(aes(x=time,y=price),
       data=subset(yo,id %in% sample.ids))+
  facet_wrap(~id)+
  geom_line()+
  geom_point(aes(size=all.purchases),pch=1)

install.packages('GGally')
library(GGally)
names(pf)
set.seed(1836)
pf_subset<-pf[,c(2:15)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset),1000),])
set.seed(1836)
pf_subset[sample.int(nrow(pf_subset),1000),]
with(pf_subset,cor.test(age,mobile_likes))
cor.test(pf_subset$friendships_initiated,pf_subset$friend_count)

nci<-read.table('nci.tsv')
colnames(nci)<-c(1:64)
# qplot(x=log(price),data=diamonds)
# 多变量练习题
#1
ggplot(aes(x=log(price)),data=diamonds)+
  geom_histogram(aes(color=cut))+
  facet_wrap(~color)+
  scale_fill_brewer(type = 'qual')
#2
ggplot(aes(x=table,y=price),data=diamonds,)+
  scale_x_continuous(breaks = seq(0,90,5))+
  geom_point(aes(color=cut))+
  scale_color_brewer(type = 'qual')
#4
ggplot(aes(x=volume,y=log(price)),data=diamonds)+
  geom_point(aes(color=clarity))+
  xlim(0,quantile(diamonds$volume,0.99))+
  scale_color_brewer(type = 'div')
#5
pf$prop_initiated<-pf$friendships_initiated/pf$friend_count

#6
ggplot(aes(x=tenure,y=prop_initiated),
       data=pf)+
  geom_line(aes(color=year_joined.bucket),stat='summary',fun.y=median)+
  geom_smooth()

ggplot(aes(x=cut,y=price/carat),data=diamonds)+
  geom_jitter(aes(color=color))+
  facet_wrap(~clarity)+
  scale_color_brewer(type = 'div')

ggplot(aes(x=carat,y=price),data=diamonds)+
  geom_point()+
  geom_smooth(method='lm',color='red')
  xlim(0,quantile(diamonds$carat,0.99))+
  ylim(0,quantile(diamonds$price,0.99))

install.packages('GGally')
install.packages('scales')
install.packages('memisc')
install.packages('lattice')
install.packages('mass')
install.packages('car')

library(memisc)
library(lattice)
library(MASS)
library(car)
library(scales)
library(reshape2)
library(reshape)

set.seed(20022012)
diamond_samp<-diamonds[sample(1:length(diamonds$price),10000),]
ggpairs(diamond_samp, 
        lower = list(continuous = wrap("points", shape = I('.'))), 
        upper = list(combo = wrap("box", outlier.shape = I('.'))))
?ggpairs

plot1 <- qplot(x=price,data=diamonds,binwidth=10) + 
  ggtitle('Price')

plot2 <- qplot(x=price,data=diamonds,binwidth=0.01) +
  ggtitle('Price (log10)')+
  scale_x_log10()

grid.arrange(plot1,plot2)

cuberoot_trans=function() trans_new('cuberoot',
                                    transform=function(x) x^(1/3),
                                    inverse=function(x) x^3)
ggplot(aes(carat,price),data=diamonds)+
  geom_jitter(alpha=0.5,size=0.75)+
  scale_x_continuous(trans=cuberoot_trans(),limits = c(0.2,3),
                     breaks=c(0.2,0.5,1,2,3))+
  scale_y_continuous(trans=log10_trans(),limits = c(350,15000),
                     breaks = c(350,1000,5000,10000,15000))+
  ggtitle('Price(log10)by Cube-root of Carat')

head(sort(table(diamonds$carat),decreasing=T))

#钻石与价格预测
install.packages('RColorBrewer', dependencies = TRUE) 
library(RColorBrewer)

ggplot(aes(x = carat, y = price,color=cut), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Color', reverse = FALSE,
                                          override.aes = list(alpha = 1, size = 2))) + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Color')

m1<- lm(I(log(price))~I(carat^(1/3)),data=diamonds) 
#告诉R使用I函数内部的表达式来转换变量，然后再进行回归
m2<-update(m1, ~ . + carat) #在回归中添加carat变量
m3<-update(m2, ~ . + cut)
m4<-update(m3, ~ . + color)
m5<-update(m4, ~ . + clarity)
mtable(m1,m2,m3,m4,m5)

install.packages('RCurl')
install.packages('bitops')
library(bitops)
library(RCurl)
load("BigDiamonds.rda")
diamondsbig$logprice<-log(diamondsbig$price)
m1<-lm(logprice ~ I(carat^(1/3)),
       data=diamondsbig[diamondsbig$price<10000 & diamondsbig$cert=='GIA',])
m2<-update(m1, ~ . + carat) 
m3<-update(m2, ~ . + cut)
m4<-update(m3, ~ . + color)
m5<-update(m4, ~ . + clarity)  # m5是full model
mtable(m1,m2,m3,m4,m5)

install.packages("knitr", dependencies = T)

set.seed(2000)
loan_samp<-loan[sample(1:length(loan$MonthlyLoanPayment),10000),]
ggpairs(loan_samp, 
        lower = list(continuous = wrap("points", shape = I('.'))), 
        upper = list(combo = wrap("box", outlier.shape = I('.'))))
scale_y_sqrt()
ggplot(aes(x=MonthlyLoanPayment,y=LoanNumber),data=loan)+
  geom_point(alpha=1/20)
cor.test(loans$MonthlyLoanPayment,loans$LoanNumber)

ggpairs(wine, 
        lower = list(continuous = wrap("points", shape = I('.'))), 
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

sqrt_trans=function() trans_new('sqtr',
                                    transform=function(x) x^(1/2),
                                    inverse=function(x) x^2)



LoanStatus,EmploymentStatus,IncomeVerifiable,
EstimatedReturn,AvailableBankcardCredit,
MonthlyLoanPayment,LoanNumber,LoanOriginalAmount.StatedMonthlyIncome,BorrowerAPR

#scale_fill_brewer(direction = 1)
#scale_color_brewer(type = "div")

cor.test(loan$LoanOriginalAmount,loan$MonthlyLoanPayment)

ggplot(aes(x=LoanOriginalAmount,y=MonthlyLoanPayment,color=EmploymentStatus),data=loan)+
  geom_point()+
  scale_x_sqrt()+
  scale_color_brewer(type = "div")+
  facet_wrap(~LoanStatus)+
  xlab('Sqrt-root of LoanOriginalAmount')+
  ggtitle('Facet warp by LoanStatus')



