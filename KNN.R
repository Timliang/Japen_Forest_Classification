Project.Data=read.csv("/Users/Liang enhua/Desktop/学习/ERG/Project/Project/ForestTypes/training.csv")
Project=read.csv("/Users/Liang enhua/Desktop/学习/ERG/Project/Project/ForestTypes/testing.csv")

cset.seed(1)
sample_get = sample(1:nrow(Project.Data),nrow(Project.Data)*0.7)
sample_get = sample(1:nrow(a),nrow(a)*0.7)
Project.Data=Project.Data[,-1]#首先去除无用的两个predictors
Project.Data=Project.Data[,-1]
data_sample = a[sample_get,]
data_OutofSample = a[-sample_get,]

train.data.non_shares=Project.Data[,-59]
fix(Project)

a=subset(Project.Data,shares>=quantile(shares,0.89))
c=subset(Project.Data,shares<quantile(shares,0.89))
summary(a)
b=rep(0,60)

plot(Project.Data$kw_max_max,Project.Data$shares)
mean(c$kw_max_max)
plot(c$kw_max_max,c$shares)
dim(c[c$kw_max_max>800000,])

for (i in 3:60){
  b[i]=(mean(a[,i])-mean(c[,i]))*100/mean(c[,i])
  }
for (i in 1:60){
  if (b[i]<(-20)){
    print(names(c)[i])
    }
}

data_sample=data_sample[order(data_sample[,59]),]#根据shares的大小来排序
fix(Project.Data)
#最好的KNN
train.data=  cbind (data_sample$kw_avg_avg,data_sample$LDA_02,data_sample$data_channel_is_world,data_sample$is_weekend,data_sample$data_channel_is_socmed,data_sample$weekday_is_saturday,data_sample$LDA_04,
                    data_sample$data_channel_is_entertainment,data_sample$data_channel_is_tech,data_sample$kw_avg_max,data_sample$weekday_is_sunday,
                    data_sample$LDA_00,data_sample$num_hrefs,data_sample$global_subjectivity,data_sample$title_sentiment_polarity,data_sample$rate_negative_words,
                    data_sample$title_subjectivity,data_sample$LDA_01)
test.data=  cbind (data_OutofSample$kw_avg_avg,data_OutofSample$LDA_02,data_OutofSample$data_channel_is_world,data_OutofSample$is_weekend,data_OutofSample$data_channel_is_socmed,data_OutofSample$weekday_is_saturday,data_OutofSample$LDA_04,
                    data_OutofSample$data_channel_is_entertainment,data_OutofSample$data_channel_is_tech,data_OutofSample$kw_avg_max,data_OutofSample$weekday_is_sunday,
                    data_OutofSample$LDA_00,data_OutofSample$num_hrefs,data_OutofSample$global_subjectivity,data_OutofSample$title_sentiment_polarity,data_OutofSample$rate_negative_words,
                    data_OutofSample$title_subjectivity,data_OutofSample$LDA_01)

#KNN                    
train.data=  cbind (data_sample$n_tokens_title,data_sample$num_hrefs,data_sample$data_channel_is_entertainment,data_sample$kw_min_avg,data_sample$kw_max_avg,data_sample$kw_avg_avg,data_sample$self_reference_avg_sharess,data_sample$is_weekend,data_sample$LDA_02,data_sample$data_channel_is_world)
test.data=  cbind  (data_OutofSample$n_tokens_title,data_OutofSample$num_hrefs,data_OutofSample$data_channel_is_entertainment,data_OutofSample$kw_min_avg,data_OutofSample$kw_max_avg,data_OutofSample$kw_avg_avg,data_OutofSample$self_reference_avg_sharess,data_OutofSample$is_weekend,data_OutofSample$LDA_02,data_OutofSample$data_channel_is_world)
train.shares= data_sample$shares
library(FNN)
knn.fit=knn.reg(train.data, test = train.data, c$shares, k = 2, algorithm=c("brute"))
mean((knn.fit$pred-(c$shares))^2)

train.data=cbind(n_unique_tokens,n_non_stop_words,num_hrefs,num_imgs,num_videos,data_channel_is_lifestyle,data_channel_is_socmed,kw_max_min,kw_min_avg,kw_max_avg,kw_avg_avg,self_reference_min_shares,self_reference_max_shares,self_reference_avg_sharess,weekday_is_saturday,weekday_is_sunday,is_weekend,LDA_03,title_sentiment_polarity,abs_title_sentiment_polarity,data_channel_is_bus,data_channel_is_world,LDA_02)[a,]

#linear regression
attach(Project.Data)
lm.fit=lm(shares~kw_avg_avg+LDA_02+data_channel_is_world+is_weekend+data_channel_is_socmed+weekday_is_saturday+LDA_04+
                  data_channel_is_entertainment+data_channel_is_tech+kw_avg_max+weekday_is_sunday+LDA_00+num_hrefs+global_subjectivity+title_sentiment_polarity+rate_negative_words
                  +title_subjectivity+LDA_01, data=data_sample)
          



#KNN and CV
knn.fit=knn.reg(train.data, test = NULL, train.shares, k = 1000, algorithm=c("brute"))

Project.Data=Project.Data[order(Project.Data[,59]),]#根据shares的大小来排序
fix(Project.Data)#查看data
print(object.size(Project.Data),units="Mb")#看数据大小

#用boxplot来稍微观察一下data

par(mfrow=c(1,2))#将绘图窗口划为1行两列，同时显示两图  
dotchart(Project.Data$shares)#绘制单变量散点图,多兰图
boxplot(Project.Data$shares)
mean(Project.Data$shares)
title("异常值检测箱线图")


q1=quantile(Project.Data$shares, 0.25)-1.5*(quantile(Project.Data$shares, 0.75)-quantile(Project.Data$shares, 0.25)) 
q99=quantile(Project.Data$shares, 0.75)+1.5*(quantile(Project.Data$shares, 0.75)-quantile(Project.Data$shares, 0.25)) 
Project.Data[Project.Data$shares<q1,]$shares=0
Project.Data[Project.Data$shares>q99,]$shares=0


#用盖帽法处理异常值

q1=quantile(Project.Data$shares, 0.025) 
q99=quantile(Project.Data$shares, 0.975)
Project.Data[Project.Data$shares<q1,]$shares=q1
Project.Data[Project.Data$shares>q99,]$shares=q99
summary(Project.Data$shares)
plot(Project.Data$shares)


#检测每一个变量的Var,从而对var过大的变量进行处理
Var_Each_Predictors=rep(0,58)
for (i in 1:58){Var_Each_Predictors[i]=var(Project.Data[,i+1])}

#判断var是否过大
for (i in 1:58){
  if (var(Project.Data[,i+1])>1000000){
    q1=quantile(Project.Data[,i+1], 0.25)-1.5*(quantile(Project.Data[,i+1], 0.75)-quantile(Project.Data[,i+1], 0.25)) 
    q99=quantile(Project.Data[,i+1], 0.75)+1.5*(quantile(Project.Data[,i+1], 0.75)-quantile(Project.Data[,i+1], 0.25))
    if (q1<min(Project.Data[,i+1])){
      Project.Data[Project.Data[,i+1]>q99,i+1]=-10000
    }
    else{
      Project.Data[Project.Data[,i+1]<q1,i+1]=-10000
      Project.Data[Project.Data[,i+1]>q99,i+1]=-10000
    }
  }
}

for (i in 2:59){ 
  Project.Data=Project.Data[Project.Data$shares!=0,]
}

  

#后面内容正在完善中！！！
#对数据中每一个变量做相似处理,但是行不通
for (i in 1:58){
  q1=quantile(Project.Data[,i], 0.001) 
  q99=quantile(Project.Data[,i], 0.999)
  Project.Data[Project.Data[,i]<q1,][,i]=q1
  Project.Data[Project.Data[,i]>q99,][,i]=q99 }

#分箱法
for (i in 1:59){
  x[,i]=apply(x,i,mean)[i]
  }              # 用第一行的均值代替第一行中的数据  



xi=1.1  
sd.s=sd(saledata[complete.cases(Project.Data),]$"num_keywords.")  
mn.s=mean(saledata[complete.cases(Project.Data),]$"num_keywords.")
points(xi,mn.s,col="red",pch=18)  
arrows(xi, mn.s - sd.s, xi, mn.s + sd.s, code = 3, col = "pink", angle = 75, length = .1)  
text(rep(c(1.05,1.05,0.95,0.95),length=length(sp$out)),labels=sp$out[order(sp$out)],  
     sp$out[order(sp$out)]+rep(c(150,-150,150,-150),length=length(sp$out)),col="red") 



train.data= cbind (n_tokens_title,num_hrefs,data_channel_is_entertainment,kw_min_avg,kw_max_avg,kw_avg_avg,self_reference_avg_sharess,is_weekend)[sample_get,]
test.data= cbind (n_tokens_title,num_hrefs,data_channel_is_entertainment,kw_min_avg,kw_max_avg,kw_avg_avg,self_reference_avg_sharess,is_weekend)[!sample_get,]
train.shares= shares[sample_get]

