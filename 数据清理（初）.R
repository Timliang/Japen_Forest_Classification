Project.Data=read.csv("/Users/Liang enhua/Desktop/学习/ERG/Homework/Project/OnlineNewsPopularity/OnlineNewsPopularity.csv", header=T,na.strings="?")


Project.Data=Project.Data[,-1]#首先去除无用的两个predictors
Project.Data=Project.Data[,-1]

Project.Data=Project.Data[order(Project.Data[,59]),]#根据shares的大小来排序
fix(Project.Data)#查看data
print(object.size(Project.Data),units="Mb")#看数据大小

#用boxplot来稍微观察一下data

par(mfrow=c(1,2))#将绘图窗口划为1行两列，同时显示两图  
dotchart(Project.Data$shares)#绘制单变量散点图,多兰图  
title("异常值检测箱线图")

#用盖帽法处理异常值

q1=quantile(Project.Data$shares, 0.001) 
q99=quantile(Project.Data$shares, 0.999)
Project.Data[Project.Data$shares<q1,]$shares=q1
Project.Data[Project.Data$shares>q99,]$shares=q99
summary(Project.Data$shares)
plot(Project.Data$shares)

q1=quantile(Project.Data[,1], 0.001) 
q99=quantile(Project.Data[,1], 0.999)
Project.Data[Project.Data[,1]<q1,][,1]=q1
#检测每一个变量的Var,从而对var过大的数据进行处理
Var_Each_Predictors=rep(0,58)
for (i in 1:58){
  Var_Each_Predictors[i]=var(Project.Data[,i+1])
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