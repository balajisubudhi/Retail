library(googleVis)

mydata=read.table("data1.csv",header=TRUE,sep=",")
mydata1=read.table("data2.csv",header=TRUE,sep=",")
mydata1$Date=as.Date(mydata1$Date,"%d-%b-%y")

mydata2=read.table("data3.csv",header=TRUE,sep=",")
mydata2$Date=as.Date(mydata2$Date,"%d-%b-%y")

mydata3=read.table("data4.csv",header=TRUE,sep=",")

mydata4=read.table("data5.csv",header=TRUE,sep=",")
mydata4=mydata4[1:540,]
mydata4$Date=as.Date(mydata4$Date,"%m/%d/%Y")

mydata5=read.table("data6.csv",header=TRUE,sep=",")
mydata5$Month=as.Date(mydata5$Month,"%d-%b-%y")