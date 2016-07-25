#StockDataBuilder

A1<-read.csv("Teva.csv",header = T,skip = 3,as.is = T,quote = "")
A2<-read.csv("Africa.csv",header = T,skip = 3,as.is = T,quote = "")
A3<-read.csv("Bezek.csv",header = T,skip = 3,as.is = T,quote = "")
A4<-read.csv("cil.csv",header = T,skip = 3,as.is = T,quote = "")
A5<-read.csv("Delek.csv",header = T,skip = 3,as.is = T,quote = "")
A6<-read.csv("discountInvesment.csv",header = T,skip = 3,as.is = T,quote = "")
A7<-read.csv("leumi.csv",header = T,skip = 3,as.is = T,quote = "")
A8<-read.csv("migdal.csv",header = T,skip = 3,as.is = T,quote = "")
A9<-read.csv("perigo.csv",header = T,skip = 3,as.is = T,quote = "")
A10<-read.csv("poalim.csv",header = T,skip = 3,as.is = T,quote = "")
A11<-read.csv("partner.csv",header = T,skip = 3,as.is = T,quote = "")
A12<-read.csv("Mizrahi.csv",header = T,skip = 3,as.is = T,quote = "")
CompanyNames<-c("Teva","Africa",'Bezek',"cil","Delek","discountInvesment",),

sum(A1$תאריך == A2$תאריך)
sum(A1$תאריך == A3$תאריך)
sum(A1$תאריך == A4$תאריך)
sum(A1$תאריך == A5$תאריך)
sum(A1$תאריך == A6$תאריך)
sum(A1$תאריך == A7$תאריך)
sum(A1$תאריך == A8$תאריך)
sum(A1$תאריך == A8$תאריך)
sum(A1$תאריך == A9$תאריך)
sum(A1$תאריך == A10$תאריך) #A10 contains dates from 2013 - perigo company
sum(A1$תאריך == A11$תאריך)
sum(A1$תאריך == A12$תאריך)



