
  #import data from mysql

library(dplyr)
library(ggplot2)
mydb<-src_mysql(dbname="zlzp",host="127.0.0.1",user="root",password = "jcd0038")

src_tbls(mydb)
zlzp=tbl(mydb,"zlzp")
zlzp=tbl_df(zlzp)
zlzp#the column company encounter messy code

#fix the messy code problem
library(stringi)
stri_enc_get()
#GBK
stri_enc_detect2(zlzp$company[1])
#"utf-8"
zlzp$company<-stri_conv(zlzp$company,"UTF-8","GBK")
zlzp$degree<-stri_conv(zlzp$degree,"UTF-8","GBK")
zlzp$location<-stri_conv(zlzp$location,"UTF-8","GBK")
zlzp
#now is nice

#clean & prepare data
#move duplicated rows
zlzp<-zlzp[!duplicated(zlzp),]
nrow(zlzp)

library(tidyr)
zlzp<-separate(zlzp,location,c("area_big","area_small"),"-")
#选择工资大于4000的，因为有一部分低工资的工作是实习
ZLZP<-zlzp%>%
  filter(salary>4000)
nrow(ZLZP)



#smmary the data
ZLZP%>%
  select(salary,area_big)%>%
  group_by(area_big)%>%
  summarise(counts=n(),mean_salary=mean(salary))
ggplot(ZLZP,aes(salary,fill=area_big))+geom_density(alpha=0.4,na.rm = TRUE)+labs(fill="地区",x="工资（元/月）")+xlim(4000,40000)+theme(legend.position = c(0.9,0.8))
#one obsevation out that range


#analys the data in shanghai 
zlzp_sh<-ZLZP%>%
  filter(area_big=="上海",postdate<=as.Date("2017-01-13"))


#不同工作经验/学位之间的工资比较
zlzp_sh%>%
  group_by(degree)%>%
  summarise(n=n(),mean_salary=mean(salary))%>%
  arrange(desc(mean_salary))

ggplot(zlzp_sh,aes(exp,salary,color=degree))+geom_jitter(alpha=0.5,show.legend =FALSE,na.rm = TRUE)+geom_smooth(method="lm",se=FALSE,na.rm = TRUE)+ylim(0,40000)+xlab("工作经验（年）")+ylab("工资（元/月）")+labs(color="学历")

View(zlzp_sh)
#id432 的一家公司月薪达到惊人的10万/月，而且其它条件也不高，应该是该公司的失误,把10000输成100000。为了不影响预测，把这项数据去掉

zlzp_sh$degree<-as.factor(zlzp_sh$degree)
zlzp_sh_model<-zlzp_sh%>%
  filter(id!=432)%>%
  select(-id,-company,-postdate,-area_big,-area_small,-url)

save(zlzp_sh_model,file = "zlzp_sh_model.Rdata")

#公司分类
zlzp_sh<-zlzp_sh%>%
  filter(id!=432)

zlzp_sh$company_type<-rep(NA,nrow(zlzp_sh))

#公司分类
zlzp_sh$company_type[grepl("金融",zlzp_sh$company)]<-"金融"
zlzp_sh$company_type[setdiff(grepl("文化|广告",zlzp_sh$company),grepl("金融",zlzp_sh$company))]<-"文化"

zlzp_sh$company_type[setdiff(grepl("贸易|商务|电子商务",zlzp_sh$company),grepl("文化|广告|金融",zlzp_sh$company))]<-"商务"

zlzp_sh$company_type[setdiff(grepl("信息|咨询",zlzp_sh$company),grepl("文化|广告|金融|贸易|商务|电子商务",zlzp_sh$company))]<-"信息"
zlzp_sh$company_type[setdiff(grepl("网络|互联网",zlzp_sh$company),grepl("信息|咨询|文化|广告|金融|贸易|商务|电子商务",zlzp_sh$company))]<-"互联网"
zlzp_sh$company_type[is.na(zlzp_sh$company_type)]<-"其它"

zlzp_sh$company_type<-as.factor(zlzp_sh$company_type)

zlzp_sh%>%
  group_by(company_type)%>%
  summarise(n=n(),mean.salary=mean(salary))%>%
  arrange(desc(mean.salary),n)

#save data file
save(zlzp_sh,file="H:/R projects/zlzp/zlzp.Rdata")




