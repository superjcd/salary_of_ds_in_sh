load(file="H:/R projects/zlzp/zlzp_sh_model.Rdata")
options(digits = 0)

#初步的线性回归
library(dplyr)
library(ggplot2)
library(leaps)
contrasts(as.factor(zlzp_sh_model$degree))#based on 本科
fit_lm<-lm(salary~.,data=zlzp_sh_model)

summary_fit<-summary(fit_lm)
summary_fit_coef<-as.data.frame(coef(summary_fit))
summary_fit_coef$coef<-rownames(summary_fit_coef)

as.data.frame(summary_fit_coef)%>%
  select(coef,Estimate,`Pr(>|t|)`)%>%
  arrange(desc(Estimate))


#predict function for regsubsets
predict_reg<-function(model,newdata,id){
  formula<-as.formula(model$call[[2]])
  matrix<-model.matrix(formula,data=newdata)
  coef<-coef(model,id)
  vars<-names(coef)
  matrix[,vars]%*%coef
}

#k-fold cross validation
k<-10
set.seed(1)
folds<-sample(1:k,nrow(zlzp_sh_model),rep=TRUE)
mat.error<-matrix(NA,k,27,dimnames = list(NULL,paste(1:27)))
mat.cp<-matrix(NA,k,27,dimnames = list(NULL,paste(1:27)))
#paste method turn object to string
for (j in 1:k){
  reg.fit<-regsubsets(salary~.,data = zlzp_sh_model[folds!=j,],nvmax = 27)
  reg_sum<-summary(reg.fit)
  for (i in 1:27){
    reg_pre<-predict_reg(reg.fit,zlzp_sh_model[folds==j,],id=i)
    mat.error[j,i]<-mean((zlzp_sh_model$salary[folds==j]-reg_pre)^2)
    mat.cp[j,i]<-reg_sum$cp[i]
  }
}
#we have mean error matrix
mean.cv.error<-apply(mat.error,2,mean)
which.min(mean.cv.error)

#we have one standard-dev of mean error
sd.mean.error<-sd(mean.cv.error)

#we have mean.cp matrix
mean.cp<-apply(mat.cp,2,mean)


df.mean<-data.frame(id=1:27,mean.error=mean.cv.error)
df.cp<-data.frame(id=1:27,cp=mean.cp,mean.error=mean.cv.error)

#全子集回归的可视化结果
ggplot(df.mean,aes(mean.error,reorder(id,mean.error)))+geom_point()+geom_vline(xintercept = sd.mean.error+min(mean.cv.error))+ylab("id")

#subset df.bic by one standard-error rule
df.select<-subset(df.cp,mean.error<sd.mean.error+min(mean.cv.error))
ggplot(df.cp,aes(cp,reorder(id,mean.error)))+geom_point()+
  geom_point(data=df.select,color="red",size=1.5)+ylab("id")
#结论
#1)从交叉验证的结果来看25变量的mean.cv.error最小.
#2)但是和其它的模型差距并不大，应用one standard-error  rule,把落在一个离最小mean.error一个standard-error模型作为候选（共12个），查看这些模型的Cp统计量（不选BIC，因为既有连续性也有离散变量）
#3）17变量模型的cp值最小。结合奥姆剃刀法则，应选取变量相对较少的模型，所以综合来看选择17变量模型。

#apply this 17-variable model to full data sets
reg_selected<-regsubsets(salary~.,data=zlzp_sh_model,nvmax = 27)
coef(reg_selected,id=17)


#dicision tree 
library(tree)
set.seed(118)
zlzp_tree<-tree(salary~.,data=zlzp_sh_model)
summary(zlzp_tree)
#only get the train error
plot(zlzp_tree)
text(zlzp_tree,pretty=0)
#可以看到exp是最为重要的因素


#tree pruning
cv.out<-cv.tree(zlzp_tree)
#names(zlzp_prune_tree)
cv.out
plot(cv.out$size,cv.out$dev,col="red",type="b")
#size 7 is good
zlzp_prune<-prune.tree(zlzp_tree,best = 7)
plot(zlzp_prune)
text(zlzp_prune,pretty = 0)#pretty=0 show the catagory


#pprepare for test data
#connect to new database
mydb<-src_mysql(dbname="zlzp",host="127.0.0.1",user="root",password = "jcd0038")
src_tbls(mydb)
zlzp_new=tbl(mydb,"zlzp")#new data add to the same table
zlzp_new=tbl_df(zlzp_new)
zlzp_new#the column company encounter messy code again

#fix the messy code
library(stringi)
stri_enc_detect2(zlzp_new$company[1])
#"utf-8"
zlzp_new$company<-stri_conv(zlzp_new$company,"UTF-8","GBK")
zlzp_new$degree<-stri_conv(zlzp_new$degree,"UTF-8","GBK")
zlzp_new$location<-stri_conv(zlzp_new$location,"UTF-8","GBK")
zlzp_new

#通过时间选择新的数据，之前爬取的数据都是2017-1-13之前的
zlzp_test<-zlzp_new%>%
  filter(as.Date(postdate)>as.Date("2017-1-13"))

zlzp_test<-separate(zlzp_test,location,c("area_big","area_small"),"-")

zlzp_test<-zlzp_test[!duplicated(zlzp_test),]
zlzp_test_model<-zlzp_test%>%
  filter(salary>4000,degree!="博士")%>%
  mutate(Degree=as.factor(degree))%>%
  select(-degree,-id,-company,-postdate,-area_big,-area_small,-url)%>%
  rename(degree=Degree)


#prediction
zlzp_test_model<-na.omit(zlzp_test_model)
nrow(zlzp_test_model)
model_name<-rep(NA,8)
mean_error<-rep(NA,8)

pre_lm<-predict(fit_lm,newdata = zlzp_test_model)
model_name[1]<-"lm"
mean_error[1]<-mean(abs(pre_lm-zlzp_test_model$salary))
#2087
data1<-data.frame(realvalue=zlzp_test_model$salary,prediction=pre_lm,model=rep("lm",703))

#regsubsets
reg_test<-regsubsets(salary~.,data=zlzp_sh_model,nvmax = 27)
pre_subsets<-predict_reg(reg_test,newdata = zlzp_test_model,id=17)
model_name[2]<-"regsubset"
mean_error[2]<-mean(abs(pre_subsets-zlzp_test_model$salary))
#2126
data2<-data.frame(realvalue=zlzp_test_model$salary,prediction=pre_subsets,model=rep("regsubsets",703))

#lasso regression
library(glmnet)
x<-model.matrix(salary~.,data=zlzp_sh_model)[,-1]
y<-zlzp_sh_model$salary
zlzp.lasso<-glmnet(x,y,alpha = 1)
set.seed(0)
cv.lasso.out<-cv.glmnet(x,y,alpha = 1)
s_best_lasso<-cv.lasso.out$lambda.min
#use this s_best_lasso to predict test-error
new_x<-model.matrix(salary~.,data=zlzp_test_model)[,-1]
pre_lasso<-predict(zlzp.lasso,s=s_best_lasso,newx = new_x)
model_name[3]<-"lasso"
mean_error[3]<-mean(abs(zlzp_test_model$salary-pre_lasso))
#4319
data3<-data.frame(realvalue=zlzp_test_model$salary,prediction=c(pre_lasso),model=rep("lasso",703))

#ridge regression
zlzp.ridge<-glmnet(x,y,alpha = 0)
set.seed(0)
cv.ridge.out<-cv.glmnet(x,y,alpha = 0)
s_best_ridge<-cv.lasso.out$lambda.min
#use this s_best_ridge to predict test-error
pre_ridge<-predict(zlzp.ridge,s=s_best_ridge,newx = new_x)
model_name[4]<-"ridge"
mean_error[4]<-mean(abs(zlzp_test_model$salary-pre_ridge))
#4529
data4<-data.frame(realvalue=zlzp_test_model$salary,prediction=c(pre_ridge),model=rep("ridge",703))

#decision tree
pre_tree<-predict(zlzp_tree,newdata = zlzp_test_model)
model_name[5]<-"decision tree"
mean_error[5]<-mean(abs(zlzp_test_model$salary-pre_tree))
#2068
data5<-data.frame(realvalue=zlzp_test_model$salary,prediction=pre_tree,model=rep("tree",703))

#tree pruning
pre_prune<-predict(zlzp_prune,newdata = zlzp_test_model)
model_name[6]<-"prunned tree"
mean_error[6]<-mean(abs(zlzp_test_model$salary-pre_prune))
#2130
data6<-data.frame(realvalue=zlzp_test_model$salary,prediction=pre_prune,model=rep("prunned tree",703))

#random forest(mtry argument indicates the number of varibal used to predict test error)
set.seed(0)
error_rf<-rep(0,10)
for(i in 8:17){
  fit_rf<-randomForest(salary~.,data = zlzp_sh_model,mtry=i,importance=TRUE)
  pre_rf<-predict(fit_rf,newdata = zlzp_test_model)
  error_rf[i-7]<-mean(abs(pre_rf-zlzp_test_model$salary))
}
error_rf
which.min(error_rf)
#when mtry=17(17 varibles),the error is smalllest,which is 1060
rf.zlzp<-randomForest(salary~.,data=zlzp_sh_model,mtry=17,importance=TRUE)

pre_randomforest<-predict(rf.zlzp,newdata = zlzp_test_model)
model_name[7]<-"random forest"
mean_error[7]<-mean(abs(zlzp_test_model$salary-pre_randomforest))
#1059
data7<-data.frame(realvalue=zlzp_test_model$salary,prediction=pre_randomforest,model=rep("randomforest",703))

#boosted tree
error_bt<-rep(0,5)
set.seed(0)
for(i in 1:5){
  fit_boost<-gbm(salary~.,data=zlzp_sh_model,n.trees = 5000,interaction.depth = i,distribution = "gaussian")
  pre_bt<-predict(fit_boost,newdata=zlzp_test_model,n.trees=5000)
  error_bt[i]<-mean(abs(zlzp_test_model$salary-pre_bt))
}
error_bt
which.min(error_bt)
#5
set.seed(0)
boost.zlzp<-gbm(salary~.,data=zlzp_sh_model,n.tree=5000,interaction.depth = 5,distribution = "gaussian")
pre_bt<-predict(boost.zlzp,newdata = zlzp_test_model,n.trees = 5000)
model_name[8]<-"boostes tree"
mean_error[8]<-mean(abs(zlzp_test_model$salary-pre_bt))
#1725
data8<-data.frame(realvalue=zlzp_test_model$salary,prediction=pre_bt,model=rep("boosted tree",703))

#prepare dataframe for data visiulazation
df_me<-data.frame(model=model_name,mean.error=mean_error)
arrange(df_me,mean.error)

df_predict<-rbind(data1,data2,data3,data4,data5,data6,data7,data8)

#data-vis
ggplot(df_me,aes(mean.error,reorder(model,mean.error)))+geom_point(pch=20,col="red",size=4)+labs(title=" 各种方法的预测误差值",y="模型",x="平均误差（元）")+theme(axis.text.y = element_text(face = "bold",size=8),plot.title = element_text(face = "bold",size=16),axis.title = element_text(face="bold",size=10))

df.rf<-as.data.frame(importance(rf.zlzp))
df.rf$varible<-rownames(df.rf)
names(df.rf)

ggplot(df.rf,aes(`%IncMSE`,reorder(varible,`%IncMSE`)))+geom_point()+labs(title='变量的相对重要性（随机森林）',y="变量")+theme(axis.text = element_text(face="bold"),plot.title = element_text(face="bold",size=17))

base<-ggplot(df_predict,aes(realvalue,prediction))+geom_point(col='red')+geom_abline(size=1.2,lty="dashed")+scale_y_continuous(breaks = c(5000,10000,15000,20000,25000,30000),labels = scales::unit_format("k",1e-3))+scale_x_continuous(breaks = c(5000,10000,15000,20000,25000,30000),labels = scales::unit_format("k",1e-3))+facet_wrap(~model)
base+labs(x="实际工资（元/月）",y="预测公测（元/月)")+theme(strip.text=element_text(face="bold"))
#tree method  just use a fraction of varibles,so many points overlapped

#我的期望工资
```{r}
a<-as.integer(0)
b<-as.integer(1)
jcd<-data.frame(salary=0,exp=a,degree="硕士",statistics=a,math=a,computer=a,information=a,finance=a,analysis=b,dataminning=b,monitoring=b,communication=b,customerbehavio=a,bigdata=a,excel=a,office=a,ppt=b,spss=b,r=b,sas=a,stata=a,python=b,java=a,hadoop=b,spark=a,hive=a,sql=a)
levels(jcd$degree)<-levels(zlzp_sh_model$degree)#this vary important!without this you will always encounter the error 
#use randon forest model to predict
pre_jcd1<-predict(rf.zlzp,newdata = jcd)
pre_jcd1
#1.139811e+04 

#use boosted tree
pre_jcd2<-predict(boost.zlzp,newdata=jcd,n.trees = 5000)
pre_jcd2
#1.199366e+04
#I prefer the boosted tree though the test-error is little bit higher than random forest!
