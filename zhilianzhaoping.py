#-*-coding:utf-8-*-
#the pages of hangzhou has a length about 10,shanghai has about 20
from bs4 import BeautifulSoup
from urllib.request import urlopen
import re
import pymysql
from urllib.error import HTTPError
#import csv

#creat a connection to mysql
conn=pymysql.connect(host="127.0.0.1",port=3306,user="root",passwd="jcd0038",db="mysql",charset="utf8")
cur=conn.cursor()
cur.execute("use zlzp")

skilllist=['统计学','数学','计算机','信息技术','金融','分析','挖掘','监测','沟通','客户行为','大数据','excel',
          'office','ppt','spss','r','sas','stata','python','java','hadoop','spark','hive','sql']

print(len(skilllist))
#create a list that has the value of 45 0s
valuelist=[]
for i in range(len(skilllist)):
    valuelist.append(0)

#convert two list into dict
dict=dict(list(zip(skilllist,valuelist)))
print(dict)


#turn numeric strings into numeric ones and return an mean
def addtozlzp(company,salary,location,postdate,exp,degree,statistics,math,computer,information,finance,
              analysis,dataminning,monitoring,communication,customerbehavio,bigdata,excel,office,ppt,
              spss,r,sas,stata,python,java,hadoop,spark,hive,sql,url):
    cur.execute("select * from zlzp_test where company=%s and salary=%s",(company,salary))
    if cur.rowcount==0:
        cur.execute("insert into zlzp_test(company,salary,location,postdate,exp,degree,statistics,math,computer,information,finance,analysis,dataminning,monitoring,communication,customerbehavio,bigdata,excel,office,ppt,spss,r,sas,stata,python,java,hadoop,spark,hive,`sql`,url) value(%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)",
                    (company,salary,location,postdate,exp,degree,statistics,math,computer,information,finance,analysis,dataminning,monitoring,communication,customerbehavio,bigdata,excel,office,ppt,spss,r,sas,stata,python,java,hadoop,spark,hive,sql,url))
        conn.commit()
    else:
        print("existed already")

def turntoint(A):
    A=re.findall("(\d+)",A,re.S)
    #A is a list contains numeric strs
    list=[]
    if len(A)==0:
        list.append(0)
        return(list[0])
    else:
        for a in A:
            list.append(int(a))

        avragenum=sum(list)/len(list)
        return(avragenum)

#find patterns in the job discription and scroe
def findpattern(skilllist,jobdiscription):
    for skill in skilllist:
        re_len=len(re.findall(skill,jobdiscription,re.S))
        if re_len > 0:
            dict[skill]=1
        else:
            dict[skill]=0



def getcontent(url,i):

    if i<30:
        try:
            html=urlopen(url)
        except HTTPError as e:
            print(e)

        bsobj=BeautifulSoup(html,'html.parser')
        nextpage=bsobj.find("a",{"class":"next-page"}).attrs["href"]
        print("nextpage is:",nextpage)
        tds=bsobj.findAll("td",{"class":"zwmc"})

        for td in tds:
            url=td.find("a").attrs["href"]
            print(url)
            try:
                html2=urlopen(url)
            except HTTPError as e:
                continue
            bsobj2=BeautifulSoup(html2,'html.parser')

            try:
                company=bsobj2.find("div",{"class":"inner-left fl"}).find("h2").get_text()
            except AttributeError as e:
                continue

            info_body=bsobj2.find("ul",{"class":"terminal-ul clearfix"}).findAll("li")
            salary=turntoint(info_body[0].get_text())
            location=info_body[1].get_text().split("：")[1]
            postdate=info_body[2].get_text().split("：")[1]
            exp=turntoint(info_body[4].get_text().split("：")[1])
            degree=info_body[5].get_text().split("：")[1]
            #print(company,salary,location,postdate,exp,degree)

            skills=bsobj2.find("div",{"class":"tab-inner-cont"}).get_text().strip("\n").strip(" ").lower()
           #return skills
           #score for each pattern if exist
            findpattern(skilllist,skills)
            #专业
            statistics=dict["统计学"]
            math=dict["数学"]
            computer=dict["计算机"]
            information=dict['信息技术']
            finance=dict['金融']
            #print(statistics,math,computer,information,finance)
            #能力
            analysis=dict['分析']
            dataminning=dict['挖掘']
            monitoring=dict['监测']
            communication=dict['沟通']
            customerbehavio=dict['客户行为']
            bigdata=dict['大数据']
            #print(analysis,dataminning,monitoring,communication,customerbehavio,bigdata)
            #office技能
            excel=dict['excel']
            office=dict['office']
            ppt=dict['ppt']
            #print(excel,office,ppt)
            #统计工具
            spss=dict['spss']
            r=dict['r']
            sas=dict['sas']
            stata=dict['stata']
            #print(spss,r,sas,stata)
            #脚本语言
            python=dict['python']
            java=dict['java']
            #print(python,java)
            #大数据工具
            hadoop=dict['hadoop']
            spark=dict['spark']
            hive=dict['hive']
            sql=dict['sql']
            print(hadoop,spark,hive,sql)
            addtozlzp(company,salary,location,postdate,exp,degree,statistics,math,computer,information,finance,
              analysis,dataminning,monitoring,communication,customerbehavio,bigdata,excel,office,ppt,
              spss,r,sas,stata,python,java,hadoop,spark,hive,sql,url)


        i=i+1
        getcontent(nextpage,i)

getcontent("http://sou.zhaopin.com/jobs/searchresult.ashx?jl=%E4%B8%8A%E6%B5%B7&kw=%E6%95%B0%E6%8D%AE%E5%88%86%E6%9E%90&kt=3&isadv=0&sg=7c8c16e0a15f425091d69e7c31bf4445&p=8",1)
