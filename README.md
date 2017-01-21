# salary_of_ds_in_sh
To analysis the salary that companies in shanghai are willing to pay for a data scientist/analysist ,i wrote this small project.
It contain 3 parts as following:
1.zhilianzhaoping.py:A web crawler that scrape the  web site named 智联招聘(http://www.zhaopin.com/)，which is one of the biggest on-line recruit website.

2.preparedata.r:clean the data retrived by web scrapping.Put all cleanned data into a neat dataframe.

3.modeling&visiulization.r:Appled some supervised statistical learning method to model the dataframe and compare the test-error among these methods.

As a result,in this paticular case,randomforest method perfommed best and the test-error is only about one thousand RMB.Given the big range of the salary,this is quite a good prediction.
