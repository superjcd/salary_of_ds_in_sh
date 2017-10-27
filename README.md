# 用R的机器学习方法来预测分析在沪的数据分析师的期望工资
 数据挖掘部分使用了reuqests包和Beautifulsoup中以及正则表达式来抓取需要的信息，算是最最基础的爬虫了。因为数据量不大所以也够用了。
 分析的部分，结合了《An Introdunction to Statistical Learning》的关于机器学习的知识，拟合了多种机器学习模型。
 关于模型的比较，用较早爬取的数据做为训练集），后面的爬取的数据为验证集合，分析比较了各种模型的预测效果（即比较预测和实际值的误差），其中随机森林的预测的效果最佳，误差在1千/月左右。具体分析结果可见PDF文件。
 项目的不足点有：
 1 数据源比较少，智联招聘的数据不如其他的招聘平台来的多。
 2 因为是一个比较简陋的爬虫，爬虫性能有待提高。
 3 预测变量是自己添加的并通过正则表达式查找的，所以有些变量没能捕捉到，能结合NLP的话
 效果可能会更好。
 4 多数预测变量的值域只有[0，1]（表示某一要求的有无，比如对sql要求的有无），限制了Lasso回归等线性模型的发挥，如果能量化要求的程度的
 程度或许会更好。
 具体预测结果见文档中的PDF文件
