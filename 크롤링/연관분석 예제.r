## 연관분석 예제 (https://blog.naver.com/leedk1110/220785911828, https://m.blog.naver.com/PostView.nhn?blogId=leedk1110&logNo=220788082381&proxyReferer=https%3A%2F%2Fwww.google.com%2F)

library(arules)
library(arulesViz) 

data("Groceries") 
Groceries
class(Groceries)

summary(Groceries)
inspect(Groceries[1:10])

itemFrequency(Groceries) 
itemFrequency(Groceries,type='absolute')
itemFrequencyPlot(Groceries, type='absolute')
itemFrequencyPlot(Groceries, topN=20, type='absolute') 

## ----------------------------------------------------
  
mtx <- matrix(c(1,1,1,0,0,
                1,1,1,1,0,
                0,0,1,1,0,
                0,1,0,1,1,
                0,0,0,1,0), ncol=5, byrow=T)
rownames(mtx) <- paste0("ti",1:5)
colnames(mtx) <- letters[1:5]
mtx

mtx.trans <- as(mtx, 'transactions')   
mtx.trans
summary(mtx.trans)

inspect(mtx.trans)

df <- as.data.frame(mtx)
df.trans <- as(df,"transactions") # error 
str(df)

df <- as.data.frame(sapply(df,as.logical)) 
str(df)

df.trans <- as(df,'transactions')  
df.trans
summary(df.trans)

inspect(df.trans)

list <- list(tr1=c("a","b","c"),
             tr2=c("a","d"),
             tr3=c("b","e"),
             tr4=c("a","d","e"),
             tr5=c("b","c","d"))
list

list.trans <- as(list,'transactions')
list.trans
summary(list.trans)

inspect(list.trans)

list.trans@data
list.trans@itemInfo

Groceries@data  
Groceries@itemInfo

level1 <- c("음료","음료","햄버거","햄버거","피자")
list.trans@itemInfo <- cbind(list.trans@itemInfo,level1)
list.trans@itemInfo
inspect(list.trans)

list.trans2 <- aggregate(list.trans, 'level1')
list.trans2
inspect(list.trans2)
image(list.trans2)


# -----------------------------------------------------------------------


install.packages(c("arules","arulesViz"))
library(arules)
library(arulesViz)

data("Groceries") 
Groceries

# 2회이상 거래가 이루어진 2품목이상 10품목이하의 itemset의 support를 구하는 함수
ecl <- eclat(Groceries, parameter=list(support=2/9835,minlen=2 ,maxlen=10))
ecl
inspect(sort(ecl)[1:50])
summary(ecl)

rule <- apriori(Groceries, parameter=list(minlen=2))
rule  

rule <- apriori(Groceries, control=list(verbos=F), parameter=list(support=50/9835, confidence=0.6 ,minlen=2)) 
rule
summary(rule)
inspect(rule)

rule <- sort(rule, by='lift')
inspect(rule[1:20])

rule <- apriori(Groceries, parameter=list(support=0.0015, minlen=2),
                appearance=list(none="other vegetables"))
inspect(rule)

rule <- apriori(Groceries, parameter=list(support=0.0008, minlen=2),
                appearance=list(none=c("other vegetables","whole milk")))
inspect(rule)

rule_s <- apriori(Groceries, parameter=list(support=50/9835,confidence=0.3, minlen=2, maxlen=6),
                  appearance=list(rhs="soda",default='lhs'))
rule_s <- sort(rule_s, by='lift')
inspect(rule_s)

rule <- apriori(Groceries, parameter=list(support=5/9835,confidence=0.3 ,minlen=2, maxlen=6),
                appearance=list(lhs='yogurt',default='rhs'))
inspect(rule)

gro <- aggregate(Groceries, 'level2')
rule <- apriori(gro, parameter = list(confidence=0.5))
inspect(rule)

plot(rule_s)
plot(rule_s,method="grouped")
plot(rule_s,method="graph")
plot(rule_s,method="graph",interactive = T)
plot(rule_s,method="paracoord")