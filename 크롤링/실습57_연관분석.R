#연관분석 : 연관된 규칙을 찾는 무방향성 데이터 마이닝 기법
#연관 규칙은 특정 사건이 발생했을 때 함께 (빈번히)발생하는 또 다른 사건의 규칙이다.
#컬럼명은 영어로 써야 알이 이해 한다.

install.packages("arules")
library(arules)

cust <- read.csv("testdata/priori_data2.csv", stringsAsFactors = F)
cust
cust$sangpum[cust$irum=="홍길동"]

cust.list <-split(cust$sangpum, cust$irum) #이름을 기준으로 구매한 상품들을 짤라서 나눔
cust.list

#trainsaction type 으로 변환이 됨
cust.tran <- as(cust.list, "transactions") #as합수는 타입을 바꿔줌
cust.tran
class(cust.tran)
cust.rules <-apriori(cust.tran) # apriori 에서 transaction 함수를 달라고 함. 그래서 as라는 함수로 transaction 객체를 만듬 
cust.rules #4개의 룰이 만들어 졌음
summary(cust.rules)

inspect(cust.rules) # {자갈치 를 구매하고 맛동산을 구매한사람}  support지지도 0.2    1          confidence(1)보다 크면 양의 상관관계  1 
                    #
# (support confidence lift)지지도 신뢰도 향상도

# -------------------------------------------------------------
#customer <- read.csv("testdata/priori_data.csv", stringsAsFactors = F)
customer <- read.csv("testdata/priori_data.csv", stringsAsFactors = F) #stringsAsFactors == as.is=T
customer <- read.csv("testdata/priori_data.csv", as.is=T) #as.is=T 는 chr로 옴
str(customer)
customer

customer$sangpum[customer$irum=='홍길동'] #특정사람이 구매한 모든 상품 확인
#스플릿으로 사람들이 구매한 상품 나누기
customer.list<-split(customer$sangpum, customer$irum) #거래별 항목정리
customer.list

#거래별 항목정리 후 transaction 객체 생성
customer.tran<-as(customer.list,"transactions") #에러 (중복)
customer.list<-sapply(customer.list, unique) #customer.list 중복제거
customer.tran<-as(customer.list,"transactions")
customer.tran #트랜잭션 6개와 9개의 아이탬으로 만들어 졌음
summary(customer.tran) #자세한 정보 보기

#빈도수 따로보기
itemFrequency(customer.tran)

itemFrequency(customer.tran[,1:5]) #1열에서 5열까지

itemFrequencyPlot(customer.tran,main="지지도 1%이상 항목들") #비도에 대한 차트 그리기

itemFrequencyPlot(customer.tran,topN=5,main="지지도 상위 5% 항목들")

#연관 규칙 생성
customer.rules <- apriori(customer.tran) #신뢰도 0.8

#요약정보
summary(customer.rules) #80개의 룰이 만들어짐

#연관 규칙 확인하는 함수
inspect(customer.rules)

#부분적으로 보기 
#높은순으로 보기
inspect(sort(customer.rules,by ="lift")[1:5])
#낮은순으로 보기
inspect(sort(customer.rules,by ="lift",decreasing = FALSE)[1:5])
inspect(sort(customer.rules,by ="support")[1:5]) #지지도 별로, 신뢰도별 향상도별로 볼수 있다.

rule_subset<-subset(customer.rules, items %in% c("맛동산","짱구"))
inspect(rule_subset[1:5])
#시각화
install.packages("arulesViz")
library(arulesViz)

plot(customer.rules, method = "grouped")
plot(customer.rules, method = "graph")
