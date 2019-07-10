## https://developers.naver.com/main/ # 네이버 개발자 계정 홈페이지 

urlStr <- "https://openapi.naver.com/v1/search/blog.xml?" # 기본 url 생성 
searchString <- "query=신혼부부" # 쿼리생성 
searchString <- iconv(searchString, to="UTF-8") # 인코딩 
searchString <- URLencode(searchString)
searchString

etcString <- "&display=100&start=1&sort=sim"

reqUrl <- paste(urlStr, searchString, etcString, sep="")
reqUrl # 요청할 url 생성 

install.packages("httr")
library(httr)

clientid <- "GC_RIpLxQUqT9NK1OkmC" # 개인 api id 값
clientSecret <- "bLbCIIo2GT" # 개인 apu secret 값

apiResult <- GET(reqUrl, add_headers("X-Naver-Client-Id"=clientid, 
                                     "X-Naver-Client-Secret"=clientSecret))

apiResult # Status 값이 200이어야 정상. 500 이면 시스템 에러 
str(apiResult)

apiResult$content

result <- rawToChar(apiResult$content)
result
Encoding(result) <- "UTF-8"
result

library(rJava)
library(KoNLP)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
useSejongDic()
library(dplyr)
library(stringr)


babymom <- result
babymom <- gsub("<(\\/?)(\\w+)*([^<>]*)>", "", babymom)
babymom <- gsub("[[:punct:]]", "", babymom) # 문장부호 제거
babymom <- gsub("[[:cntrl:]]","",babymom) # 특수문자 제거
babymom <- gsub("[A-z]", "", babymom) # 모든 영문자 제거
babymom <- gsub("[0-9]", "", babymom) # 숫자 제거
babymom <- gsub(" +", "", babymom)
babymom <- gsub("^","", babymom)
babymom <- gsub("ㅋ","", babymom)
babymom <- gsub("ㅎ","", babymom)
babymom <- gsub("ㅜ","", babymom)
babymom <- gsub("naver","",babymom)
babymom <- gsub("blog","",babymom)
babymom <- gsub("https","",babymom)
babymom <- gsub("link","",babymom)
babymom <- gsub("title","",babymom)
babymom <- gsub("com","",babymom)
babymom <- gsub(" ","",babymom)
babymom <- sapply(result,extractNoun,USE.NAMES=F)
babymom <- Filter(function(x) {nchar(x)>=2 & nchar(x)<=4}, babymom)
babymom <- unlist(babymom)
wordcount <- table(babymom)
head(sort(wordcount,decreasing=T),100)
require(wordcloud2)
wordcloud2(wordcount[wordcount>2],size=8,col="random-dark",rotateRatio=0.5,
           backgroundColor="white",shape="circle")







## sentimental analysis
# 출처 : https://github.com/park1200656/KnuSentiLex (군산대 감성사전)
# site : https://goo-eungs.tistory.com/22 
# 서울연구원 : https://www.si.re.kr/search/node?keys=%EC%B6%9C%EC%82%B0&type%5B%5D=infographics&ptimes=total&created%5Bmin%5D=&created%5Bmax%5D=

library(plyr)
library(stringr)

setwd("D:/Workplace/StatisticsKorea_Big_Data_Project/크롤링")

positive <- readLines("D:/Workplace/StatisticsKorea_Big_Data_Project/크롤링/군산대 감성사전/positive.txt",
                      encoding="EUC-KR")
positive <- positive[-1]
negative <- readLines("D:/Workplace/StatisticsKorea_Big_Data_Project/크롤링/군산대 감성사전/negative.txt",
                      encoding="EUC-KR")
negative <- negative[-1]

require(plyr)

sentimental <- function(sentences,positive,negative) {
  
  scores <- laply(sentences,function(sentence,positive,negative) {
    sentence <- gsub('[[:punct:]]','',sentence) # 문장부호 제거
    sentence <- gsub('[[:cntrl:]]','',sentence) # 특수문자 제거
    sentence <- gsub('\\d+','',sentence) # 숫자 제거
    
    word.list <- str_split(sentence,'\\s+') # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개이상)
    words <- unlist(word.list) # unlist() : list를 vector 객체로 구조변경
    
    pos.matches <- match(words,positive) # words의 단어를 positive에서 matching
    neg.matches <- match(words,negative)
    
    pos.matches <- !is.na(pos.matches) # NA 제거, 위치(숫자)만 추출
    neg.matches <- !is.na(neg.matches)
    
    score <- sum(pos.matches) - sum(neg.matches) # 긍정 - 부정
    return(score)
  }, positive, negative)

  scores.df <- data.frame(score=scores,text=sentences)
  return(scores.df)
}

result <- sentimental(babymom,positive,negative)
result$color[result$score >= 1] <- "blue"
result$color[result$score == 0] <- "green"
result$color[result$score < 0] <- "red"

table(result$color)

result$remark[result$score >= 1] <- "긍정"
result$remark[result$score == 0] <- "중립"
result$remark[result$score < 0] <- "부정"

sentiment_result <- table(result$remark)

pie(sentiment_result,main="감성분석 결과",col=c("blue","red","green"),radius=0.8)

library(ggplot2)
library(dplyr)

table(result$remark)

remark <- c("긍정","부정","중립")
count <- c(42,17,2328)
result1 <- data.frame(remark=remark,count=count)
result1 <- result1 %>%
  mutate(pct=round(count/sum(count)*100,2)) %>%
  mutate(ylabel=paste0("약",pct,"%")) %>%
  mutate(ypos=cumsum(pct)-0.5*pct)
result1$remark <- factor(result1$remark,levels=rev(as.character(result1$remark)))
result1
  
ggplot(result1,aes(x="",y=pct,fill=remark)) +
  geom_bar(stat="identity", width=1) +  
  geom_text(aes(y=ypos,label=ylabel),color="black") +
  coord_polar(theta="y",start=0) +
  xlab("") + ylab("") +
  ggtitle("육아 관련 네이버 블로그 감성분석") +
  theme(plot.title=element_text(color="black",size=16,face="bold",hjust=0.5))




## Associate analysis
# 출처 : http://blog.naver.com/PostView.nhn?blogId=nonamed0000&logNo=220959156052&categoryNo=24&parentCategoryNo=0&viewDate=&currentPage=1&postListTopCurrentPage=1&from=postView

library(tm)
library(arules) 

result
class(result)

test <- result

test <- str_split(test, "</title>")


str(test)
View(test)
test[[1]][3]
length(test[[1]])

stest <- c()
for(i in 1:102) {
  stest <- c(stest, test[[1]][i])
}
head(stest)

babymom <- Corpus(VectorSource(babymom))
inspect(babymom)

wordtran <- as(babymom, "transactions") # lword에 중복데이터가 있으면 error발생
wordtran 






    
    









