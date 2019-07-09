## https://developers.naver.com/main/ # 네이버 개발자 계정 홈페이지 

urlStr <- "https://openapi.naver.com/v1/search/blog.xml?" # 기본 url 생성 
searchString <- "query=육아" # 쿼리생성 
searchString <- iconv(searchString, to="UTF-8") # 인코딩 
searchString <- URLencode(searchString)
searchString

etcString <- "&display=100&start=1&sort=sim"

reqUrl <- paste(urlStr, searchString, etcString, sep="")
reqUrl # 요청할 url 생성 

install.packages("httr")
library(httr)

clientid <- "sjGlJYPwbTh7w_HlY4Ie" # 개인 api id 값
clientSecret <- "BY87aFjp4q" # 개인 apu secret 값

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

babymom <- gsub("<(\\/?)(\\w+)*([^<>]*)>", " ", result)
babymom <- gsub("[[:punct:]]", " ", babymom) # 특수문자 제거
babymom <- gsub("[A-z]", " ", babymom) # 모든 영문자 제거
babymom <- gsub("[0-9]", " ", babymom) # 숫자 제거
babymom <- gsub(" +", " ", babymom)
babymom <- gsub("^"," ", babymom)
babymom <- gsub("ㅋ"," ", babymom)
babymom <- gsub("ㅎ"," ", babymom)
babymom <- gsub("ㅜ"," ", babymom)

babymom

babymom <- sapply(babymom,extractNoun,USE.NAMES=F)
babymom <- unlist(babymom)
babymom <- Filter(function(x) {nchar(x)>=2 & nchar(x)<=5}, babymom)
wordcount <- table(babymom)
head(sort(wordcount,decreasing=T),100)
require(wordcloud2)
wordcloud2(wordcount[wordcount>3],size=8,col="random-dark",rotateRatio=0.5,
           backgroundColor="white",shape="diamond")







## sentimental analysis
# 출처 : https://github.com/park1200656/KnuSentiLex (군산대 감성사전)
# site : https://goo-eungs.tistory.com/22 

library(dplyr)
library(stringr)

setwd("D:/Workplace/StatisticsKorea_Big_Data_Project/크롤링")

positive <- readLines("D:/Workplace/StatisticsKorea_Big_Data_Project/크롤링/군산대 감성사전/positive.txt",
                      encoding="UTF-8")
positive <- positive[-1]
negative <- readLines("D:/Workplace/StatisticsKorea_Big_Data_Project/크롤링/군산대 감성사전/negative.txt",
                      encoding="UTF-8")
negative <- negative[-1]

sentimental <- function(sentences,positive,negative) {
  scores = laply(sentences,function(sentence,positive,negative) {
    sentence = gsub('[[:punct:]]','',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)
    
    word.list = str_split(sentence,'\\s+')
    words = unlist(word.list)
    
    pos.matches = match(words,positive)
    neg.matches = match(words,negative)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches)-sum(neg.matches)
    return(score)
  }, positive, negative)

scores.df = data.frame(score=scores,text=sentences)
return(scores.df)
}

result <- sentimental(babymom,positive,negative)
result$color[result$score>=1] <- "blue"
result$color[result$score==0] <- "green"
result$color[result$score<0] <- "red"

table(result$color)

result$remark[result$score>=1] <- "긍정"
result$remark[result$score==0] <- "중립"
result$remark[result$score<0] <- "부정"

sentiment_result <- table(result$remark)

pie(sentimental_result,main="감성분석 결과",col=c("blue","red","green"),radius=0.8)


    
    
    









