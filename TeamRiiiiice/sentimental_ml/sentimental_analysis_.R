#21.08.12
# naver_blog_검색어키워드_감성분석 + 군산대감성어사전이용

#00.파일 위치 설정
getwd()
setwd('C://Workspace//sentimental')

#01.필요 패키지 설치 및 로드
install.packages('plyr')
library(plyr)
library(stringr)

#02.감성분석할 리뷰데이터 가져오기
#txt<-readLines("recipe.txt",encoding = "UTF-8") # 레시피파일
#txt<-readLines("egn_review.txt",encoding = "UTF-8") #전저리리뷰파일

txt<-readLines("egn_2015_blog_ssal.txt",encoding = "UTF-8")  #전처리_2015_blog_ssal


#03. 사용자 한국어 감성사전 정의, 불러오기
#긍정어 사전
positive <- readLines("positive.txt", encoding = "UTF-8")
positive=positive[-1]
#부정어 사전
negative <- readLines("negative.txt", encoding = "UTF-8")
negative=negative[-1]

#04. 함수정의
sentimental = function(sentences, positive, negative){
  
  scores = laply(sentences, function(sentence, positive, negative) {
    
    sentence = gsub('[[:punct:]]', '', sentence) # 문장부호 제거
    sentence = gsub('[[:cntrl:]]', '', sentence) # 특수문자 제거
    sentence = gsub('\\d+', '', sentence)        # 숫자 제거
    
    word.list = str_split(sentence, '\\s+')      # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개 이상)
    words = unlist(word.list)                    # unlist() : list를 vector 객체로 구조변경
    
    pos.matches = match(words, positive)         # words의 단어를 positive에서 matching
    neg.matches = match(words, negative)         # words의 단어를 negative에서 matching
    
    pos.matches = !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)  # 긍정 - 부정   
    return(score)
  }, positive, negative)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

result=sentimental(txt, positive, negative)

result$color[result$score >=1] = "blue"  #긍정 파란색
result$color[result$score ==0] = "green" #중립 초록색
result$color[result$score < 0] = "red"   #부정 빨간색

#05. 테이블확인
table(result$color)

result$remark[result$score >=1] = "긍정"
result$remark[result$score ==0] = "중립"
result$remark[result$score < 0] = "부정"

# 테이블확인
sentiment_result= table(result$remark)
sentiment_result

#06. 파이그래프
pie(sentiment_result, main="감성분석 결과", col=c("blue","red","green"), radius=0.8)

#07. 텍스트 내용확인
# 긍정이라고 인식한 것 
txt[(result$score >=1) == TRUE]
# 중립이라고 인식한 것
txt[(result$score == 0) == TRUE]
# 부정이라고 인식한 것
txt[(result$score < 0) == TRUE]
