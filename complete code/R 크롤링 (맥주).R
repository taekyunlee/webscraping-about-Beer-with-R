# 맥주 비교 크롤링
# 맥주자체
query = '맥주'
query = iconv(query, to = 'UTF-8', toRaw = T)
query = paste0('%', paste(unlist(query), collapse = '%'))
query = toupper(query)

if(!require(httr)){install.packages("httr"); library(httr)}

end_num = 1000
display_num = 100
start_point = seq(1,end_num,display_num)
# seq(1,10,2)
final_dat = NULL
for(i in 1:length(start_point))
{
  url = paste0('https://openapi.naver.com/v1/search/blog.xml?query=',query,
               '&display=',display_num,'&start=',start_point[i],'&sort=sim')
  url_body = read_xml(GET(url, header), encoding = "UTF-8")
  
  title = url_body %>% xml_nodes('item title') %>% xml_text()
  bloggername = url_body %>% xml_nodes('item bloggername') %>% xml_text()
  postdate = url_body %>% xml_nodes('postdate') %>% xml_text()
  link = url_body %>% xml_nodes('item link') %>% xml_text()
  description = url_body %>% xml_nodes('item description') %>% html_text()
  
  temp_dat = cbind(title, bloggername, postdate, link, description)
  final_dat = rbind(final_dat, temp_dat)
  cat(i, '\n')
}

final_dat = data.frame(final_dat, stringAsFactors = F)

final_dat$title          # 블로그 제목
final_dat$bloggername    # 블로거 이름
final_dat$postdate       # 게시일 
final_dat$link           # 링크
str(final_dat$description)    # 설명

final_dat$description = gsub('\n|\t|<.*?>|&quot;',' ',final_dat$description)
final_dat$description = gsub('[^가-힣a-zA-Z]',' ',final_dat$description)
final_dat$description = gsub(' +',' ',final_dat$description)

final_dat$title = gsub('\n|\t|<.*?>|&quot;',' ',final_dat$title)
final_dat$title = gsub('[^가-힣a-zA-Z]',' ',final_dat$title)
final_dat$title = gsub(' +',' ',final_dat$title)

final_dat$description
final_dat$title

library(KoNLP)
nouns1=KoNLP::extractNoun(final_dat$description)
nouns2=KoNLP::extractNoun(final_dat$title)

wordframe = data.frame(table(c(unlist(nouns1),unlist(nouns2))))

wordata <- wordframe[order(wordframe$Freq,decreasing = TRUE ),] 

# 빈도수 체크해서 순서대로 나열 완료
# 쓸데없는 의미를 가진단어와 검색단어 없애기 (불용어 처리) 

noneed =as.character(wordata$Var1)[nchar(as.character(wordata$Var1)) == 1] # 한글자 목록
noneed        
length(need)
need = c('맛','잔','병','집','술','캔','밤','비','낮','향','입','속','컵','산',
         '강','삶','값','밥','회','길','몸','발','돈','빵','탕','땀','꽃','춤','팁','힘')
noneed1 = setdiff(noneed,need)

#as.character(wordata$Var1) %in% noneed1  #wordata중에서 필요없는곳에 
#들어있는 애들이 있냐?
#k =as.character(wordata$Var1)[as.character(wordata$Var1) %in% noneed1]
#wordata$Var1 == c('카스','맥주')
#wordata$Var1 %in% noneed1
wordata <-wordata[(!wordata$Var1 %in% noneed1),  ]  
wordata <-wordata[(!wordata$Var1 %in% c('맥주','ml','lt','ver','gt','CM','amp')),  ] # 가장 빈번한 검색값과 쓸데없는 영어 문자 지우기

# 막대그래프
library(ggplot2)
ggplot(wordata[c(1:20), ], aes(x = wordata[c(1:20), ]$Var1, y = wordata[c(1:20), ]$Freq)) +
  geom_col(fill = "lightblue", colour = "black") +
  scale_x_discrete(limits = wordata[c(1:20), ]$Var1) + xlab("단어 빈도수 1워 ~ 20위") + ylab("단어 빈도수") +
  ggtitle('맥주 1위 ~ 20위 연관단어 출현 빈도수')+  theme(axis.text.x = element_text(size=18),
                                             axis.title.x = element_text(size = 10))

#워드 클라우드
library(wordcloud)

palete <- brewer.pal(9, "Set1")  # 색상

wordata               
wordcloud(wordata$Var1 , freq= wordata$Freq, scale=c(3, 1),
          rot.per=0.3 , min.freq=20, random.order=F, random.color=T, colors=palete)

###############################################################################
library(rvest)

client_id = 'T0WxI3_oV9E3h4J9sEUK';
client_secret = '6ROytPyC6q';
header = httr::add_headers(
  'X-Naver-Client-Id' = client_id,
  'X-Naver-Client-Secret' = client_secret)

query = '맥주 카스'
query = iconv(query, to = 'UTF-8', toRaw = T)
query = paste0('%', paste(unlist(query), collapse = '%'))
query = toupper(query)

if(!require(httr)){install.packages("httr"); library(httr)}

end_num = 1000
display_num = 100
start_point = seq(1,end_num,display_num)
# seq(1,10,2)
final_dat = NULL
for(i in 1:length(start_point))
{
  url = paste0('https://openapi.naver.com/v1/search/blog.xml?query=',query,
               '&display=',display_num,'&start=',start_point[i],'&sort=sim')
  url_body = read_xml(GET(url, header), encoding = "UTF-8")
  
  title = url_body %>% xml_nodes('item title') %>% xml_text()
  bloggername = url_body %>% xml_nodes('item bloggername') %>% xml_text()
  postdate = url_body %>% xml_nodes('postdate') %>% xml_text()
  link = url_body %>% xml_nodes('item link') %>% xml_text()
  description = url_body %>% xml_nodes('item description') %>% html_text()
  
  temp_dat = cbind(title, bloggername, postdate, link, description)
  final_dat = rbind(final_dat, temp_dat)
  cat(i, '\n')
}

final_dat = data.frame(final_dat, stringAsFactors = F)

final_dat$title          # 블로그 제목
final_dat$bloggername    # 블로거 이름
final_dat$postdate       # 게시일 
final_dat$link           # 링크
final_dat$description    # 설명

final_dat$description = gsub('\n|\t|<.*?>|&quot;',' ',final_dat$description)
final_dat$description = gsub('[^가-힣a-zA-Z]',' ',final_dat$description)
final_dat$description = gsub(' +',' ',final_dat$description)

final_dat$title = gsub('\n|\t|<.*?>|&quot;',' ',final_dat$title)
final_dat$title = gsub('[^가-힣a-zA-Z]',' ',final_dat$title)
final_dat$title = gsub(' +',' ',final_dat$title)

final_dat$description
final_dat$title

library(KoNLP)
nouns1=KoNLP::extractNoun(final_dat$description)
nouns2=KoNLP::extractNoun(final_dat$title)
 
wordframe = data.frame(table(c(unlist(nouns1),unlist(nouns2))))

wordata <- wordframe[order(wordframe$Freq,decreasing = TRUE ),] 

# 빈도수 체크해서 순서대로 나열 완료
# 쓸데없는 의미를 가진단어와 검색단어 없애기 (불용어 처리) 

noneed =as.character(wordata$Var1)[nchar(as.character(wordata$Var1)) == 1] # 한글자 목록
noneed        
length(need)
need = c('맛','잔','병','집','술','캔','밤','비','낮','향','입','속','컵','산',
         '강','삶','값','밥','회','길','몸','발','돈','빵','탕','땀','꽃','춤','팁','힘')
noneed1 = setdiff(noneed,need)

#as.character(wordata$Var1) %in% noneed1  #wordata중에서 필요없는곳에 
                                         #들어있는 애들이 있냐?
#k =as.character(wordata$Var1)[as.character(wordata$Var1) %in% noneed1]
#wordata$Var1 == c('카스','맥주')
#wordata$Var1 %in% noneed1
wordata <-wordata[(!wordata$Var1 %in% noneed1),  ]  
wordata <-wordata[(!wordata$Var1 %in% c('맥주','카스','ml','lt','ver','gt','CM','amp')),  ] # 가장 빈번한 검색값과 쓸데없는 영어 문자 지우기

# 막대그래프
library(ggplot2)
ggplot(wordata[c(1:20), ], aes(x = wordata[c(1:20), ]$Var1, y = wordata[c(1:20), ]$Freq)) +
  geom_col(fill = "lightblue", colour = "black") +
  scale_x_discrete(limits = wordata[c(1:20), ]$Var1) + xlab("단어 빈도수 1워 ~ 20위") + ylab("단어 빈도수") +
  ggtitle('맥주 카스(cass)  1위 ~ 20위 연관단어 출현 빈도수') + theme(axis.text.x = element_text(size=16),
                                                     axis.title.x = element_text(size = 10))

#워드 클라우드
library(wordcloud)

palete <- brewer.pal(9, "Set1")  # 색상

wordata               
wordcloud(wordata$Var1 , freq= wordata$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=25, random.order=F, random.color=T, colors=palete)

#####################################################################
#2




query = '맥주 기네스'
query = iconv(query, to = 'UTF-8', toRaw = T)
query = paste0('%', paste(unlist(query), collapse = '%'))
query = toupper(query)

if(!require(httr)){install.packages("httr"); library(httr)}

end_num = 1000
display_num = 100
start_point = seq(1,end_num,display_num)

final_dat = NULL
for(i in 1:length(start_point))
{
  url = paste0('https://openapi.naver.com/v1/search/blog.xml?query=',query,'&display=',display_num,'&start=',start_point[i],'&sort=sim')
  url_body = read_xml(GET(url, header), encoding = "UTF-8")
  
  title = url_body %>% xml_nodes('item title') %>% xml_text()
  bloggername = url_body %>% xml_nodes('item bloggername') %>% xml_text()
  postdate = url_body %>% xml_nodes('postdate') %>% xml_text()
  link = url_body %>% xml_nodes('item link') %>% xml_text()
  description = url_body %>% xml_nodes('item description') %>% html_text()
  
  temp_dat = cbind(title, bloggername, postdate, link, description)
  final_dat = rbind(final_dat, temp_dat)
  cat(i, '\n')
}
final_dat = data.frame(final_dat, stringAsFactors = F)

final_dat$title          # 블로그 제목
final_dat$bloggername    # 블로거 이름
final_dat$postdate       # 게시일 
final_dat$link           # 링크
final_dat$description    # 설명

final_dat$description = gsub('\n|\t|<.*?>|&quot;',' ',final_dat$description)
final_dat$description = gsub('[^가-힣a-zA-Z]',' ',final_dat$description)
final_dat$description = gsub(' +',' ',final_dat$description)
final_dat$description

library(KoNLP)
nouns1=KoNLP::extractNoun(final_dat$description)
nouns2=KoNLP::extractNoun(final_dat$title)

wordframe = data.frame(table(c(unlist(nouns1),unlist(nouns2))))

wordata <- wordframe[order(wordframe$Freq,decreasing = TRUE ),] 

# 빈도수 체크해서 순서대로 나열 완료
# 쓸데없는 의미를 가진단어와 검색단어 없애기 (불용어 처리) 

noneed =as.character(wordata$Var1)[nchar(as.character(wordata$Var1)) == 1] # 한글자 목록
noneed        
length(need)
need = c('맛','잔','병','집','술','캔','밤','비','낮','향','입','속','컵','산',
         '강','삶','값','밥','회','길','몸','발','돈','빵','탕','땀','꽃','춤','팁','힘')
noneed1 = setdiff(noneed,need)

#as.character(wordata$Var1) %in% noneed1  #wordata중에서 필요없는곳에 
#들어있는 애들이 있냐?
#k =as.character(wordata$Var1)[as.character(wordata$Var1) %in% noneed1]
#wordata$Var1 == c('카스','맥주')
#wordata$Var1 %in% noneed1
wordata <-wordata[(!wordata$Var1 %in% noneed1),  ]  
wordata <-wordata[(!wordata$Var1 %in% c('맥주','기네스','ml','lt','ver','gt','CM','amp')),  ] # 가장 빈번한 검색값과 쓸데없는 영어 문자 지우기

# 막대그래프
library(ggplot2)
ggplot(wordata[c(1:20), ], aes(x = wordata[c(1:20), ]$Var1, y = wordata[c(1:20), ]$Freq)) +
  geom_col(fill = "lightblue", colour = "black") +
  scale_x_discrete(limits = wordata[c(1:20), ]$Var1) + xlab("단어 빈도수 1워 ~ 20위") + ylab("단어 빈도수") +
  ggtitle('맥주 기네스  1위 ~ 20위 연관단어 출현 빈도수')+ theme(axis.text.x = element_text(size=13),
                                                 axis.title.x = element_text(size = 10))

#워드 클라우드
library(wordcloud)

palete <- brewer.pal(9, "Set1")  # 색상

wordata               
wordcloud(wordata$Var1 , freq= wordata$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=20, random.order=F, random.color=T, colors=palete)
###########################################################################
#3




query = '맥주 파울라너'
query = iconv(query, to = 'UTF-8', toRaw = T)
query = paste0('%', paste(unlist(query), collapse = '%'))
query = toupper(query)

if(!require(httr)){install.packages("httr"); library(httr)}

end_num = 1000
display_num = 100
start_point = seq(1,end_num,display_num)

final_dat = NULL
for(i in 1:length(start_point))
{
  url = paste0('https://openapi.naver.com/v1/search/blog.xml?query=',query,'&display=',display_num,'&start=',start_point[i],'&sort=sim')
  url_body = read_xml(GET(url, header), encoding = "UTF-8")
  
  title = url_body %>% xml_nodes('item title') %>% xml_text()
  bloggername = url_body %>% xml_nodes('item bloggername') %>% xml_text()
  postdate = url_body %>% xml_nodes('postdate') %>% xml_text()
  link = url_body %>% xml_nodes('item link') %>% xml_text()
  description = url_body %>% xml_nodes('item description') %>% html_text()
  
  temp_dat = cbind(title, bloggername, postdate, link, description)
  final_dat = rbind(final_dat, temp_dat)
  cat(i, '\n')
}
final_dat = data.frame(final_dat, stringAsFactors = F)

final_dat$title          # 블로그 제목
final_dat$bloggername    # 블로거 이름
final_dat$postdate       # 게시일 
final_dat$link           # 링크
final_dat$description    # 설명

final_dat$description = gsub('\n|\t|<.*?>|&quot;',' ',final_dat$description)
final_dat$description = gsub('[^가-힣a-zA-Z]',' ',final_dat$description)
final_dat$description = gsub(' +',' ',final_dat$description)
final_dat$description

library(KoNLP)
nouns1=KoNLP::extractNoun(final_dat$description)
nouns2=KoNLP::extractNoun(final_dat$title)

wordframe = data.frame(table(c(unlist(nouns1),unlist(nouns2))))

wordata <- wordframe[order(wordframe$Freq,decreasing = TRUE ),] 

# 빈도수 체크해서 순서대로 나열 완료
# 쓸데없는 의미를 가진단어와 검색단어 없애기 (불용어 처리) 

noneed =as.character(wordata$Var1)[nchar(as.character(wordata$Var1)) == 1] # 한글자 목록
noneed        
length(need)
need = c('맛','잔','병','집','술','캔','밤','비','낮','향','입','속','컵','산',
         '강','삶','값','밥','회','길','몸','발','돈','빵','탕','땀','꽃','춤','팁','힘')
noneed1 = setdiff(noneed,need)

#as.character(wordata$Var1) %in% noneed1  #wordata중에서 필요없는곳에 
#들어있는 애들이 있냐?
#k =as.character(wordata$Var1)[as.character(wordata$Var1) %in% noneed1]
#wordata$Var1 == c('카스','맥주')
#wordata$Var1 %in% noneed1
wordata <-wordata[(!wordata$Var1 %in% noneed1),  ]  
wordata <-wordata[(!wordata$Var1 %in% c('맥주','파울라너','ml','lt','ver','gt','CM','amp')),  ] # 가장 빈번한 검색값과 쓸데없는 영어 문자 지우기

# 막대그래프
library(ggplot2)
ggplot(wordata[c(1:20), ], aes(x = wordata[c(1:20), ]$Var1, y = wordata[c(1:20), ]$Freq)) +
  geom_col(fill = "lightblue", colour = "black") +
  scale_x_discrete(limits = wordata[c(1:20), ]$Var1) + xlab("단어 빈도수 1워 ~ 20위") + ylab("단어 빈도수") +
  ggtitle('맥주 파울라너  1위 ~ 20위 연관단어 출현 빈도수')

#워드 클라우드
library(wordcloud)

palete <- brewer.pal(9, "Set1")  # 색상

wordata               
wordcloud(wordata$Var1 , freq= wordata$Freq, scale=c(3, 0.8),
          rot.per=0.1 , min.freq=15, random.order=F, random.color=T, colors=palete)
############################################################################

query = '맥주 하이트'
query = iconv(query, to = 'UTF-8', toRaw = T)
query = paste0('%', paste(unlist(query), collapse = '%'))
query = toupper(query)

if(!require(httr)){install.packages("httr"); library(httr)}

end_num = 1000
display_num = 100
start_point = seq(1,end_num,display_num)

final_dat = NULL
for(i in 1:length(start_point))
{
  url = paste0('https://openapi.naver.com/v1/search/blog.xml?query=',query,'&display=',display_num,'&start=',start_point[i],'&sort=sim')
  url_body = read_xml(GET(url, header), encoding = "UTF-8")
  
  title = url_body %>% xml_nodes('item title') %>% xml_text()
  bloggername = url_body %>% xml_nodes('item bloggername') %>% xml_text()
  postdate = url_body %>% xml_nodes('postdate') %>% xml_text()
  link = url_body %>% xml_nodes('item link') %>% xml_text()
  description = url_body %>% xml_nodes('item description') %>% html_text()
  
  temp_dat = cbind(title, bloggername, postdate, link, description)
  final_dat = rbind(final_dat, temp_dat)
  cat(i, '\n')
}
final_dat = data.frame(final_dat, stringAsFactors = F)

final_dat$title          # 블로그 제목
final_dat$bloggername    # 블로거 이름
final_dat$postdate       # 게시일 
final_dat$link           # 링크
final_dat$description    # 설명

final_dat$description = gsub('\n|\t|<.*?>|&quot;',' ',final_dat$description)
final_dat$description = gsub('[^가-힣a-zA-Z]',' ',final_dat$description)
final_dat$description = gsub(' +',' ',final_dat$description)
final_dat$description

library(KoNLP)
nouns1=KoNLP::extractNoun(final_dat$description)
nouns2=KoNLP::extractNoun(final_dat$title)

wordframe = data.frame(table(c(unlist(nouns1),unlist(nouns2))))

wordata <- wordframe[order(wordframe$Freq,decreasing = TRUE ),] 

# 빈도수 체크해서 순서대로 나열 완료
# 쓸데없는 의미를 가진단어와 검색단어 없애기 (불용어 처리) 

noneed =as.character(wordata$Var1)[nchar(as.character(wordata$Var1)) == 1] # 한글자 목록
noneed        
length(need)
need = c('맛','잔','병','집','술','캔','밤','비','낮','향','입','속','컵','산',
         '강','삶','값','밥','회','길','몸','발','돈','빵','탕','땀','꽃','춤','팁','힘')
noneed1 = setdiff(noneed,need)

#as.character(wordata$Var1) %in% noneed1  #wordata중에서 필요없는곳에 
#들어있는 애들이 있냐?
#k =as.character(wordata$Var1)[as.character(wordata$Var1) %in% noneed1]
#wordata$Var1 == c('카스','맥주')
#wordata$Var1 %in% noneed1
wordata <-wordata[(!wordata$Var1 %in% noneed1),  ]  
wordata <-wordata[(!wordata$Var1 %in% c('맥주','하이트','ml','lt','ver','gt','CM','amp')),  ] # 가장 빈번한 검색값과 쓸데없는 영어 문자 지우기

# 막대그래프
library(ggplot2)
ggplot(wordata[c(1:20), ], aes(x = wordata[c(1:20), ]$Var1, y = wordata[c(1:20), ]$Freq)) +
  geom_col(fill = "lightblue", colour = "black") +
  scale_x_discrete(limits = wordata[c(1:20), ]$Var1) + xlab("단어 빈도수 1워 ~ 20위") + ylab("단어 빈도수") +
  ggtitle('맥주 하이트  1위 ~ 20위 연관단어 출현 빈도수') + theme(axis.text.x = element_text(size=13),
                                                  axis.title.x = element_text(size = 10))

#워드 클라우드
library(wordcloud)

palete <- brewer.pal(9, "Set1")  # 색상

wordata               
wordcloud(wordata$Var1 , freq= wordata$Freq, scale=c(3, 0.8),
          rot.per=0.1 , min.freq=15, random.order=F, random.color=T, colors=palete)

####################################################################################


query = '맥주 블랑'
query = iconv(query, to = 'UTF-8', toRaw = T)
query = paste0('%', paste(unlist(query), collapse = '%'))
query = toupper(query)

if(!require(httr)){install.packages("httr"); library(httr)}

end_num = 1000
display_num = 100
start_point = seq(1,end_num,display_num)

final_dat = NULL
for(i in 1:length(start_point))
{
  url = paste0('https://openapi.naver.com/v1/search/blog.xml?query=',query,'&display=',display_num,'&start=',start_point[i],'&sort=sim')
  url_body = read_xml(GET(url, header), encoding = "UTF-8")
  
  title = url_body %>% xml_nodes('item title') %>% xml_text()
  bloggername = url_body %>% xml_nodes('item bloggername') %>% xml_text()
  postdate = url_body %>% xml_nodes('postdate') %>% xml_text()
  link = url_body %>% xml_nodes('item link') %>% xml_text()
  description = url_body %>% xml_nodes('item description') %>% html_text()
  
  temp_dat = cbind(title, bloggername, postdate, link, description)
  final_dat = rbind(final_dat, temp_dat)
  cat(i, '\n')
}
final_dat = data.frame(final_dat, stringAsFactors = F)

final_dat$title          # 블로그 제목
final_dat$bloggername    # 블로거 이름
final_dat$postdate       # 게시일 
final_dat$link           # 링크
final_dat$description    # 설명

final_dat$description = gsub('\n|\t|<.*?>|&quot;',' ',final_dat$description)
final_dat$description = gsub('[^가-힣a-zA-Z]',' ',final_dat$description)
final_dat$description = gsub(' +',' ',final_dat$description)
final_dat$description

library(KoNLP)
nouns1=KoNLP::extractNoun(final_dat$description)
nouns2=KoNLP::extractNoun(final_dat$title)

wordframe = data.frame(table(c(unlist(nouns1),unlist(nouns2))))

wordata <- wordframe[order(wordframe$Freq,decreasing = TRUE ),] 

# 빈도수 체크해서 순서대로 나열 완료
# 쓸데없는 의미를 가진단어와 검색단어 없애기 (불용어 처리) 

noneed =as.character(wordata$Var1)[nchar(as.character(wordata$Var1)) == 1] # 한글자 목록
noneed        
length(need)
need = c('맛','잔','병','집','술','캔','밤','비','낮','향','입','속','컵','산',
         '강','삶','값','밥','회','길','몸','발','돈','빵','탕','땀','꽃','춤','팁','힘')
noneed1 = setdiff(noneed,need)

#as.character(wordata$Var1) %in% noneed1  #wordata중에서 필요없는곳에 
#들어있는 애들이 있냐?
#k =as.character(wordata$Var1)[as.character(wordata$Var1) %in% noneed1]
#wordata$Var1 == c('카스','맥주')
#wordata$Var1 %in% noneed1
wordata <-wordata[(!wordata$Var1 %in% noneed1),  ]  
wordata <-wordata[(!wordata$Var1 %in% c('맥주','블랑','ml','lt','ver','gt','CM','amp')),  ] # 가장 빈번한 검색값과 쓸데없는 영어 문자 지우기

# 막대그래프
library(ggplot2)
ggplot(wordata[c(1:20), ], aes(x = wordata[c(1:20), ]$Var1, y = wordata[c(1:20), ]$Freq)) +
  geom_col(fill = "lightblue", colour = "black") +
  scale_x_discrete(limits = wordata[c(1:20), ]$Var1) + xlab("단어 빈도수 1워 ~ 20위") + ylab("단어 빈도수") +
  ggtitle('맥주 블랑  1위 ~ 20위 연관단어 출현 빈도수') + theme(axis.text.x = element_text(size=13),
                                                 axis.title.x = element_text(size = 10))

#워드 클라우드
library(wordcloud)

palete <- brewer.pal(9, "Set1")  # 색상

wordata               
wordcloud(wordata$Var1 , freq= wordata$Freq, scale=c(3, 0.8),
          rot.per=0.1 , min.freq=15, random.order=F, random.color=T, colors=palete)


###################################################################################

query = '맥주 칭따오'
query = iconv(query, to = 'UTF-8', toRaw = T)
query = paste0('%', paste(unlist(query), collapse = '%'))
query = toupper(query)

if(!require(httr)){install.packages("httr"); library(httr)}

end_num = 1000
display_num = 100
start_point = seq(1,end_num,display_num)

final_dat = NULL
for(i in 1:length(start_point))
{
  url = paste0('https://openapi.naver.com/v1/search/blog.xml?query=',query,'&display=',display_num,'&start=',start_point[i],'&sort=sim')
  url_body = read_xml(GET(url, header), encoding = "UTF-8")
  
  title = url_body %>% xml_nodes('item title') %>% xml_text()
  bloggername = url_body %>% xml_nodes('item bloggername') %>% xml_text()
  postdate = url_body %>% xml_nodes('postdate') %>% xml_text()
  link = url_body %>% xml_nodes('item link') %>% xml_text()
  description = url_body %>% xml_nodes('item description') %>% html_text()
  
  temp_dat = cbind(title, bloggername, postdate, link, description)
  final_dat = rbind(final_dat, temp_dat)
  cat(i, '\n')
}
final_dat = data.frame(final_dat, stringAsFactors = F)

final_dat$title          # 블로그 제목
final_dat$bloggername    # 블로거 이름
final_dat$postdate       # 게시일 
final_dat$link           # 링크
final_dat$description    # 설명

final_dat$description = gsub('\n|\t|<.*?>|&quot;',' ',final_dat$description)
final_dat$description = gsub('[^가-힣a-zA-Z]',' ',final_dat$description)
final_dat$description = gsub(' +',' ',final_dat$description)
final_dat$description

library(KoNLP)
nouns1=KoNLP::extractNoun(final_dat$description)
nouns2=KoNLP::extractNoun(final_dat$title)

wordframe = data.frame(table(c(unlist(nouns1),unlist(nouns2))))

wordata <- wordframe[order(wordframe$Freq,decreasing = TRUE ),] 

# 빈도수 체크해서 순서대로 나열 완료
# 쓸데없는 의미를 가진단어와 검색단어 없애기 (불용어 처리) 

noneed =as.character(wordata$Var1)[nchar(as.character(wordata$Var1)) == 1] # 한글자 목록
noneed        
length(need)
need = c('맛','잔','병','집','술','캔','밤','비','낮','향','입','속','컵','산',
         '강','삶','값','밥','회','길','몸','발','돈','빵','탕','땀','꽃','춤','팁','힘')
noneed1 = setdiff(noneed,need)

#as.character(wordata$Var1) %in% noneed1  #wordata중에서 필요없는곳에 
#들어있는 애들이 있냐?
#k =as.character(wordata$Var1)[as.character(wordata$Var1) %in% noneed1]
#wordata$Var1 == c('카스','맥주')
#wordata$Var1 %in% noneed1
wordata <-wordata[(!wordata$Var1 %in% noneed1),  ]  
wordata <-wordata[(!wordata$Var1 %in% c('맥주','칭따오','ml','lt','ver','gt','CM','amp')),  ] # 가장 빈번한 검색값과 쓸데없는 영어 문자 지우기

# 막대그래프
library(ggplot2)
ggplot(wordata[c(1:20), ], aes(x = wordata[c(1:20), ]$Var1, y = wordata[c(1:20), ]$Freq)) +
  geom_col(fill = "lightblue", colour = "black") +
  scale_x_discrete(limits = wordata[c(1:20), ]$Var1) + xlab("단어 빈도수 1워 ~ 20위") + ylab("단어 빈도수") +
  ggtitle('맥주 칭따오  1위 ~ 20위 연관단어 출현 빈도수')

#워드 클라우드
library(wordcloud)

palete <- brewer.pal(9, "Set1")  # 색상

wordata               
wordcloud(wordata$Var1 , freq= wordata$Freq, scale=c(3, 0.8),
          rot.per=0.1 , min.freq=15, random.order=F, random.color=T, colors=palete)

####################################################################################

query = '맥주 클라우드'
query = iconv(query, to = 'UTF-8', toRaw = T)
query = paste0('%', paste(unlist(query), collapse = '%'))
query = toupper(query)

if(!require(httr)){install.packages("httr"); library(httr)}

end_num = 1000
display_num = 100
start_point = seq(1,end_num,display_num)

final_dat = NULL
for(i in 1:length(start_point))
{
  url = paste0('https://openapi.naver.com/v1/search/blog.xml?query=',query,'&display=',display_num,'&start=',start_point[i],'&sort=sim')
  url_body = read_xml(GET(url, header), encoding = "UTF-8")
  
  title = url_body %>% xml_nodes('item title') %>% xml_text()
  bloggername = url_body %>% xml_nodes('item bloggername') %>% xml_text()
  postdate = url_body %>% xml_nodes('postdate') %>% xml_text()
  link = url_body %>% xml_nodes('item link') %>% xml_text()
  description = url_body %>% xml_nodes('item description') %>% html_text()
  
  temp_dat = cbind(title, bloggername, postdate, link, description)
  final_dat = rbind(final_dat, temp_dat)
  cat(i, '\n')
}
final_dat = data.frame(final_dat, stringAsFactors = F)

final_dat$title          # 블로그 제목
final_dat$bloggername    # 블로거 이름
final_dat$postdate       # 게시일 
final_dat$link           # 링크
final_dat$description    # 설명

final_dat$description = gsub('\n|\t|<.*?>|&quot;',' ',final_dat$description)
final_dat$description = gsub('[^가-힣a-zA-Z]',' ',final_dat$description)
final_dat$description = gsub(' +',' ',final_dat$description)
final_dat$description

library(KoNLP)
nouns1=KoNLP::extractNoun(final_dat$description)
nouns2=KoNLP::extractNoun(final_dat$title)

wordframe = data.frame(table(c(unlist(nouns1),unlist(nouns2))))

wordata <- wordframe[order(wordframe$Freq,decreasing = TRUE ),] 

# 빈도수 체크해서 순서대로 나열 완료
# 쓸데없는 의미를 가진단어와 검색단어 없애기 (불용어 처리) 

noneed =as.character(wordata$Var1)[nchar(as.character(wordata$Var1)) == 1] # 한글자 목록
noneed        
length(need)
need = c('맛','잔','병','집','술','캔','밤','비','낮','향','입','속','컵','산',
         '강','삶','값','밥','회','길','몸','발','돈','빵','탕','땀','꽃','춤','팁','힘')
noneed1 = setdiff(noneed,need)

#as.character(wordata$Var1) %in% noneed1  #wordata중에서 필요없는곳에 
#들어있는 애들이 있냐?
#k =as.character(wordata$Var1)[as.character(wordata$Var1) %in% noneed1]
#wordata$Var1 == c('카스','맥주')
#wordata$Var1 %in% noneed1
wordata <-wordata[(!wordata$Var1 %in% noneed1),  ]  
wordata <-wordata[(!wordata$Var1 %in% c('맥주','클라우드','ml','lt','ver','gt','CM','amp')),  ] # 가장 빈번한 검색값과 쓸데없는 영어 문자 지우기

# 막대그래프
library(ggplot2)
ggplot(wordata[c(1:20), ], aes(x = wordata[c(1:20), ]$Var1, y = wordata[c(1:20), ]$Freq)) +
  geom_col(fill = "lightblue", colour = "black") +
  scale_x_discrete(limits = wordata[c(1:20), ]$Var1) + xlab("단어 빈도수 1워 ~ 20위") + ylab("단어 빈도수") +
  ggtitle('클라우드 칭따오  1위 ~ 20위 연관단어 출현 빈도수')

#워드 클라우드
library(wordcloud)

palete <- brewer.pal(9, "Set1")  # 색상

wordata               
wordcloud(wordata$Var1 , freq= wordata$Freq, scale=c(3, 0.8),
          rot.per=0.1 , min.freq=15, random.order=F, random.color=T, colors=palete)


#############################################################################
a = unlist(strsplit(final_dat$description,' '))
table(a)

# 각각의 맥주 특징에 대한 표현 빈도수 
# 특징 사전
beerfeature = read.csv('C:/Users/renz/Desktop/RJ/맥주/맥주특징.csv' ,header = F)
beerfeature = as.vector(beerfeature$V1)

###############################################################################
# 카스
cass<- data.frame(table(a[a %in% beerfeature]))
cass <- cass[order(cass$Freq,decreasing = TRUE ),] 

wordcloud(cass$Var1 , freq= cass$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=2, random.order=F, random.color=T, colors=palete)
###############################################################################
# 기네스
guiness<- data.frame(table(a[a %in% beerfeature]))
guiness <- guiness[order(guiness$Freq,decreasing = TRUE ),] 

wordcloud(guiness$Var1 , freq= guiness$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=2, random.order=F, random.color=T, colors=palete)
################################################################################
# 파울라너
paulaner<- data.frame(table(a[a %in% beerfeature]))
paulaner <- paulaner[order(paulaner$Freq,decreasing = TRUE ),] 

wordcloud(paulaner$Var1 , freq= paulaner$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=2, random.order=F, random.color=T, colors=palete)
################################################################################
# 하이트
hite<- data.frame(table(a[a %in% beerfeature]))
hite <- hite[order(hite$Freq,decreasing = TRUE ),] 

wordcloud(hite$Var1 , freq= hite$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=1, random.order=F, random.color=T, colors=palete)
###################################################################################
# 블랑
blank<- data.frame(table(a[a %in% beerfeature]))
blank <- blank[order(blank$Freq,decreasing = TRUE ),] 

wordcloud(blank$Var1 , freq= blank$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=2, random.order=F, random.color=T, colors=palete)
##################################################################################
# 칭따오
ching<- data.frame(table(a[a %in% beerfeature]))
ching <- ching[order(ching$Freq,decreasing = TRUE ),] 

wordcloud(ching$Var1 , freq= ching$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=2, random.order=F, random.color=T, colors=palete)
##################################################################################
# 클라우드
cloud<- data.frame(table(a[a %in% beerfeature]))
cloud <- cloud[order(cloud$Freq,decreasing = TRUE ),] 

wordcloud(cloud$Var1 , freq= cloud$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=2, random.order=F, random.color=T, colors=palete)
##################################################################################

# 감성사전 (긍정,부정)  -> 워드클라우드(긍,부) 긍정단어의 비율, 부정 단어의 비율

a = unlist(strsplit(final_dat$description,' '))
table(a)

pos = read.csv('C:/Users/renz/Desktop/RJ/맥주/긍정사전.csv' ,header = F)
pos = as.vector(pos$V1)
neg = read.csv('C:/Users/renz/Desktop/RJ/맥주/부정사전.csv' ,header = F)
neg = as.vector(neg$V1)
##################################################################################
# cass
pcass<- data.frame(table(a[a %in% pos]))
pcass <- pcass[order(pcass$Freq,decreasing = TRUE ),] 

wordcloud(pcass$Var1 , freq= pcass$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=2, random.order=F, random.color=T, colors=palete)

ncass<- data.frame(table(a[a %in% neg]))
ncass <- ncass[order(ncass$Freq,decreasing = TRUE ),] 

wordcloud(ncass$Var1 , freq= ncass$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=1, random.order=F, random.color=T, colors=palete)

sum(pcass$Freq)
sum(ncass$Freq)

sum(pcass$Freq)/(sum(pcass$Freq)+sum(ncass$Freq)) # 긍정단어가 차지하는 비율
1-sum(pcass$Freq)/(sum(pcass$Freq)+sum(ncass$Freq)) # 부정단어가 차지하는 비율

slices = c(sum(pcass$Freq)/(sum(pcass$Freq)+sum(ncass$Freq)),1-sum(pcass$Freq)/(sum(pcass$Freq)+sum(ncass$Freq)))
lbls = c('긍정','부정')
lbls <- paste(lbls,c(round((sum(pcass$Freq)/(sum(pcass$Freq)+sum(ncass$Freq)))*100,1)  ,round((1-sum(pcass$Freq)/(sum(pcass$Freq)+sum(ncass$Freq)))*100,1) ,"%",sep="")) 
pie(slices, labels = lbls, main="Pie Chart of sentiment")
##################################################################################
# 기네스
pguiness<- data.frame(table(a[a %in% pos]))
pguiness <- pguiness[order(pguiness$Freq,decreasing = TRUE ),] 

wordcloud(pguiness$Var1 , freq= pguiness$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=2, random.order=F, random.color=T, colors=palete)

nguiness<- data.frame(table(a[a %in% neg]))
nguiness <- nguiness[order(nguiness$Freq,decreasing = TRUE ),] 

wordcloud(nguiness$Var1 , freq= nguiness$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=1, random.order=F, random.color=T, colors=palete)

sum(pguiness$Freq)
sum(nguiness$Freq)

sum(pguiness$Freq)/(sum(pguiness$Freq)+sum(nguiness$Freq)) # 긍정단어가 차지하는 비율
1-sum(pguiness$Freq)/(sum(pguiness$Freq)+sum(nguiness$Freq)) # 부정단어가 차지하는 비율

slices = c(sum(pguiness$Freq)/(sum(pguiness$Freq)+sum(nguiness$Freq)),1-sum(pguiness$Freq)/(sum(pguiness$Freq)+sum(nguiness$Freq)))
lbls = c('긍정','부정')
lbls <- paste(lbls,c(round((sum(pguiness$Freq)/(sum(pguiness$Freq)+sum(nguiness$Freq)))*100,1)  ,round((1-sum(pguiness$Freq)/(sum(pguiness$Freq)+sum(nguiness$Freq)))*100,1) ,"%",sep="")) 
pie(slices, labels = lbls, main="Pie Chart of sentiment")

####################################################################################
# 파울라너 
ppaulaner<- data.frame(table(a[a %in% pos]))
ppaulaner <- ppaulaner[order(ppaulaner$Freq,decreasing = TRUE ),] 

wordcloud(ppaulaner$Var1 , freq= ppaulaner$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=2, random.order=F, random.color=T, colors=palete)

npaulaner<- data.frame(table(a[a %in% neg]))
npaulaner <- npaulaner[order(npaulaner$Freq,decreasing = TRUE ),] 

wordcloud(npaulaner$Var1 , freq= npaulaner$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=1, random.order=F, random.color=T, colors=palete)

sum(ppaulaner$Freq)
sum(npaulaner$Freq)

sum(ppaulaner$Freq)/(sum(ppaulaner$Freq)+sum(npaulaner$Freq)) # 긍정단어가 차지하는 비율
1-sum(pguiness$Freq)/(sum(ppaulaner$Freq)+sum(npaulaner$Freq)) # 부정단어가 차지하는 비율

slices = c(sum(ppaulaner$Freq)/(sum(ppaulaner$Freq)+sum(npaulaner$Freq)),1-sum(ppaulaner$Freq)/(sum(ppaulaner$Freq)+sum(npaulaner$Freq)))
lbls = c('긍정','부정')
lbls <- paste(lbls,c(round((sum(ppaulaner$Freq)/(sum(ppaulaner$Freq)+sum(npaulaner$Freq)))*100,1)  ,round((1-sum(ppaulaner$Freq)/(sum(ppaulaner$Freq)+sum(npaulaner$Freq)))*100,1) ,"%",sep="")) 
pie(slices, labels = lbls, main="Pie Chart of sentiment")
#######################################################################################
# 하이트
phite<- data.frame(table(a[a %in% pos]))
phite <- phite[order(phite$Freq,decreasing = TRUE ),] 

wordcloud(phite$Var1 , freq= phite$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=2, random.order=F, random.color=T, colors=palete)

nhite<- data.frame(table(a[a %in% neg]))
nhite <- nhite[order(nhite$Freq,decreasing = TRUE ),] 

wordcloud(nhite$Var1 , freq= nhite$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=1, random.order=F, random.color=T, colors=palete)

sum(phite$Freq)
sum(nhite$Freq)

sum(phite$Freq)/(sum(phite$Freq)+sum(nhite$Freq)) # 긍정단어가 차지하는 비율
1-sum(phite$Freq)/(sum(phite$Freq)+sum(nhite$Freq)) # 부정단어가 차지하는 비율


slices = c(sum(phite$Freq)/(sum(phite$Freq)+sum(nhite$Freq)),1-sum(phite$Freq)/(sum(phite$Freq)+sum(nhite$Freq)))
lbls = c('긍정','부정')
lbls <- paste(lbls,c(round((sum(phite$Freq)/(sum(phite$Freq)+sum(nhite$Freq)))*100,1)  ,round((1-sum(phite$Freq)/(sum(phite$Freq)+sum(nhite$Freq)))*100,1) ,"%",sep="")) 
pie(slices, labels = lbls, main="Pie Chart of sentiment")
####################################################################################
# 블랑
pblank<- data.frame(table(a[a %in% pos]))
pblank <- pblank[order(pblank$Freq,decreasing = TRUE ),] 

wordcloud(pblank$Var1 , freq= pblank$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=3, random.order=F, random.color=T, colors=palete)

nblank<- data.frame(table(a[a %in% neg]))
nblank <- nblank[order(nblank$Freq,decreasing = TRUE ),] 

wordcloud(nblank$Var1 , freq= nblank$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=1, random.order=F, random.color=T, colors=palete)

sum(pblank$Freq)
sum(nblank$Freq)

sum(pblank$Freq)/(sum(pblank$Freq)+sum(nblank$Freq)) # 긍정단어가 차지하는 비율
1-sum(pblank$Freq)/(sum(pblank$Freq)+sum(nblank$Freq)) # 부정단어가 차지하는 비율


slices = c(sum(pblank$Freq)/(sum(pblank$Freq)+sum(nblank$Freq)),1-sum(pblank$Freq)/(sum(pblank$Freq)+sum(nblank$Freq)))
lbls = c('긍정','부정')
lbls <- paste(lbls,c(round((sum(pblank$Freq)/(sum(pblank$Freq)+sum(nblank$Freq)))*100,1)  ,round((1-sum(pblank$Freq)/(sum(pblank$Freq)+sum(nblank$Freq)))*100,1) ,"%",sep="")) 
pie(slices, labels = lbls, main="Pie Chart of sentiment")

#####################################################################################
# 칭따오
pching<- data.frame(table(a[a %in% pos]))
pching <- pching[order(pching$Freq,decreasing = TRUE ),] 

wordcloud(pching$Var1 , freq= pching$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=2, random.order=F, random.color=T, colors=palete)

nching<- data.frame(table(a[a %in% neg]))
nching <- nching[order(nching$Freq,decreasing = TRUE ),] 

wordcloud(nching$Var1 , freq= nching$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=1, random.order=F, random.color=T, colors=palete)

sum(pching$Freq)
sum(nching$Freq)

sum(pching$Freq)/(sum(pching$Freq)+sum(nching$Freq)) # 긍정단어가 차지하는 비율
1-sum(pching$Freq)/(sum(pching$Freq)+sum(nching$Freq)) # 부정단어가 차지하는 비율


slices = c(sum(pching$Freq)/(sum(pching$Freq)+sum(nching$Freq)),1-sum(pching$Freq)/(sum(pching$Freq)+sum(nching$Freq)))
lbls = c('긍정','부정')
lbls <- paste(lbls,c(round((sum(pching$Freq)/(sum(pching$Freq)+sum(nching$Freq)))*100,1)  ,round((1-sum(pching$Freq)/(sum(pching$Freq)+sum(nching$Freq)))*100,1) ,"%",sep="")) 
pie(slices, labels = lbls, main="Pie Chart of sentiment")

####################################################################################
# 클라우드
pcloud<- data.frame(table(a[a %in% pos]))
pcloud <- pcloud[order(pcloud$Freq,decreasing = TRUE ),] 

wordcloud(pcloud$Var1 , freq= pcloud$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=2, random.order=F, random.color=T, colors=palete)

ncloud<- data.frame(table(a[a %in% neg]))
ncloud <- ncloud[order(ncloud$Freq,decreasing = TRUE ),] 

wordcloud(ncloud$Var1 , freq= ncloud$Freq, scale=c(3, 1),
          rot.per=0.1 , min.freq=1, random.order=F, random.color=T, colors=palete)

sum(pcloud$Freq)
sum(ncloud$Freq)

sum(pcloud$Freq)/(sum(pcloud$Freq)+sum(ncloud$Freq)) # 긍정단어가 차지하는 비율
1-sum(pcloud$Freq)/(sum(pcloud$Freq)+sum(ncloud$Freq)) # 부정단어가 차지하는 비율


slices = c(sum(pcloud$Freq)/(sum(pcloud$Freq)+sum(ncloud$Freq)),1-sum(pcloud$Freq)/(sum(pcloud$Freq)+sum(ncloud$Freq)))
lbls = c('긍정','부정')
lbls <- paste(lbls,c(round((sum(pcloud$Freq)/(sum(pcloud$Freq)+sum(ncloud$Freq)))*100,1)  ,round((1-sum(pcloud$Freq)/(sum(pcloud$Freq)+sum(ncloud$Freq)))*100,1) ,"%",sep="")) 
pie(slices, labels = lbls, main="Pie Chart of sentiment")

####################################################################################
# 동시 출현 단어
install.packages("tm")
install.packages("qgraph")
library(tm)
library(qgraph)
final_dat$description
final_dat$title

localeToCharset()

sentence <- c(final_dat$description,final_dat$title)
write.csv(sentence,'sentences.csv')

sen = read.csv('sentences.csv')
str(sen)
sen = as.vector(sen$x)
corp <- Corpus(VectorSource(sen))
tdm <- TermDocumentMatrix(corp)
m<- as.matrix(tdm)
rownames(m)<-iconv(rownames(m),'UTF-8','CP949')
word.count <- rowSums(m)  
word.order <- order(word.count, decreasing=T)
freq.words <- m[word.order[1:20], ]
co.matrix <- freq.words %*% t(freq.words) 

qgraph(co.matrix,
      labels=rownames(co.matrix),   
      diag=F,                       
       layout='spring',           
      edge.color='blue',
       vsize=log(diag(co.matrix))*2)




