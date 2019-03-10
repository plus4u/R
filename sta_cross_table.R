# -- 1)  실습파일 가져오기
getwd()

df_data = read.csv("C:/R/work_r/source/cleanDescriptive.csv", header=TRUE)
df_data                                                               

head(df_data)                                                        

#  
x = df_data$level2                                                   # 리코딩 변수 이용
y = df_data$pass2                                                    # 리코딩 변수 이용
x; y                                                                 # 부모학력수준(x) -> 자녀대학진학여부(y)

# -- 3) 데이터프레임 생성
#       변수 모델링 : 부모학력수준(독립변수:x) -> 자녀진학여부(종속변수:y)
#       형식 : data.frame(명목척도=x, 명목척도=y)
df_result = data.frame(Level=x, Pass=y)                              # 데이터 프레임 생성 - 데이터 묶음
head(df_result ,10)

dim(df_result)                                                       # 차원보기- 248   2


#       교차분할표를 통해서 범주형 변수의 관계를 분석하는 방법

# -- 1) 교차분할표 생성 
table(df_result)                                                     # 빈도보기

# -- 2) package를 이용한 교차분할표 생성
install.packages("gmodels")                                          # gmodels 패키지 설치
library(gmodels)                                                     # CrossTable() 함수 사용

# -- 3) diamond의 cut과 color에 대한 교차분할표 생성
install.packages("ggplot2")                                          # diamonds 데이터 셋 사용을 위한 패키지 설치
library(ggplot2)                                                     # diamonds
head(diamonds)

# -- 4) 크로스테이블 구성
CrossTable(x=diamonds$color, y=diamonds$cut)

# -- 5) 크로스테이블 구성2
#       <조건1> CrossTable()함수 이용
#       <조건2> 변수모델링 : 부모학력수준(x) -> 자녀대학진학여부(y)
str(x)

x = df_data$level2
str(x)
head(x,5)
y = df_data$pass2
df = data.frame(x, y)
CrossTable(x= df$x, y = df$y)
CrossTable(x= df$x, y = df$y, chisq = TRUE) 


# -- 2. 카이제곱 검정 : CrossTable() 이용

# -- 1) 일원카이제곱
# ------------------------------

# -- 적합도/선호도 검정
#    chisq.test() 함수를 이용하여 관찰치와 기대빈도 일치여부 검정

# (1) 적합성 검정 예
#
#     귀무가설 : 기대치와 관찰치는 차이가 없다. : p > 알파
#                예) 도박사의 주사위는 게임에 적합하다.
#     대립가설 : 기대치와 관찰치는 차이가 있다. : p < 알파  
#                예) 도박사의 주사위는 게임에 적합하지 않다.
#
#     가설 설정 방법
#         귀무가설 : 같다 = 다르지않다 = 차이가 없다 = 효과가 없다
#         대립가설 : 같지않다 = 다르다 = 차이가 있다 = 효과가 있다

#     60회 주사위를 던져서 나온 관측도수/기대도수
#     관측도수 : 4, 6, 17,16 ,8,9
#     기대도수 : 10,10,10,10,10,10

chisq.test(c(4,6,17,16,8,9))
#     2) X-squared = 14.2 > 11.071, df = 5(n-1) -> 귀무가설 기각
#     1) p-value = 0.01439 < 0.05 -> 귀무가설 기각

#    <유의확률 해석>
#        유의확률(p-value : 0.01439)이 0.05미만이기 때문에 유의미한 수준(α=0.05)에서 귀무가설을 기각할 수 있다.

#    <검정통계량 해석>
#        검정통계량이 자유도(df)가 5이고, 유의수준이 0.05인 경우 chi-square 분포표에 의하면 임계값이 11.071에 해당된다.
#        그러므로 X-squared 기각값(역)은 χ2 >= 11.071이 된다.

# (2) 선호도 분석
#
#     귀무가설 : 기대치와 관찰치는 차이가 없다.
#                예) 스포츠음료의 선호도에 차이가 없다.
#     대립가설 : 기대치와 관찰치는 차이가 있다.
#                예) 스포츠음료의 선호도에 차이가 있다.
#
df_data22 = textConnection(
    "스포츠음료종류  관측도수
    1   41
    2   30
    3   51
    4   71
    5   61
    ")
x = read.table(df_data22, header=T)
x                                                                    # 스포츠음료종류 관측도수

chisq.test(x$관측도수)                                               # X-squared = 20.4882, df = 4, p-value = 0.0003999

# <유의확률 해석>
#     유의확률(p-value : 0.0003999)이 0.05미만이기 때문에 유의미한 수준(α=0.05)에서 귀무가설을 기각할 수 있다.

# ------------------------------
# -- 2) 이원카이제곱 - 교차분할표 이용
# ------------------------------

# --
# -- (1) 독립성/관련성 검정
# --

# p값이 통상 0.05 보다 작으면 관련성이 있다.
#     동일 집단의 두 변인(학력수준과 대학진과 여부)을 대상으로 관련성이 있는가 없는가?

# 귀무가설 : 부모의 학력수준과 자녀의 대학진학 여부와 관련성이 없다. p >= 0.05
#            두 변인은 독립적이다.
# 대립가설 : 부모의 학력수준과 자녀의 대학진학 여부와 관련성이 있다. p < 0.05
#            두 변인은 독립적이지 않다.

# 독립변수(x)와 종속변수(y) 생성
x = df_data$level2                                                   # 부모의 학력수준
y = df_data$pass2                                                    # 자녀의 대학진학여부

CrossTable(x, y, chisq = TRUE)                                       # p =  0.2507057   
# Pearson's Chi-squared test 
# 2) Chi^2 =  2.766951 < 5.991   d.f. =  2 -> 귀무가설 채택   
# 1) p =  0.2507057 > 0.05 -> 관련성 없음(귀무가설 채택)
chisq.test(x, y)                                                     # X-squared = 2.767, df = 2, p-value = 0.2507

# --
# -- (2) 동질성 검정
# --

# 두 집단의 분포가 동일한가? 다른 분포인가?
# 예) 교육방법에 따른 만족도 : 집단 간 차이가 없다.(동질성 검정)

# -- 1. 파일 가져오기
df_data3 = read.csv("C:\\RProject\\Rwork\\Part-III\\homogenity.csv", header=TRUE)
head(df_data3)

# method와 survery 변수만 서브셋 생성
df_data3 = subset(df_data3, !is.na(survey), c(method, survey))
head(df_data3)

# -- 2. 변수리코딩 - 코딩 변경
#       method: 1:방법1, 2:방법2, 3:방법3
#       survey: 1:매우만족, 2:만족, 3:보통, 4: 불만족, 5: 매우불만족

# 교육방법2 필드 추가
df_data3$method2[df_data3$method==1] = "방법1"
df_data3$method2[df_data3$method==2] = "방법2"
df_data3$method2[df_data3$method==3] = "방법3"

# 만족도2 필드 추가
df_data3$survey2[df_data3$survey==1] = "1.매우만족"
df_data3$survey2[df_data3$survey==2] = "2.만족"
df_data3$survey2[df_data3$survey==3] = "3.보통"
df_data3$survey2[df_data3$survey==4] = "4.불만족"
df_data3$survey2[df_data3$survey==5] = "5.매우불만족"


# -- 3. 교차분할표 작성
table(df_data3$method2, df_data3$survey2)                            # 교차표 생성 -> table(행,열)
# 만족 매우만족 매우불만족 보통 불만족
# 방법1    8        5          6   15     16 -> 50
# 방법2   14        8          6   11     11 -> 50
# 방법3    7        8          9   11     15 -> 50
# 주의 : 반드시 각 집단별 길이(50)가 같아야 한다.

# -- 4. 동질성 검정 - 모수 특성치에 대한 추론검정 
chisq.test(df_data3$method2, df_data3$survey2)                       # Pearson's Chi-squared test
# df_data:  df_data$method2 and df_data$survey2
# X-squared = 6.5447, df = 8, p-value = 0.5865 > 0.05

# -- 5. 동질성 검정 해석
#       교육방법별로 만족도에 차이가 없다고 볼 수 있다.
 