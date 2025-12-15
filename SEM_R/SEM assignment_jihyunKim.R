setwd("C:/Users/mama3/Downloads")
library(dplyr)
library(tidyverse)
library(lavaan)
j23 <- haven::read_sav("[로데이터] 2023 언론인 조사 원본 데이터(SAV).SAV")

j23
mydata <- j23 |> 
  mutate(
    female=ifelse(DQ1==1,0,1),
    SSmajor=ifelse(DQ7_1==1|DQ7_1==2|DQ7_1==3|DQ7_1==4, 1, 0),
    ageyr=DQ2,
    gen=cut(DQ2,
            c(20,29,39,49,59,99),
            include.lowest=TRUE,  # 최소값을 포함? 
            right=TRUE,  # 범위의 닫음값을 포함?
            labels=c("20s","30s","40s","50s","60s+")),
    libcon_own=q32,
    libcon_org=q33,
    satis_job=q24_1,
    satis_org=q24_2,
    satis_task=q24_3,
    GE_HR=q41_1,
    GE_culture=q41_2,
    GE_inviol=q41_3,
    GE_outviol=q41_4,
    indcode=SQ4_1_1
  ) |> select(IDX,female:indcode,
         starts_with("q42_"))  #성평등 제고 개선책: multiple response 
mydata

count(mydata, GE_HR)
count(mydata, GE_culture)
count(mydata, GE_inviol)
count(mydata, GE_outviol)

count(mydata, satis_job)
count(mydata, satis_org)
count(mydata, satis_task)

# 문제 : GE_로 시작하는 변수들과 satis_로 시작하는 변수들을 대상으로 
# 2개 차원을 잠재변수로, female 변수를 포함한 모형을 가정하고 
# female -> GE_ -> satis_ 와 같은 인과모형을(현실에 부합한다고 가정)
# 추정하라. 


model <- "
GE =~ GE_HR+GE_culture+GE_inviol+GE_outviol
SAT =~ satis_job+satis_org+satis_task
GE_HR ~~ GE_culture
GE_inviol ~~ GE_outviol
GE ~ female #외생변수 / 나머지는 내생변수 
SAT ~ female + GE
"
#모형 추정
fit <- sem(model, data=mydata)
summary(fit, fit=T)
fitmeasures(fit) ['gfi']

#모델 핏을 너무 딱딱하게 생각한다. modification으로 어떻게 보완할 수 있을지는 알지만 정당하지는 않다 
#rmsea을 어떻게 볼것인지가 중요하다 
#regressions 해석도 해야함 / 구조도 문제 없다고 생각하고 추정함 


# 문제 : 위의 추정모형에서 매개효과에 대한 통계적 유의도 테스트를 실시하고 그 결과를 해석하라. 

model <- "
GE =~ GE_HR+GE_culture+GE_inviol+GE_outviol
SAT =~ satis_job+satis_org+satis_task
GE_HR ~~ GE_culture
GE_inviol ~~ GE_outviol
GE ~ g1*female #간마 g 
SAT ~ g2*female + b1*GE #베타
IE := g1*b1 #indirect effect parameter

"
#모형 추정
fit <- sem(model, data=mydata)
summary(fit, fit=T)


# 유의미하게 주정됨 

#아래 문제는 gpt가 만지기 어려움. 위에 2개는 가능 
# 문제 : libcon_org 변수를 [0,4]진보적언론사,[5,5]중돟적 언론사,[6,10]보수적 언론사사의 범위에 따라 
# 3개의 하위표본들을 설정하라. 
# 이후, 각 하위표본에 대해 위에서 추정한 매개효과 테스트를 반복하여 
# 실시한 후, 그 결과를 해석하라. 

#cut - 연속형변수를 오디날, if도 능

mydata <- mydata |>
  mutate(
    orggrp = cut(libcon_org,c(0,4,5,10),
                 include.lowest=T,right=T,
                 labels=c('pro','mod','con'))
  )
mydata |> count(libcon_org, orggrp)
mydata |> count(orggrp) #확인도 꼭 해보기 


fit_pro <- fit <- sem(model, data=mydata |> filter(orggrp=='pro'))
fit_mod <- sem(model, data=mydata |> filter(orggrp=='mod'))
fit_con <- sem(model, data=mydata |> filter(orggrp=='con'))

summary(fit_pro,fit=T)
parameterestimates(fit_pro) |> filter(label!='') #is.na 는 결측치 / nothing이 아닌 것 
parameterestimates(fit_mod) |> filter(label!='')
parameterestimates(fit_con) |> filter(label!='')

# indrect impact 를 보기위한 파라미터에서는 진보는 signi . 중도 보수는 네거티브하게sigh 
#우리가 관심있는 파라미터는 IE / indrect
# 각각의 그룹에 대해서 해봐라 GE  ~ female  와 SAT  ~     GE   가 뚜렷하게 나타난다. 

# 표본이 클수록 카이스퀘어 값이 커진다. 다른 조건이 동이하다는 가정하에 보수가 클수밖에 없다. 표본이 크니까. 큰건지 커 보이는건지에 대한 우려점이 있음 . 고려하면서 각각 충분히 커야하고 SAMPEL 사이즈가 동등해야한다 
# 멀티그룹 structual 할때 한 그룹당 200은 넘어야 한다. 최소치가 너무 작으면 안된다. 
#전반적으로 보수적 언론층이 많은 듯 하다 


# 가능하다면, 그리고 활동하는 분과에서 유용할 것으로 판단한다면, 
# 위에서 실시한 분석을 다집단SEM(multi-group SEM)으로 구현해 보라. 
# 경고: 너가 제대로 했으면 넘어가라 - either remove the label(s), or use a vector of labels (one for each group);
model <- "
GE =~ GE_HR+GE_culture+GE_inviol+GE_outviol
SAT =~ satis_job+satis_org+satis_task
GE_HR ~~ GE_culture
GE_inviol ~~ GE_outviol
GE ~ g1*female #간마 g 
SAT ~ g2*female + b1*GE #베타
IE := g1*b1 #indirect effect parameter

"
#모형 추정
fit <- sem(model, data=mydata, group='orggrp') #3개 그룹을 백터형식으로 나눠야함 
summary(fit, fit=T)

###

#내가 보고싶은 변수 순서대로 
mydata <- mydata |> arrange(orggrp)

model <- "
GE =~ GE_HR+GE_culture+GE_inviol+GE_outviol
SAT =~ satis_job+satis_org+satis_task
GE_HR ~~ GE_culture
GE_inviol ~~ GE_outviol
GE ~ c(g1p,g1m,g1c)*female #간마 g 
SAT ~ c(g2p,g2m,g2c)*female + c(b1p,b1m,b1c)*GE #베타
IEp := g1p*b1p #indirect effect parameter for progressives
IEm := g1m*b1m #indirect effect parameter for moderates
IEc := g1c*b1c #indirect effect parameter for conservatives
"
#모형 추정
fit <- sem(model, data=mydata, group='orggrp') #3개 그룹을 백터형식으로 나눠야함 
summary(fit, fit=T)

#con                                         73.315 모형이 보수가 가장 많이 차지하고 있구나. 표본 크기가 크구나 
#pro                                         32.531
#mod                                         49.491 

#Defined Parameters: 규칙이 있음을 알 수 있다. 보수로 갈수록 네거티브로 커짐
#  Estimate  Std.Err  z-value  P(>|z|)
#IEp              -0.136    0.089   -1.521    0.128
#IEm              -0.279    0.075   -3.695    0.000
#IEc              -0.463    0.096   -4.832    0.000

#간마 베타를 통합해서 regression 해준다 

#1) group.equal 옵션을 활용 
# 같지않다고해서 카이제곱이 얼마나 이동했는지 테스트 하는 것 
model <- "
GE =~ GE_HR+GE_culture+GE_inviol+GE_outviol
SAT =~ satis_job+satis_org+satis_task
GE_HR ~~ GE_culture
GE_inviol ~~ GE_outviol
GE ~ female #+ ageyr #(연령통제변수까지 추가)#외생변수 / 나머지는 내생변수 
SAT ~ female + GE
"
#모형 추정
fit_m1 <- sem(model, data=mydata, group='orggrp',
           group.equal=c('regressions'))
summary(fit_m1)
anova(fit, fit_m1)
# 각그룹이 유의미하게 다르다고 보기 어렵다 

#2) parameter generation : 파라미터를 새로 만드는 방법 - 이 방법을 선호  
model <- "
GE =~ GE_HR+GE_culture+GE_inviol+GE_outviol
SAT =~ satis_job+satis_org+satis_task
GE_HR ~~ GE_culture
GE_inviol ~~ GE_outviol
GE ~ c(g1p,g1m,g1c)*female #간마 g 
SAT ~ c(g2p,g2m,g2c)*female + c(b1p,b1m,b1c)*GE #베타
IEp := g1p*b1p #indirect effect parameter for progressives
IEm := g1m*b1m #indirect effect parameter for moderates
IEc := g1c*b1c #indirect effect parameter for conservatives
#세집단이니까 3쌍 
D_IEpm := IEp - IEm
D_IEpc := IEp - IEc
D_IEmc := IEm - IEc

#실험데이터 potable 귀무가설H0 : M > P+c/2
CL_IE := IEm - 0.5*(IEp+IEc)
"

fit_m2 <- sem(model, data=mydata, group='orggrp')
summary(fit_m2)

# 경고메시지(들):
#anova(fit, fit_m2) #파라미터를 내부에서 생성해서 테스트해서 anvoa test는 할 수 없다. 서로 다른 집단이 아니라 내부에서 변수를 만든거라서 

#정당에 따라 젠더 이퀄리티에 따른 만족도가 달라진다. 진보와 보수간에 경향신문과 시일보


##3)scenario-based 
# 우리는 measurement가 같다고 보고 가정해서 돌린 것이다. 근데 그 가정이 틀릴수도 있음 
# 위에와 아래를 하이브리드로 섞은 것 . 

model <- "
GE =~ GE_HR+GE_culture+GE_inviol+GE_outviol
SAT =~ satis_job+satis_org+satis_task
GE_HR ~~ GE_culture
GE_inviol ~~ GE_outviol
GE ~ c(g1p,g1m,g1c)*female #간마 g 
SAT ~ c(g2p,g2m,g2c)*female + c(b1p,b1m,b1c)*GE #베타
IEp := g1p*b1p #indirect effect parameter for progressives
IEm := g1m*b1m #indirect effect parameter for moderates
IEc := g1c*b1c #indirect effect parameter for conservatives
#세집단이니까 3쌍 
D_IEpm := IEp - IEm
D_IEpc := IEp - IEc
D_IEmc := IEm - IEc

#실험데이터 potable 귀무가설H0 : M > P+c/2
CL_IE := IEm - 0.5*(IEp+IEc)
"
#모든 집단이 같다고 시나리오를 쓰는 것 
fit_m3 <- sem(model, data=mydata, group='orggrp',
              group.equal=c('loadings'))
summary(fit_m3)
anova(fit, fit_m3)

#미디에이션 효과가 각 집단마다 어떻게 나타났는지. 어떻게 앞에서는 미디에이션 임팩트와 관련된것만 봤는데 이번에는 관련 없다고 보았을 떄 
#이론이 w정당화가 가장 중요함. 너가 왜 이런 방법을 썼느지. 
#리뷰어가 공격할 수 있음. 아니면 그냥 전통에 기반하든가 - 내용찾기가 일이겠지만 
# 이론적 정당화가 가장 필요할거다. 2번이 가장 추천. 3번은 방어할 떄. 


#########################연속형변수 해결법 - 주요변수만 본다 
model <- "
GE =~ GE_HR+GE_culture+GE_inviol+GE_outviol
SAT =~ satis_job+satis_org+satis_task
IDEO =~ libcon_org 
GE_HR ~~ GE_culture
GE_inviol ~~ GE_outviol
GE ~ female + IDEO 
SAT ~ female + GE + IDEO 
"
#모형 추정
fit <- sem(model, data=mydata)
summary(fit)

#모형이 복잡해짐 
mydata |>
  mutate(across(
    .cols=c(libcon_org,GE_HR,GE_culture,GE_inviol,GE_outviol),
    .fns=function(x){x-mean(x)}
  )) |>
  mutate(
    ideo_hr=libcon_org*GE_HR,
    ideo_cult=libcon_org*GE_culture,
    ideo_inv=libcon_org*GE_inviol,
    ideo_outv=libcon_org*GE_outviol,
  )

model <- "
GE =~ GE_HR+GE_culture+GE_inviol+GE_outviol
SAT =~ satis_job+satis_org+satis_task
IDEO =~ libcon_org 
I_GE =~ ideo_hr+ideo_cult+ideo_inv+ideo_outv
GE_HR ~~ GE_culture
GE_inviol ~~ GE_outviol
GE ~ female + IDEO + ideo_fem
SAT ~ female + GE + IDEO + I_GE
"
fit <- sem(model, mydata)
summary(fit) #I_GE interatiion 그래프를 넣은 것 
