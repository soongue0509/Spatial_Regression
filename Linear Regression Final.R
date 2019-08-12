getwd()
coffee_sp <- fread("coffee_sp.csv") %>% as_tibble
coffee_sp_tri <- fread("coffee_sp_tri.csv") %>% as_tibble
coffee_sp # 겹치는 좌표들 제외 


library(data.table) #install.packages(data.table)
library(rlang) #install.packages(rlang)
library(dplyr) #install.packages(dplyr)
library(tidyr)

############################
#### 데이터 전처리 하기 ####
############################


### 1 위경도 좌표, 행정동, 시군구 붙여주기 

setwd("C:/Users/soong/Desktop")
location<-fread("상가정보.csv")%>%as_tibble()

location <- location %>% select(상권업종중분류명,도로명,경도,위도,시군구명, 행정동명) %>% 
  separate(도로명, c("si","gu","match"), seq="[[:space:]]") %>% 
  select(상권업종중분류명, match,경도,위도,시군구명, 행정동명)


### commerce 데이터와 결합하기 
commerce <- fread("commerce_201807_final3.csv") %>% as_tibble()
commerce <- commerce %>% select(-lat,-lon)

location 
coffee_location <- location %>% select(-1) 

colnames(coffee_location) <- c("match", "lon", "lat", "시군구", "행정동")

commerce_location <- inner_join(commerce,coffee_location, by = c("name" = "match"))



### So, Here is our final data set . 
### 각 업종별 분석을 진행하려고 하면 이제 이 데이터로 진행하면 된다 

write.csv(commerce_location, "FINAL.csv", row.names = F)

#### Data Importing 

commerce <- fread("FINAL.csv") %>% as_tibble

'이제 이 commerce 데이터로 원하는 업종을 필터링해서 진행하면 된다'
' < 예시 > 
원하는업종이름 <- commerce %>% filter(서비스_업종_코드_명 == "커피음료")
원하는업종이름 %>%  distinct(lon,lat, .keep_all = T) #이거 꼭 진행해 줘야됨 
'
치킨집 <- commerce %>% filter(서비스_업종_코드_명 == "치킨집")
치킨집 <- 치킨집 %>%  distinct(lon,lat, .keep_all = T) #이거 꼭 진행해 줘야됨 
치킨집$평균_매출_금액Y <- log(치킨집$평균_매출_금액Y)


getwd()
setwd("C:/Users/soong/Desktop/TeamData")
write.csv(치킨집, '치킨집.csv',row.names = F)

coffee_sp_tri <- fread("coffee_sp_tri.csv") %>% as_tibble
coffee <- coffee_sp_tri %>% distinct(name, .keep_all = T) 


data_for_lm <- chicken_uniq %>% arrange(lon,lat)
lm_model <- lm(평균_매출_금액Y ~.  , data = data_for_lm[,-c(1,2,105,106,107)])
lm_model <- lm(log(평균_매출_금액Y) ~.  , data = data_for_lm[,-c(1,2,105,106,107)])

summary(lm_model)
par(mfrow = c(2,2))
plot(lm_model)
plot(lm_model2)

y <-data_for_lm$평균_매출_금액Y



### Outlier detection by plotting

summary(y)
par(mfrow = c(1,1))
hist(y, breaks = 50, col = rgb(0,0,1,0.5))
boxplot(y, col = rgb(0,0,1,0.5), main = "Boxplot of y")
shapiro.test(y)
qqnorm(y, main = "Normal QQ Plot - y")
qqline(y, col = "red")

hist(치킨집$평균_매출_금액Y, breaks = 50, col = rgb(0,0,1,0.5))
boxplot(log(y), col = rgb(0,0,1,0.5), main = "Boxplot of log(y)")
shapiro.test(log(y))

boxcox(y)

library(ggpubr)
coffee
ggdensity(y, 
          main = "Test of Normality",
          xlab = "점포당 평균매출 Y")

ggqqplot(y)

ggdensity(치킨집$평균_매출_금액Y, 
          main = "Test of Normality with log",
          xlab = "점포당 평균매출 Y")

ggqqplot(log(y))
?ggqqqplot()

library(MVN)
mvn(data_for_lm[,-c(1,2,105,106,107)], tol = 1e-50)
mvn(data_for_lm[,c(3,27,29)], tol = 1e-50, showOutliers = FALSE)


mvnTest = "hz"
?m


'총 6개의 아웃라이어를 제거하자 ,   310, 527, 804, 423, 812 '
data_for_lm <- data_for_lm[-c(310,423,527,804,812),]
summary(lm_model)
par(mfrow = c(2,2))
plot(lm_model)

### 이외에도 다른 정규성 테스트들 참고
# skewness and kurtosis, they should be around (0,3)
skewness(y)
kurtosis(y)
# Shapiro-Wilks test
shapiro.test(log(y))
# Kolmogorov-Smirnov test
ks.test(y,"pnorm",mean(y),sqrt(var(y)))
# Anderson-Darling test
ad.test(y)



#### Data Importing 

getwd()
setwd()


#### Test Residual's Autocorrelation ####  
snack <- fread("분식집_이상치제거_최종.csv") %>% as_tibble

setwd("C:/Users/soong/Desktop")
commerce <- fread("FINAL.csv") %>% as_tibble

commerce_location <- commerce %>% select(name, lon, lat) %>% distinct(lon,lat, .keep_all = T)
commerce_location

snack_coords <- left_join(snack, commerce_location, by = c("name" = "name")) %>% select(-lon.x, -lat.x)
colnames(snack_coords)[107:108] <- c( "lon", "lat")
snack_coords



residuals <- lm_model$residuals
n <- 996
model_for_AT = lm(residuals[-n] ~ residuals[-1])  # lm model for Autocorrelation Test  
summary(model_for_AT) # coefficients are not significant !   -> It means 
# 하지만 이 테스트는 한 obs의 전후 끼리만 테스트 한 다는 한계가 있다. 
model_for_AT = lm(residuals[-c(n-1,n)] ~ residuals[-c(1,2)])  
summary(model_for_AT) # 두시점 차이도 coefficient  not significant 

model_for_AT = lm(residuals[-c(n-2,n-1,n)] ~ residuals[-c(1,2,3)])  
summary(model_for_AT) # 세시점 차이도 coefficient  not significant 

model_for_AT = lm(residuals[-c(n-3,n-2,n-1,n)] ~ residuals[-c(1,2,3,4)])  
summary(model_for_AT) # 네시점 차이도 coefficient  not significant 

model_for_AT = lm(residuals[-c(n-4,n-3,n-2,n-1,n)] ~ residuals[-c(1,2,3,4,5)])  
summary(model_for_AT) # 다섯시점 차이도 coefficient  not significant 

 


#### 공간 선형 회귀 적합 ####

### 이웃 행렬 만들기 (Weighted Matrix) ### 

count <- snack_coords %>% group_by(name) %>% summarise(count = n()) %>% filter(count >3)
count                   
snack_tri <- inner_join(snack_coords, count, by = c("name" = "name")) 
snack_tri3 <- snack_tri %>% group_by(name) %>% sample_n(3)


snack_tri3 %>% names

library(spdep)

road_names <- snack_tri3$name %>% unique
final = list()
for( i in road_names){
  c1 = filter(snack_tri3[,c(1,107,108)], name == i)
  r1 = rbind(c1,c1[1,])
  P1 = Polygon(r1[,-1]) 
  Ps = Polygons(list(P1), ID = which(road_names == i))
  final[which(road_names == i)] = Ps
}
SPs = SpatialPolygons(final, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) #위경도를 나타내는 지표 
dat_data.frame <- snack_tri3 %>% distinct(name, .keep_all = T)
SPDF = SpatialPolygonsDataFrame(SPs, data = dat_data.frame)

par(mfrow = c(1,1))
plot(SPDF)



coords <- cbind(SPDF$lon, SPDF$lat)
knn_nb1 <- knn2nb(knearneigh(coords, k = 1 ), row.names = SPDF$name )
knn_nb2 <- knn2nb(knearneigh(coords, k = 2 ), row.names = SPDF$name )
knn_nb3 <- knn2nb(knearneigh(coords, k = 3 ), row.names = SPDF$name )
knn_nb4 <- knn2nb(knearneigh(coords, k = 4 ), row.names = SPDF$name )
knn_nb5 <- knn2nb(knearneigh(coords, k = 5 ), row.names = SPDF$name ) # Find near 5 places 
knn_nb8 <- knn2nb(knearneigh(coords, k = 8 ), row.names = SPDF$name ) # Find near 5 places 

knn_nb10 <- knn2nb(knearneigh(coords, k = 10 ), row.names = SPDF$name )
knn_nb15 <- knn2nb(knearneigh(coords, k = 15 ), row.names = SPDF$name )
knn_nb20 <- knn2nb(knearneigh(coords, k = 20 ), row.names = SPDF$name )

spatial_weight <- nb2listw(knn_nb3)
spatial_weight <- nb2listw(knn_nb5)
spatial_weight <- nb2listw(knn_nb10)
spatial_weight <- nb2listw(knn_nb20)



# 공간적으로 지연된 평균 벡터를 구하기 위해 lag.listw 함수를 사용한다
lagged.means <- lag.listw(spatial_weight, dat_data.frame$평균_매출_금액Y)
choropleth(SPDF, lagged.means, shades)
?choropleth
plot(SPDF)
par(mfrow= c(1,1))
plot(log(y), lagged.means, asp = 1, 
     xlim = range(log(y)), ylim = range(log(y)))
abline(a = 0, b = 1, col = 'red')
abline(v = mean(log(y)), lty = 2)
abline(h = mean(lagged.means), lty = 2)

dat_data.frame %>% names
lm_model2 <- lm(평균_매출_금액Y ~.  , data = dat_data.frame[,-c(1,2,105,106,107,108,109)])
lm_model2 %>% summary


lm.morantest(lm_model2,spatial_weight)
lm.LMtests(lm_model2,spatial_weight,test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))



#############################
##### 변수 선택 및 제거 #####
#############################

###(1) Best subset Selection 
data_for_lm 
install.packages("leaps")
library(leaps)


best_subset <- regsubsets(log(평균_매출_금액Y)~.,data = data_for_lm[,-c(1,2,105,106,107,108)],
           nvmax = 20, nbest = 5)

?regsubsets
best_subset %>% summary



###(2) Stepwise selection 
lm_model2 <- lm(평균_매출_금액Y ~.  , data = dat_data.frame[,-c(1,2,105,106,107,108,109)])
stepwise_model <- step(lm_model2, direction = "both")
summary(stepwise_model)

###(3) Lasso Technique  ( shrinkage method )
library(glmnet)


x <- model.matrix(평균_매출_금액Y~., data_for_lm[,-c(1,2,105,106,107,108)])[,-1]
y <- data_for_lm$평균_매출_금액Y


# train & test 를 7:3 으로 나눠서 C-V 진행

set.seed(777)
nrow(x)*0.7 
train = sample(1:nrow(x), size = nrow(x)*0.7)
test = (-train)
ytest = y[test]

cv.lasso <- cv.glmnet(x[train,], y[train], alpha = 1)

lasso.coef = predict(cv.lasso, type = "coefficients", s = cv.lasso$lambda.min) # coefficients
lasso.prediction = predict(cv.lasso, s=cv.lasso$lambda.min, newx = x[test,]) # coefficients

plot(cv.lasso) ## 1 
plot(cv.lasso$glmnet.fit, xvar="lambda", label = TRUE) ## 2
plot(cv.lasso$glmnet.fit, xvar="norm", label = TRUE) ## 3

# lambda가 아래인것들에서 선택하면 됨 두번째플랏에서!
log(cv.lasso$lambda)[20] %>% exp
cv.lasso$lambda.1se %>% log
cv.lasso$lambda.min %>% log

# 계수 수동적으로 불러오는법

small.lambda.index <- which(cv.lasso$lambda == cv.lasso$lambda.min)
small.lambda.betas <- cv.lasso$glmnet.fit$beta[, small.lambda.index]

lasso_model <- glmnet(x= x,y = y, family = "gaussian", alpha = 1, lambda = cv.lasso$lambda.min )
lasso_model$beta # 이를 통해 살아남은 변수 확인 가능 





#### 선택된 변수들로 공간 선형 회귀 적합 ####
eq_stepwise <- log(평균_매출_금액Y) ~ 화요일_매출_비율 + 수요일_매출_비율 + 
  목요일_매출_비율 + 금요일_매출_비율 + `시간대_00~06_매출_비율` + 
  `시간대_06~11_매출_비율` + `시간대_11~14_매출_비율` + `시간대_14~17_매출_비율` + 
  `시간대_17~21_매출_비율` + 남성_매출_비율 + 연령대_30_매출_비율 + 
  연령대_40_매출_비율 + 연령대_50_매출_비율 + 연령대_60_이상_매출_비율 + 
  개업_점포_수 + 남성_유동인구_수 + 여성_유동인구_수 + 시간대_1_유동인구_수 + 
  시간대_2_유동인구_수 + 시간대_4_유동인구_수 + 시간대_6_유동인구_수 + 
  월요일_유동인구_수 + 화요일_유동인구_수 + 수요일_유동인구_수 + 
  목요일_유동인구_수 + 금요일_유동인구_수 + 토요일_유동인구_수 + 
  일요일_유동인구_수 + 집객시설_수 + 종합병원_수 + 극장_수 + 
  지하철_역_수 + 남성_직장_인구_수 + 연령대_10_직장_인구_수 + 
  연령대_40_직장_인구_수 + 여성연령대_20_상주인구_수 + 여성연령대_30_상주인구_수 + 
  여성연령대_60_이상_상주인구_수 + 아파트_면적_66_제곱미터_미만_세대_수 + 
  아파트_가격_1_억_미만_세대_수 + 아파트_평균_면적 + 아파트_평균_시가



### stepwise로 선택된 변수들 가지고 lm을 돌려보자 

summary(stepwise_model)

'lagsarlm : spatial simultaneous autoregressive lag and spatial Durbin models '
'Durbin = T 로하면 lag 모델이 된다. '


lm_model2 <- lm(formula = eq_stepwise, data = data_for_lm[,-c(1,2,105,106,107)] )


lm.morantest(lm_model2,spatial_weight) # P -value가 0.05보다 낮으면 상관성이 있는것으로 파악 
lm.LMtests(lm_model2,spatial_weight,test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))


# stepwise 모델에 있는 equation으로 공간 모형 적합하기 
summary(stepwise_model)

eq_stepwise <- 평균_매출_금액Y ~ 주말_매출_비율 + 오후_매출_비율 + 
  젊은_연령대_매출_비율 + 주말_유동인구_수 + 연령대_20_유동인구_수 + 
  연령대_40_유동인구_수 + 일반_병원_수 + 중학교_수 + 고등학교_수 + 
  슈퍼마켓_수 + 버스_터미널_수 + 지하철_역_수 + 남성_직장_인구_수 + 
  남성연령대_20_상주인구_수 + 남성연령대_30_상주인구_수 + 여성연령대_20_상주인구_수 + 
  여성연령대_30_상주인구_수 + 여성연령대_60_이상_상주인구_수 + 
  아파트_면적_99_제곱미터_세대_수 + 아파트_가격_4_억_세대_수 + 
  아파트_가격_6_억_이상_세대_수 + 아파트_평균_면적 + 아파트_평균_시가 + 
  아파트_면적_165_제곱미터_세대_수


SLX_model  <- lmSLX(formula = eq_stepwise , data = dat_data.frame[,-c(1,2,73,74,75,76,77)],
                        listw = spatial_weight)

summary(SLX_model)
impacts(SLX_model, listw = spatial_weight)
summary(impacts(SLX_model, listw = spatial_weight, R = 500), zstats = TRUE)





SR_model <- lagsarlm(formula = eq_stepwise, data = dat_data.frame[,-c(1,2,73,74,75,76,77)],
                      listw = spatial_weight,
                      tol.solve = 1e-20)
SR_model %>% summary


summary(SR_model)$coefficients %>% exp



Error_model <- errorsarlm(formula = eq_stepwise , data = dat_data.frame[,-c(1,2,73,74,75,76,77)],
                          listw = spatial_weight,
                          tol.solve = 1e-25)
summary(Error_model)


lm_model4 <- lagsarlm(formula = eq_stepwise, data = data_for_lm[,-c(1,2,105,106,107)],
                      listw = spatial_weight, 
                      Durbin = T,
                      tol.solve = 1e-25)
lm_model4 %>% summary

?bptest.sarlm()



CAR_model <- spautolm(formula = eq_stepwise, data = dat_data.frame[,-c(1,2,105,106,107,108,109)],
                      listw = spatial_weight, family = 'CAR',
                      tol.solve = 1e-25) # 왜 실행이 안될까 


#########################################
##### 두번째 모델링 sphet#####
#########################################



lmtest::bptest(lmMod)
car::ncvTest(lmMod)


library(sphet)



