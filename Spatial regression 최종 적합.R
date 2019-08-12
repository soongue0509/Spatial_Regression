#1) Download BurkeyAcademy's R Spatial Regression ZIP file from https://sites.google.com/a/burkeyacademy.com/spatial/home/files, click on R Spatial Regression 1.zip
#2) Extract to a directory
#3) Open RStudio
# I could handle the rest for you, but you need to learn!
#4) Click File, New Project, and create the project inside the directory from 1). Name it what you like.
#5) Click File, New File, R Script. Paste this text document into the window that opens

#If you have not previously downloaded these packages, run these three commands. 
#You can highlight them and click run (top right of this window)
install.packages("spdep")
install.packages("rgdal")
install.packages("rgeos")

#Let's Read in the SHP file (Map + Data in one) using rgdal
#See video https://www.youtube.com/watch?v=FHBDAGc9-8E for how to create this

library(rgdal)
spat.data = readOGR(dsn = ".", layer = "NCVACO")
names(spat.data) #show variable names
summary(spat.data)
#See how some of our quantitative variables are being treated like qualitative (e.g.PCI)
#which is Per Capita Income (R lists frequencies rather than summary stats). 
#This will cause problems. Fix this by overwriting these variables with a numeric version:
spat.data$PCI=as.numeric(levels(spat.data$PCI))[spat.data$PCI]
#Now summary will treat it as a number, calculating a mean. If we don't do this R thinks
#These variables are categorical.
spplot(spat.data,"SALESPC") #make map

#load library spdep, make weights matrix (nb type)
library(spdep)
queen.nb = poly2nb(SPDF) 
rook.nb = poly2nb(SPDF,queen = FALSE) 
queen.listw = nb2listw(queen.nb, zero.policy = TRUE) # , zero.policy = TRUE
rook.listw = nb2listw(rook.nb, zero.policy = TRUE) #convert nb to listw type
listw1= queen.listw

queen.nb = poly2nb(dat_spatial_df)

?poly2nb



#turn off scientific notation for reasonably-sized values
options(scipen = 7)
?options # R이 컴퓨팅하고 그 결과를 보여주는데에 영향을 미치는 파라미터 
#Let's run the Four simplest models: OLS, SLX, Lag Y, and Lag Error
#OLS

dat %>% names

practice <- dat %>% select(1,7,8,15,16,
                           25,26,43,44,
                           69,70,71,72,
                           101,102,
                           103,104
                           )


dat_lm <- practice %>% select(-lat,-lon)
dat_lm <- dat %>% select(-lat,-lon)


reg1 = lm(평균_매출_금액Y ~ . ,data = SPDF)
reg1b=lm(평균_매출_금액Y ~ . -1,data = SPDF)
summary(reg1)
summary(reg1b)

lm.morantest(reg1, listw1) # 공간상관성 존나 씹 심함
lm.LMtests(reg1, listw1, test = c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))

#p=rho, T=theta, and L=lambda
#SLX Spatially Lagged X y=Xß+WXT+e
dat_lm %>% names

reg2.c = lmSLX(평균_매출_금액Y ~ 금요일_매출_비율 + 토요일_매출_비율 +
                         남성_유동인구_수 + 아파트_평균_시가 , data = SPDF, listw = listw1)
?lmSLX


dat_lm %>% dim


summary(reg2)
impacts(reg2,listw = listw1)

summary(impacts(reg2,listw = listw1, R = 500),zstats = TRUE) #Add zstats,pvals; R=500 not needed for SLX


#or just use OLS, doing it "by hand"
#create lagged x's

x1 = model.matrix(reg1) #x values used in OLS regression #1

#create lagged X values, change name prepending "lagx."

lagx1 = create_WX(x1, listw1, prefix = "lagx") 


dat2_lm = cbind(dat_lm,lagx1)
dat2_lm <- dat2_lm %>% select(-204,205)

reg2b=lm(평균_매출_금액Y~., data = dat2_lm)
reg2c=lm(평균_매출_금액Y~.-1, data = dat2_lm)

summary(reg2b) #only difference is in R^2 calculation& F-Stat? Strange! 
summary(reg2c)

#Which is "right"? Doing it by hand give the correct answers- bug in lmSLX.

rsq.reg2=1-sum(reg2$residuals^2)/(var(spat.data$DUI1802)*(length(spat.data$DUI1802)-1))
rsq.reg2b=1-sum(reg2b$residuals^2)/(var(spat.data$DUI1802)*(length(spat.data$DUI1802)-1))
rsq.reg2  
rsq.reg2b
#I confirmed this bug with Roger Bivand and he has fixed the code- 
#it might take a little while to make it into the next spdep update.
#To follow this see https://github.com/r-spatial/spdep/commit/becba8b9f8861421124f6a947390fb4e57b8e0ef


#SAR (Sorry Roger Bivand!) Spatial Lag (Autoregressive) Model y=pWy+XB+e 
reg3=lagsarlm(reg.eq1,data= spat.data, listw1)
summary(reg3)


#Anything with a Lag Y in it: You Cannot interpret Betas as marginal effects.
#Anything with SLX in it: Also need to think about total impact (Direct+Indirect)
#You must use the "impacts" Command:
impacts(reg2,listw=listw1)
summary(impacts(reg2,listw=listw1,R=500),zstats=TRUE) #Add zstats,pvals; R=500 not needed for SLX

impacts(reg3,listw=listw1)
summary(impacts(reg3,listw=listw1,R=500),zstats=TRUE) #Add zstats,pvals
#Caution: These pvalues are simulated, and seem to vary a bit from run to run.

#SEM Spatial Error Model  y=XB+u,   u=LWu+e
reg4=errorsarlm(reg.eq1,data=spat.data, listw1)
summary(reg4)

#Spatial Hausman Test
Hausman.test(reg4)