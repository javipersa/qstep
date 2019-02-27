################################################
###########BUILDING THE DATASET#################
################################################
################################################
################################################

#As no API is available, download files first. 
#Read Polity - Source <http://www.systemicpeace.org/inscrdata.html>
library(readxl)
polity <- read_excel("Desktop/RD Vars/polity/polity.xls")
View(polity)

#Read GDP from World bank
library(foreign)
gdp<- read_csv("Desktop/RD Vars/gdp world/gdp wb/gdp.csv", skip=3)
View(gdp)

#Read Population from World Bank
world_pop<- read_csv("Desktop/RD Vars/world pop/pop/pop.csv", skip=3)
View(world_pop)

#Read Life Expectancy from World Bank
life_expectancy<- read_csv("Desktop/RD Vars/life expectancy/life expectancy/life_expect.csv", skip=3)
View(life_expectancy)

#Read Gini from World Bank
gini <- read_csv("world_gini.csv")
View(gini)

#Read Capacity/Power from CorrelatesOfWarProject
capacity<-read_csv("Desktop/RD Vars/capacity/capacity/capacity.csv")
View(capacity)

#Adjust/Reshape - wide to long + same variable names
library(reshape2)
#GDP reshape
gdp_long<-melt(gdp)
colnames(gdp_long)[6]<-"year"
colnames(gdp_long)[7]<-"gdp"
#Population Reshape
pop_long<-melt(world_pop)
colnames(pop_long)[6]<-"year"
colnames(pop_long)[7]<-"pop"
#Life Expectancy Reshape
life_long<-melt(life_expectancy)
colnames(life_long)[6]<-"year"
colnames(life_long)[7]<-"pop"
#Gini
gini_long<-melt(gini)
colnames(gini_long)[2]<-"scode"
colnames(gini_long)[22]<-"year"
colnames(gini_long)[23]<-"gini"
write.csv(gini_long, file = "gini_long.csv")
#Adjusted value by extrapolating and interpolating in Stata
gini_long <- read_csv("gini_long.csv")

#Cleaning Polity and Correlates/Capacity
keep<-c("scode", "year", "polity2")
clean_polity<-polity[keep]
keep2<-c("stateabb","year","cinc")
clean_capacity<-capacity[keep2]
colnames(clean_capacity)[1]<-"scode"
clean_polity<-as.data.frame(clean_polity)
clean_capacity<-as.data.frame(clean_capacity)

#Merge Datasets
library("tidyverse")
#Merge GDP with Population
m1 <- gdp_long %>% inner_join(pop_long, by=c("Country Code","year"))
#Merge the previous with Life Expectancy
m2 <- m1 %>% inner_join(life_long, by=c("Country Code","year"))

#Cleaning m2 before merging polity and capacity
names(m2)
cols.dont.want<-c("Indicator Name.x","Indicator Code.x", "X64.x", "Country Name.y" ,"Indicator Name.y","Indicator Code.y","X64.y","Country Name","Indicator Name","Indicator Code","X64")             
m3<-m2[,!names(m2) %in% cols.dont.want, drop=F]
names(m3)
colnames(m3)[1]<-"Country Name"
colnames(m3)[2]<-"scode"
colnames(m3)[4]<-"gdp"
colnames(m3)[5]<-"population"
colnames(m3)[6]<-"expectancy"
 
#Continue Merging Polity and Capacity
m4<- m3 %>% inner_join(clean_polity, by=c("scode","year"))
#Said it was string, need to transform to numeric
m4<-m3 %>%
  mutate_all(type.convert) %>%
  mutate_if(is.factor, as.character) 
#Retry
m5<- m4 %>% inner_join(clean_polity, by=c("scode","year"))
m6<- m5 %>% inner_join(clean_capacity, by=c("scode","year"))
colnames(m6)[1]<-"country"

#Check Structure
str(m6)
#Export as CSV to save , and import again to make sure NA are read as missing. 
write.csv(m6, file = "det_of_expect.csv")

##Last, for the sake of the example, lets add a variable seemingly outside of the scope of interest and see how it relates to the rest
#Meat Consumption per Capita from World Resources Institute <wri.org>
meat <- read_excel("Desktop/meat.xlsx")
View(meat)
#Tranform wide to long.
meat1<-melt(meat, id.vars ="label")
write.csv(meat1, file = "meat.csv")
#Adjusted value by extrapolating and interpolating in Stata
#Read in file again - now as CSV
meat_con<- read_csv("Desktop/meat.csv")
View(meat_con)
#Rename and clean columns
colnames(meat_con)[1]<-"scode"
keep3<-c("scode","year","meat_con_pc")
clean_meat<-meat_con[keep3]
#Merge with master data set
m7<-det_of_expect %>% inner_join(clean_meat, by=c("scode","year"))
#Save again
write.csv(m7, file = "det_of_expect.csv")

#Almost forgot to merge with Gini
m8<-det_of_expect %>% inner_join(gini_long,by=c("scode","year"))
#Save again
write.csv(m8, file = "det_of_expect.csv")


################################################
###########ANALYZING THE DATA##################
################################################
################################################
################################################

#Have Fun
library(readr)
det_of_expect <- read_csv("https://goo.gl/wE9cd1") #It will take a while to load
attach(det_of_expect)
#Population and GDP look/work better if we take the log
det_of_expect$gdp_log<-log(det_of_expect$gdp)
det_of_expect$pop_log<-log(det_of_expect$population)
#Also lets see GDP per capita log
det_of_expect$gdp_pc_log<-log((gdp/population))
#Plus CINC needs to be re-scaled 
det_of_expect$cinc_red<-cinc*100
attach(det_of_expect)

#Lets Plot some graphs
plot(pop_log, gdp_log)
abline(lm(gdp_log~pop_log),col="red")

plot(gdp_pc_log,polity2)
abline(lm(polity2~gdp_pc_log),col="red")

plot(gdp_pc_log,cinc_red)
abline(lm(cinc_red~gdp_pc_log), col="red")

plot(gdp_pc_log,gini)
abline(lm(gini~gdp_pc_log), col="blue")


plot(meat_con_pc, gini)
abline(lm(gini~meat_con_pc),col="red")

#We spot data Gini has been incorrectly intra/extrapolated -boundary values. 
summary(gini)
library("plyr")
library("tidyverse")
det_of_expect <- det_of_expect %>% mutate(gini = replace(gini, gini>100, 100)) #To Change values larger than a 100
det_of_expect <- det_of_expect %>% mutate(gini = replace(gini, gini<0, 0)) #To Change values larger than a 100
attach(det_of_expect)
summary(gini)

#Explore with Relation of Expectancy
plot(gdp_pc_log, expectancy)
abline(lm(expectancy~gdp_pc_log),col="red")

plot(polity2, expectancy)
abline(lm(expectancy~polity2), col="red")

plot(pop_log, expectancy)
abline(lm(expectancy~pop_log),col="red")

plot(gini,expectancy)
abline(lm(expectancy~gini),col="blue")

plot(meat_con_pc,expectancy)
abline(lm(expectancy~meat_con_pc),col="red")

plot(cinc_red,expectancy)
text(cinc_red,expectancy,labels=scode,cex= 0.7, pos=3)
abline(lm(expectancy~cinc_red),col="red")

#Some Fancier graphs
library(tidyverse)
ggplot(data=det_of_expect, aes(x=gdp_pc_log, y=expectancy))+
  geom_point(alpha = 0.3,  position = position_jitter()) + stat_smooth(method = "lm")

ggplot(data=det_of_expect, aes(x=gini, y=expectancy, colour=scode))+
  geom_point(alpha = 0.3,  position = position_jitter())
#+ stat_smooth(method = "lm")

ggplot(data=det_of_expect, aes(x=meat_con_pc,y=gdp_pc_log,colour=scode))+
  geom_point(alpha=0.3, position=position_jitter())

ggplot(data=det_of_expect, aes(x=cinc_red,y=expectancy,colour=scode))+
  geom_point(alpha=0.3, position=position_jitter())

notwant<-c("IND","RUS","JPN","CHN","USA")
det_of_expect%>%
  filter(!scode %in% notwant)%>%
  ggplot(aes(x=cinc_red, y=expectancy, colour=scode))+
  geom_point()

p<-ggplot(det_of_expect,aes(x=cinc_red, y=gdp_pc_log))
mainplot<-p %+% subset(det_of_expect,cinc_red<3)+
  geom_point(alpha=0.3, position=position_jitter())
mainplot


#####Regression Models
model1<- lm(expectancy~gdp_pc_log) #We start with a bivariate regression
summary(model1) #We get a positive, statistically significant coefficient 
#Since we are using log(GDP per capita) the interpretation of the coefficient is a bit tricky. 
#Technically it means that for every unit of log(GDP per capita)  we get around 5.7 more years in life expectancy. 
#The most common way to interpret coefficients is by looking at the standard deviation. 
sd(expectancy, na.rm = TRUE) #==10.9
#Coefficient from model one is =5.7, we can divide them (10.9/5.7= 1.9)
#Hence,"one unit change in GDP per capita (log) produces almost half a standard deviation change in life expectancy "

#Now, the R^2 =0.6744, so this is already explaining around 67% of the variance in expectancy. 

#We need to see if this effects survives controling for other potentially relevant factors. 
#On top of that, we also want to assess whether or not including other variables improves our model fit.

model2<-lm(expectancy~gdp_pc_log+gini)#Add gini
summary(model2)
#As expected, GDP per capita is still positive and significant while gini is negative and significant. 
#Interestingly,the R^2 goes up to 69% which implies, for example, that life expectancy is impacted not 
#only by the stock of wealth, but also, to how it is distributed.
#Try thinking about the interpretation of the gini coefficient. 

model3<-lm(expectancy~gdp_pc_log+gini+polity2) #Add polity score
summary(model3)
#Again, all variables behave in the expected direction. 
#GDP per capita remains positive and significant
#Gini stays negative and significant
#Polity Score is positive and significant. Again this theoretically makes sense. 
#Now, since we are starting to deal with multiple variables, which in addition have varying complex scales, it is better to use the Standard Deviation approach for both dependent and independent variables. 
sd(polity2, na.rm=TRUE)##==7.65
#Now, we know that the Coefficient for Polity is 0.19, and from model 1 we know that the SD of expectancy is 10.9, 
#So for every standard deviation change in Polity (a change of 7.65 points), we get less than a standard deviation change in life expectancy (a change of 1.44 years). 
#Again our R^2 increased to 70%

model4<-lm(expectancy~gdp_pc_log+gini+polity2+cinc_red) #Add Power/Capacity
summary(model4)
#Positive and Significant: GDP per capita, Polity, CINC
#Negative and Significant. GINI 
#The R^2 increased again to 71%

model5<-lm(expectancy~gdp_pc_log+gini+polity2+cinc_red+meat_con_pc) #Add Meat consumption
summary(model5)
#Positive and Significant: GDP per capita, Polity, CINC
#Negative and Significant. GINI 
#Now while meat consumption is still positive and significant, look at the R^2 value, it is 71% roughly the same as last time. 
#Meat consumption then adds very little to the model and we can remove it both on theoretical and empirical grounds. 
#Useful tip - To talks about the relative size of the effects, look up the difference between standardized and non-standardized coefficients. 


#For now lets get our output ready for publication 
#For completeness, when contrasting models, we want to keep the same number of observations in the regression, so I first filter the dataframe to keep all complete observations.
#Rerun model using the update formula for simplicity.
df.complete.obvs<-det_of_expect[complete.cases(det_of_expect),]
detach(det_of_expect)
attach(df.complete.obvs)

mod1<-lm(expectancy~gdp_pc_log, data=df.complete.obvs)
mod2<-update(mod1,.~.+gini)
mod3<-update(mod2,.~.+polity2)
mod4<-update(mod3,.~.+cinc_red)

install.packages("stargazer")
library("stargazer")
stargazer(mod1, mod2, mod3, mod4, type="html",
          title="Regression Results", single.row = FALSE,
           ci.level=0.90, omit.stat = c("f","ser","adj.rsq"),
          dep.var.labels=c("Life Expectancy"),
          covariate.labels=c("GDP per capita (log)","Gini","Polity Score",
                             "State Capacity"), out="regression_models.htm")


stargazer(mod1, mod2, mod3, mod4, type="latex",
          title="Regression Results", single.row = FALSE,
          ci.level=0.90, omit.stat = c("f","ser","adj.rsq"),
          dep.var.labels=c("Life Expectancy"),
          covariate.labels=c("GDP per capita (log)","Gini","Polity Score",
                             "State Capacity"))

##Some regression diagnostics
#Standard plot 
par(mfrow = c(2, 2)) #Get 4 graphs at the same time
plot(mod4)

#Fancy
install.packages("ggfortify")
library(ggfortify)
autoplot(mod4)
#They all show the same information: 

#-Residuals vs Fitted: Checks for linearity
#Normal Q-Q: Checks normality assumption
#Scale-Location: Checks homoskedasticity (equal variance of y for every value of x)//
#Check if variability (variances) of the residual points increases with the value of the fitted outcome variable
#Residuals vs Leverage: Highlight Outliers and Cases with leverage. 

#Safe File again
write.csv(det_of_expect, file = "det_of_expect.csv")

