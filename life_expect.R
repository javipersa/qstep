#As no API is available, download files first. 
#Read Polity
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

#Read Capacity/Power from CorrelatesOfWarProject
capacity<-read_csv("Desktop/RD Vars/capacity/capacity/capacity.csv")
View(capacity)

#Adjust/Reshape 
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
 
#Continue Merging
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

#Have Fun
det_of_expect <- read_csv("det_of_expect.csv")
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
plot(gdp_pc_log,polity2)
plot(gdp_log,cinc_red)
#Explore with Relation of Expectancy
plot(gdp_pc_log, expectancy)
plot(polity2, expectancy)
plot(pop_log, expectancy)
plot(cinc_red, expectancy)
text(cinc_red,expectancy,labels=scode,cex= 0.7, pos=3)

#Some Fancier graphs
library(tidyverse)
ggplot(data=det_of_expect, aes(x=gdp_pc_log, y=expectancy))+
  geom_point(alpha = 0.3,  position = position_jitter()) + stat_smooth(method = "lm")

ggplot(data=det_of_expect, aes(x=gdp_pc_log, y=expectancy, colour=scode))+
  geom_point(alpha = 0.3,  position = position_jitter())
#+ stat_smooth(method = "lm")

notwant<-c("IND","RUS","JPN","CHN","USA")
det_of_expect%>%
  filter(!scode %in% notwant)%>%
  ggplot(aes(x=cinc_red, y=expectancy, colour=scode))+
  geom_point()

#Safe File again
write.csv(det_of_expect, file = "det_of_expect.csv")

