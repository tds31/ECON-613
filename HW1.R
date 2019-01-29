setwd("~/Desktop/class stuff/SY/spring/AEM/HW1")
stu <- read.csv("datstu.csv", header = TRUE)
jss <- read.csv("datjss.csv", header = TRUE)
sss <- read.csv("datsss.csv", header = TRUE)
library(dplyr)

#Note: I've only ever used STATA and MATLAB last spring, in ECON208 and ECON623 (I'm a Fuqua student). I anticipate that seeing how to do all of this correctly will go a long way for me.

#1
n_distinct(stu$X)
schools <- unlist(stu[5:10])
length(unique(schools))
programs <- unlist(stu[11:16])
length(unique(programs))
choice1 <- paste(stu$schoolcode1,"-",stu$choicepgm1)
choice2 <- paste(stu$schoolcode2,"-",stu$choicepgm2)
choice3 <- paste(stu$schoolcode3,"-",stu$choicepgm3)
choice4 <- paste(stu$schoolcode4,"-",stu$choicepgm4)
choice5 <- paste(stu$schoolcode5,"-",stu$choicepgm5)
choice6 <- paste(stu$schoolcode6,"-",stu$choicepgm6)
choice <- data.frame(choice1,choice2,choice3,choice4,choice5,choice6)
choicelist <- unlist(choice[1:6])
length(unique(choicelist))
sum(is.na(stu$score))
stu$difschools <- apply(stu[5:10], 1, function(x) length(unique(na.omit(x))))
stu$numschools <- apply(stu[5:10], 1, function(x) length(na.omit(x)))
stu$dupcheck <- stu$numschools - stu$difschools
sum(stu$dupcheck > 0)
stu$na_count <- apply(stu[5:10], 1, function(x) sum(is.na(x)))
sum(stu$na_count >= 1)

#2
stu$school1 = sss$schoolname[match(stu$schoolcode1,sss$schoolcode)]
stu$school2 = sss$schoolname[match(stu$schoolcode2,sss$schoolcode)]
stu$school3 = sss$schoolname[match(stu$schoolcode3,sss$schoolcode)]
stu$school4 = sss$schoolname[match(stu$schoolcode4,sss$schoolcode)]
stu$school5 = sss$schoolname[match(stu$schoolcode5,sss$schoolcode)]
stu$school6 = sss$schoolname[match(stu$schoolcode6,sss$schoolcode)]
stu$district1 = sss$sssdistrict[match(stu$schoolcode1,sss$schoolcode)]
stu$district2 = sss$sssdistrict[match(stu$schoolcode2,sss$schoolcode)]
stu$district3 = sss$sssdistrict[match(stu$schoolcode3,sss$schoolcode)]
stu$district4 = sss$sssdistrict[match(stu$schoolcode4,sss$schoolcode)]
stu$district5 = sss$sssdistrict[match(stu$schoolcode5,sss$schoolcode)]
stu$district6 = sss$sssdistrict[match(stu$schoolcode6,sss$schoolcode)]
stu$choice1 = paste(stu$schoolcode1,"-",stu$choicepgm1)
stu$choice2 = paste(stu$schoolcode2,"-",stu$choicepgm2)
stu$choice3 = paste(stu$schoolcode3,"-",stu$choicepgm3)
stu$choice4 = paste(stu$schoolcode4,"-",stu$choicepgm4)
stu$choice5 = paste(stu$schoolcode5,"-",stu$choicepgm5)
stu$choice6 = paste(stu$schoolcode6,"-",stu$choicepgm6)
stu$admit = ifelse(stu$rankplace==1,stu$choice1,ifelse(stu$rankplace==2,stu$choice2,ifelse(stu$rankplace==3,stu$choice3,ifelse(stu$rankplace==4,stu$choice4,ifelse(stu$rankplace==5,stu$choice5,ifelse(stu$rankplace==6,stu$choice6,"NA"))))))
stu$admitdistrict = ifelse(stu$rankplace==1,as.character(stu$district1),ifelse(stu$rankplace==2,as.character(stu$district2),ifelse(stu$rankplace==3,as.character(stu$district3),ifelse(stu$rankplace==4,as.character(stu$district4),ifelse(stu$rankplace==5,as.character(stu$district5),ifelse(stu$rankplace==6,as.character(stu$district6),"NA"))))))
stu$admitlat = sss$ssslat[match(stu$admitdistrict,sss$sssdistrict)]
stu$admitlong = sss$ssslong[match(stu$admitdistrict,sss$sssdistrict)]
choice1 <- data.frame(stu$schoolcode1,choice1)
choice2 <- data.frame(stu$schoolcode2,choice2)
choice3 <- data.frame(stu$schoolcode3,choice3)
choice4 <- data.frame(stu$schoolcode4,choice4)
choice5 <- data.frame(stu$schoolcode5,choice5)
choice6 <- data.frame(stu$schoolcode6,choice6)
names(choice1) <- c("code","choice")
names(choice2) <- c("code","choice")
names(choice3) <- c("code","choice")
names(choice4) <- c("code","choice")
names(choice5) <- c("code","choice")
names(choice6) <- c("code","choice")
allchoice <- bind_rows(choice1,choice2,choice3,choice4,choice5,choice6)
allchoice <- data.frame(unique(allchoice))
allchoice$district = sss$sssdistrict[match(allchoice$code,sss$schoolcode)]
allchoice$lat = sss$ssslat[match(allchoice$code,sss$schoolcode)]
allchoice$long = sss$ssslong[match(allchoice$code,sss$schoolcode)]
cutoff <- stu %>%
  group_by(admit)%>%
  summarise(cutoff = min(score))
quality <- stu %>%
  group_by(admit)%>%
  summarise(quality = mean(score))
size <- stu %>%
  group_by(admit)%>%
  summarize(size = n())
allchoice$cutoff = cutoff$cutoff[match(allchoice$choice,cutoff$admit)]
allchoice$quality = quality$quality[match(allchoice$choice,quality$admit)]
allchoice$size = size$size[match(allchoice$choice,size$admit)]

#3
stu$jsslat = jss$point_y[match(stu$jssdistrict,jss$jssdistrict)]
stu$jsslong = jss$point_x[match(stu$jssdistrict,jss$jssdistrict)]
stu$lat1 = sss$ssslat[match(stu$schoolcode1,sss$schoolcode)]
stu$lat2 = sss$ssslat[match(stu$schoolcode2,sss$schoolcode)]
stu$lat3 = sss$ssslat[match(stu$schoolcode3,sss$schoolcode)]
stu$lat4 = sss$ssslat[match(stu$schoolcode4,sss$schoolcode)]
stu$lat5 = sss$ssslat[match(stu$schoolcode5,sss$schoolcode)]
stu$lat6 = sss$ssslat[match(stu$schoolcode6,sss$schoolcode)]
stu$long1 = sss$ssslong[match(stu$schoolcode1,sss$schoolcode)]
stu$long2 = sss$ssslong[match(stu$schoolcode2,sss$schoolcode)]
stu$long3 = sss$ssslong[match(stu$schoolcode3,sss$schoolcode)]
stu$long4 = sss$ssslong[match(stu$schoolcode4,sss$schoolcode)]
stu$long5 = sss$ssslong[match(stu$schoolcode5,sss$schoolcode)]
stu$long6 = sss$ssslong[match(stu$schoolcode6,sss$schoolcode)]
stu$dist1 = ((69.172*(stu$long1-stu$jsslong)*cos(stu$jsslat/57.3))^2+(69.172*(stu$lat1-stu$jsslat))^2)^0.5
stu$dist2 = ((69.172*(stu$long2-stu$jsslong)*cos(stu$jsslat/57.3))^2+(69.172*(stu$lat2-stu$jsslat))^2)^0.5
stu$dist3 = ((69.172*(stu$long3-stu$jsslong)*cos(stu$jsslat/57.3))^2+(69.172*(stu$lat3-stu$jsslat))^2)^0.5
stu$dist4 = ((69.172*(stu$long4-stu$jsslong)*cos(stu$jsslat/57.3))^2+(69.172*(stu$lat4-stu$jsslat))^2)^0.5
stu$dist5 = ((69.172*(stu$long5-stu$jsslong)*cos(stu$jsslat/57.3))^2+(69.172*(stu$lat5-stu$jsslat))^2)^0.5
stu$dist6 = ((69.172*(stu$long6-stu$jsslong)*cos(stu$jsslat/57.3))^2+(69.172*(stu$lat6-stu$jsslat))^2)^0.5

#4
mean(cutoff$cutoff, na.rm=TRUE)
sd(cutoff$cutoff, na.rm=TRUE)
mean(quality$quality, na.rm=TRUE)
sd(quality$quality, na.rm=TRUE)
stu$admitdist = ((69.172*(stu$admitlong-stu$jsslong)*cos(stu$jsslat/57.3))^2+(69.172*(stu$admitlat-stu$jsslat))^2)^0.5
distance <- stu %>%
  group_by(admit)%>%
  summarise(distance = mean(admitdist))
mean(distance$distance, na.rm=TRUE)
sd(distance$distance, na.rm=TRUE)
stu <- within(stu, quantile <- as.integer(cut(score, quantile(score, probs=0:4/4, na.rm=TRUE), include.lowest=TRUE)))
cutoff1 <- stu %>%
  group_by(admit,quantile==1)%>%
  summarise(cutoff = min(score))
cutoff2 <- stu %>%
  group_by(admit,quantile==2)%>%
  summarise(cutoff = min(score))
cutoff3 <- stu %>%
  group_by(admit,quantile==3)%>%
  summarise(cutoff = min(score))
cutoff4 <- stu %>%
  group_by(admit,quantile==4)%>%
  summarise(cutoff = min(score))
cutoff1<-cutoff1[!(cutoff1$`quantile == 1`==FALSE),]
cutoff2<-cutoff2[!(cutoff2$`quantile == 2`==FALSE),]
cutoff3<-cutoff3[!(cutoff3$`quantile == 3`==FALSE),]
cutoff4<-cutoff4[!(cutoff4$`quantile == 4`==FALSE),]
quality1 <- stu %>%
  group_by(admit,quantile==1)%>%
  summarise(quality = mean(score))
quality2 <- stu %>%
  group_by(admit,quantile==2)%>%
  summarise(quality = mean(score))
quality3 <- stu %>%
  group_by(admit,quantile==3)%>%
  summarise(quality = mean(score))
quality4 <- stu %>%
  group_by(admit,quantile==4)%>%
  summarise(quality = mean(score))
quality1<-quality1[!(quality1$`quantile == 1`==FALSE),]
quality2<-quality2[!(quality2$`quantile == 2`==FALSE),]
quality3<-quality3[!(quality3$`quantile == 3`==FALSE),]
quality4<-quality4[!(quality4$`quantile == 4`==FALSE),]
distance1 <- stu %>%
  group_by(admit,quantile==1)%>%
  summarise(distance = mean(admitdist))
distance2 <- stu %>%
  group_by(admit,quantile==2)%>%
  summarise(distance = mean(admitdist))
distance3 <- stu %>%
  group_by(admit,quantile==3)%>%
  summarise(distance = mean(admitdist))
distance4 <- stu %>%
  group_by(admit,quantile==4)%>%
  summarise(distance = mean(admitdist))
distance1<-distance1[!(distance1$`quantile == 1`==FALSE),]
distance2<-distance2[!(distance2$`quantile == 2`==FALSE),]
distance3<-distance3[!(distance3$`quantile == 3`==FALSE),]
distance4<-distance4[!(distance4$`quantile == 4`==FALSE),]
mean(cutoff1$cutoff, na.rm=TRUE)
sd(cutoff1$cutoff, na.rm=TRUE)
mean(quality1$quality, na.rm=TRUE)
sd(quality1$quality, na.rm=TRUE)
mean(distance1$distance, na.rm=TRUE)
sd(distance1$distance, na.rm=TRUE)
mean(cutoff2$cutoff, na.rm=TRUE)
sd(cutoff2$cutoff, na.rm=TRUE)
mean(quality2$quality, na.rm=TRUE)
sd(quality2$quality, na.rm=TRUE)
mean(distance2$distance, na.rm=TRUE)
sd(distance2$distance, na.rm=TRUE)
mean(cutoff3$cutoff, na.rm=TRUE)
sd(cutoff3$cutoff, na.rm=TRUE)
mean(quality3$quality, na.rm=TRUE)
sd(quality3$quality, na.rm=TRUE)
mean(distance3$distance, na.rm=TRUE)
sd(distance3$distance, na.rm=TRUE)
mean(cutoff4$cutoff, na.rm=TRUE)
sd(cutoff4$cutoff, na.rm=TRUE)
mean(quality4$quality, na.rm=TRUE)
sd(quality4$quality, na.rm=TRUE)
mean(distance4$distance, na.rm=TRUE)
sd(distance4$distance, na.rm=TRUE)

#5
cutoff <- within(cutoff, decile <- as.integer(cut(cutoff, quantile(cutoff, probs=0:10/10, na.rm=TRUE), include.lowest=TRUE)))
stu$dec1 = cutoff$decile[match(stu$choice1,cutoff$admit)]
stu$dec2 = cutoff$decile[match(stu$choice2,cutoff$admit)]
stu$dec3 = cutoff$decile[match(stu$choice3,cutoff$admit)]
stu$dec4 = cutoff$decile[match(stu$choice4,cutoff$admit)]
stu$dec5 = cutoff$decile[match(stu$choice5,cutoff$admit)]
stu$dec6 = cutoff$decile[match(stu$choice6,cutoff$admit)]
stu$numdecs <- apply(stu[66:71], 1, function(x) length(unique(na.omit(x))))
cutoff <- within(cutoff, quantile <- as.integer(cut(cutoff, quantile(cutoff, probs=0:4/4, na.rm=TRUE), include.lowest=TRUE)))
stu$quan1 = cutoff$quantile[match(stu$choice1,cutoff$admit)]
stu$quan2 = cutoff$quantile[match(stu$choice2,cutoff$admit)]
stu$quan3 = cutoff$quantile[match(stu$choice3,cutoff$admit)]
stu$quan4 = cutoff$quantile[match(stu$choice4,cutoff$admit)]
stu$quan5 = cutoff$quantile[match(stu$choice5,cutoff$admit)]
stu$quan6 = cutoff$quantile[match(stu$choice6,cutoff$admit)]
stu$numquans <- apply(stu[74:79], 1, function(x) length(unique(na.omit(x))))