#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#library
pacman::p_load("dplyr","ggplot2","reshape2","sqldf","RColorBrewer","lubridate","lmtest","readxl","survival",
               "splines","data.table","stringr","tidyr","extrafont","scales","gridExtra","tsModel","mgcv",
               "gamm4")

#Time-series data
rep(ymd(seq(as.Date("2001-01-01"),as.Date("2020-12-31"),1)),17) %>% length

dd<-data.frame(date=rep(ymd(seq(as.Date("2001-01-01"),as.Date("2020-12-31"),1)),17),
               sido=rep(c(11,21:25,26,29,31:39),each=7305))
dd$key=paste0(dd$sido,"-",dd$date)

sido_df<-data.frame(sido=c(11,21:25,26,29,31:39),
                    KOR_SIDO=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북",
                               "전남","경북","경남","제주"),
                    EN_SIDO=c("seoul","busan","daegu","incheon","gwangju","daejeon","ulsan","sejong","ggyeonggi","gangwon","chungbuk","chungnam",
                              "jeonbuk","jeonnam","gyeongbuk","gyeongnam","jeju"))

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#기상청 원시자료 
setwd("D:\\SNU\\연구\\질병관리본부\\기후보건영향평가_평가체계구축및시범사업\\2022\\자료\\기상자료")
me<-read.csv("wea_ap2001_2020_update.csv",fileEncoding = "euc-kr")

me$key=paste0(me$area,"-",me$ddate)

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#version 1, 세부적으로 나눠서 각각 

#일일 건수, 시계열 자료 함수 
daily_count<-function(data){
  
  r<-sqldf("select date,sido, count(date) as Total,sum(sex_m) as sex_m,sum(sex_f) as sex_f,
           
           sum(ag01) as ag01,sum(ag02) as ag02,sum(ag03) as ag03,
           
           sum(ag01_m) as ag01_m,sum(ag02_m) as ag02_m,sum(ag03_m) as ag03_m,
           sum(ag01_f) as ag01_f,sum(ag02_f) as ag02_f,sum(ag03_f) as ag03_f from data group by date, sido") %>% arrange(date,sido)
  
  r$key=paste0(r$sido,"-",r$date)
  r<-r %>% select(key,Total, sex_m, sex_f,ag01,ag02,ag03,ag01_m,ag01_f,ag02_m,ag02_f,ag03_m,ag03_f)
  
  zz<-merge(dd,r,by="key",all.x=T)
  
  zz[is.na(zz)]<-0
  
  zz$year =year(zz$date)
  zz$month=month(zz$date)
  zz$day  =day(zz$date)
  zz$dow  =weekdays(zz$date)
  
  zz2<-merge(zz,sido_df,by="sido",all.x=T)
  
  zz2$key=paste0(zz2$KOR_SIDO,"-",zz2$date)
  zz2<-zz2 %>% select(-date)
  
  zz2 %>% select(sido,key,year:EN_SIDO) %>% head
  zz3<-zz2 %>% left_join (me %>% select(ddate,area,key,mintemp:pm25_model)) %>% arrange(sido)
  #zz3<-zz3 %>% dplyr:: select(key,date,sido,KOR_SIDO,EN_SIDO,year:dow,Total:ag19_f,mintemp:meanpress2) %>% arrange(sido);
  zz3}

ts01<-daily_count(z01);ts02<-daily_count(z02);ts03<-daily_count(z03);ts04<-daily_count(z04)
ts05<-daily_count(z05);ts06<-daily_count(z06);ts07<-daily_count(z07);ts08<-daily_count(z08)
ts09<-daily_count(z09);ts10<-daily_count(z10);ts11<-daily_count(z11);ts12<-daily_count(z12)
ts13<-daily_count(z13);ts14<-daily_count(z14);ts15<-daily_count(z15);ts16<-daily_count(z16)
ts17<-daily_count(z17);ts18<-daily_count(z18);ts19<-daily_count(z19);ts20<-daily_count(z20)
ts21<-daily_count(z21);ts22<-daily_count(z22);ts23<-daily_count(z23);ts24<-daily_count(z24)
ts25<-daily_count(z25);ts26<-daily_count(z26);ts27<-daily_count(z27);ts28<-daily_count(z28)
ts29<-daily_count(z29);ts30<-daily_count(z30);ts31<-daily_count(z31);ts32<-daily_count(z32)
ts33<-daily_count(z33);ts34<-daily_count(z34);ts35<-daily_count(z35);ts36<-daily_count(z36)
ts37<-daily_count(z37);ts38<-daily_count(z38);ts39<-daily_count(z39);ts40<-daily_count(z40)
ts41<-daily_count(z41);ts42<-daily_count(z42);ts43<-daily_count(z43);ts44<-daily_count(z44)
ts45<-daily_count(z45);ts46<-daily_count(z46);ts47<-daily_count(z47);ts48<-daily_count(z48)
ts49<-daily_count(z49)

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#

setwd("D:\\아주대\\기후보건_노령연구\\out")

write.csv(ts01,file="ts01.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts02,file="ts02.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts03,file="ts03.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts04,file="ts04.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts05,file="ts05.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts06,file="ts06.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts07,file="ts07.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts08,file="ts08.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts09,file="ts09.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts10,file="ts10.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts11,file="ts11.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts12,file="ts12.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts13,file="ts13.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts14,file="ts14.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts15,file="ts15.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts16,file="ts16.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts17,file="ts17.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts18,file="ts18.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts19,file="ts19.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts20,file="ts20.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts21,file="ts21.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts22,file="ts22.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts23,file="ts23.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts24,file="ts24.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts25,file="ts25.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts26,file="ts26.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts27,file="ts27.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts28,file="ts28.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts29,file="ts29.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts30,file="ts30.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts31,file="ts31.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts32,file="ts32.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts33,file="ts33.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts34,file="ts34.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts35,file="ts35.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts36,file="ts36.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts37,file="ts37.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts38,file="ts38.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts39,file="ts39.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts40,file="ts40.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts41,file="ts41.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts42,file="ts42.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts43,file="ts43.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts44,file="ts44.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts45,file="ts45.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts46,file="ts46.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts47,file="ts47.csv",row.names=F,na="",fileEncoding = "euc-kr");write.csv(ts48,file="ts48.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(ts49,file="ts49.csv",row.names=F,na="",fileEncoding = "euc-kr")

