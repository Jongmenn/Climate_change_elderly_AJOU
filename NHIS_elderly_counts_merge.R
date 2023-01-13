#libary
pacman::p_load(dplyr,lubridate,ggmap,raster,rgeos,maptools,rgdal,ggrepel,readxl,
               mgcv,stringr,data.table,splines,metafor,dlnm,survival,gamm4,lmtest,
               sqldf,reshape2,RColorBrewer,scales)

setwd("D:\\아주대\\기후보건_노령연구\\공단자료")
d01<-read_excel("elderly_hospitalization_revise.xlsx",sheet=1)
d02<-read_excel("elderly_hospitalization_revise.xlsx",sheet=2)
d03<-read_excel("elderly_hospitalization_revise.xlsx",sheet=3)
d04<-read_excel("elderly_hospitalization_revise.xlsx",sheet=4)
d05<-read_excel("elderly_hospitalization_revise.xlsx",sheet=5)
d06<-read_excel("elderly_hospitalization_revise.xlsx",sheet=6)
d07<-read_excel("elderly_hospitalization_revise.xlsx",sheet=7)
d08<-read_excel("elderly_hospitalization_revise.xlsx",sheet=8)
d09<-read_excel("elderly_hospitalization_revise.xlsx",sheet=9)
d10<-read_excel("elderly_hospitalization_revise.xlsx",sheet=10)
d11<-read_excel("elderly_hospitalization_revise.xlsx",sheet=11)
d12<-read_excel("elderly_hospitalization_revise.xlsx",sheet=12)
d13<-read_excel("elderly_hospitalization_revise.xlsx",sheet=13)
d14<-read_excel("elderly_hospitalization_revise.xlsx",sheet=14)
d15<-read_excel("elderly_hospitalization_revise.xlsx",sheet=15)
d16<-read_excel("elderly_hospitalization_revise.xlsx",sheet=16)
d17<-read_excel("elderly_hospitalization_revise.xlsx",sheet=17)
d18<-read_excel("elderly_hospitalization_revise.xlsx",sheet=18)
d19<-read_excel("elderly_hospitalization_revise.xlsx",sheet=19)
d20<-read_excel("elderly_hospitalization_revise.xlsx",sheet=20)
d21<-read_excel("elderly_hospitalization_revise.xlsx",sheet=21)
d22<-read_excel("elderly_hospitalization_revise.xlsx",sheet=22)
d23<-read_excel("elderly_hospitalization_revise.xlsx",sheet=23)
d24<-read_excel("elderly_hospitalization_revise.xlsx",sheet=24)
d25<-read_excel("elderly_hospitalization_revise.xlsx",sheet=25)
d26<-read_excel("elderly_hospitalization_revise.xlsx",sheet=26)
d27<-read_excel("elderly_hospitalization_revise.xlsx",sheet=27)
d28<-read_excel("elderly_hospitalization_revise.xlsx",sheet=28)
d29<-read_excel("elderly_hospitalization_revise.xlsx",sheet=29)
d30<-read_excel("elderly_hospitalization_revise.xlsx",sheet=30)
d31<-read_excel("elderly_hospitalization_revise.xlsx",sheet=31)
d32<-read_excel("elderly_hospitalization_revise.xlsx",sheet=32)
d33<-read_excel("elderly_hospitalization_revise.xlsx",sheet=33)
d34<-read_excel("elderly_hospitalization_revise.xlsx",sheet=34)
d35<-read_excel("elderly_hospitalization_revise.xlsx",sheet=35)
d36<-read_excel("elderly_hospitalization_revise.xlsx",sheet=36)
d37<-read_excel("elderly_hospitalization_revise.xlsx",sheet=37)
d38<-read_excel("elderly_hospitalization_revise.xlsx",sheet=38)
d39<-read_excel("elderly_hospitalization_revise.xlsx",sheet=39)
d40<-read_excel("elderly_hospitalization_revise.xlsx",sheet=40)
d41<-read_excel("elderly_hospitalization_revise.xlsx",sheet=41)
d42<-read_excel("elderly_hospitalization_revise.xlsx",sheet=42)
d43<-read_excel("elderly_hospitalization_revise.xlsx",sheet=43)
d44<-read_excel("elderly_hospitalization_revise.xlsx",sheet=44)
d45<-read_excel("elderly_hospitalization_revise.xlsx",sheet=45)

zz<-rbind(dim(d01),dim(d02),dim(d03),dim(d04),dim(d05),
          dim(d06),dim(d07),dim(d08),dim(d09),dim(d10),
          dim(d11),dim(d12),dim(d13),dim(d14),dim(d15),
          dim(d16),dim(d17),dim(d18),dim(d19),dim(d20),
          dim(d21),dim(d22),dim(d23),dim(d24),dim(d25),
          dim(d26),dim(d27),dim(d28),dim(d29),dim(d30),
          dim(d31),dim(d32),dim(d33),dim(d34),dim(d35),
          dim(d36),dim(d37),dim(d38),dim(d39),dim(d40),
          dim(d41),dim(d42),dim(d43),dim(d44),dim(d45))

View(zz)
setwd("D:\\아주대\\기후보건_노령연구")
dth<-read.csv("climate_change_elderly_death_dailycount.csv",fileEncoding = "euc-kr")

#지역코드 & 레이블
#통계청 사망자료 지역코드 
sidolist_stat=c(11,21,22,23,24,25,26,29,31,32,33,34,35,36,37,38,39)
#공단자료 지역코드
sidolist_nhis=c(11,26,27,28,29,30,31,36,41,42,43,44,45,46,47,48,50)

#17개 시도 영문
sidoname=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan",
           "Sejong","KK", "KW", "CB", "CN", "JB", "JN", "KB", "KN", "JJ")

#17개 시도 한글
area=c("서울","부산","대구","인천","광주","대전","울산","세종","경기",
       "강원","충북","충남","전북","전남","경북","경남","제주")

#지역코드 데이터프레임
sido_df=data.frame(sido=sidolist_stat,sidolist_nhis)


wea<-dth %>% dplyr:: select(ddate:EN_SIDO,mintemp:pm25_model) %>% left_join(sido_df,by="sido") %>% 
  filter(year>=2017) %>% mutate(key=paste0(sidolist_nhis,"-",year,substr(ddate,6,7),substr(ddate,9,10)))

relab<-function(d){
names(d)<-gsub("m_age6579","age6579_m",names(d))
names(d)<-gsub("f_age6579","age6579_f",names(d))
names(d)<-gsub("m_age80"  ,"age80_m"  ,names(d))
names(d)<-gsub("f_age80"  ,"age80_f"  ,names(d))
d}

d01<-relab(d01);d02<-relab(d02);d03<-relab(d03);d04<-relab(d04);d05<-relab(d05)
d06<-relab(d06);d07<-relab(d07);d08<-relab(d08);d09<-relab(d09);d10<-relab(d10)
d11<-relab(d11);d12<-relab(d12);d13<-relab(d13);d14<-relab(d14);d15<-relab(d15)
d16<-relab(d16);d17<-relab(d17);d18<-relab(d18);d19<-relab(d19);d20<-relab(d20)
d21<-relab(d21);d22<-relab(d22);d23<-relab(d23);d24<-relab(d24);d25<-relab(d25)
d26<-relab(d26);d27<-relab(d27);d28<-relab(d28);d29<-relab(d29);d30<-relab(d30)
d31<-relab(d31);d32<-relab(d32);d33<-relab(d33);d34<-relab(d34);d35<-relab(d35)
d36<-relab(d36);d37<-relab(d37);d38<-relab(d38);d39<-relab(d39);d40<-relab(d40)
d41<-relab(d41);d42<-relab(d42);d43<-relab(d43);d44<-relab(d44);d45<-relab(d45)

wea2<-wea %>% 
  left_join(d01) %>% left_join(d02) %>% left_join(d03) %>% left_join(d04) %>% left_join(d05) %>% 
  left_join(d06) %>% left_join(d07) %>% left_join(d08) %>% left_join(d09) %>% left_join(d10) %>% 
  left_join(d11) %>% left_join(d12) %>% left_join(d13) %>% left_join(d14) %>% left_join(d15) %>% 
  left_join(d16) %>% left_join(d17) %>% left_join(d18) %>% left_join(d19) %>% left_join(d20) %>% 
  left_join(d21) %>% left_join(d22) %>% left_join(d23) %>% left_join(d24) %>% left_join(d25) %>% 
  left_join(d26) %>% left_join(d27) %>% left_join(d28) %>% left_join(d29) %>% left_join(d30) %>% 
  left_join(d31) %>% left_join(d32) %>% left_join(d33) %>% left_join(d34) %>% left_join(d35) %>% 
  left_join(d36) %>% left_join(d37) %>% left_join(d38) %>% left_join(d39) %>% left_join(d40) %>% 
  left_join(d41) %>% left_join(d42) %>% left_join(d43) %>% left_join(d44) %>% left_join(d45)

wea2<-wea2 %>% dplyr::select(ddate:EN_SIDO,sidolist_nhis,intestinal_tot:osteoporosis_age80_f,mintemp:pm25_model) %>% 
  dplyr::select(-(ddd))

setwd("D:\\아주대\\기후보건_노령연구\\공단자료")
write.csv(wea2,file="KCDA_AJOU_elderly_hospitalization.csv",row.names=F,fileEncoding = "euc-kr")

dat<-read_excel("KCDA_AJOU_elderly_hospitalization.xlsx")


dat %>% group_by(year,sido) %>% dplyr::summarise(tot=sum(senility_tot)) %>% View

