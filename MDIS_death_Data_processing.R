##################################################
##########아주대-기후보건 노령인구################
##################################################

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#library
pacman::p_load("dplyr","ggplot2","reshape2","sqldf","RColorBrewer","lubridate","lmtest","readxl","survival",
               "splines","data.table","stringr","tidyr","extrafont","scales","gridExtra","tsModel","mgcv","gamm4")

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#working directory
setwd("D:\\EUMC\\데이터관리\\통계청_MDIS\\보건복지\\사망원인통계\\사망연간자료\\A형")

dataset_label<-list.files()[grep("사망_연간자료",list.files())]

data.list=NULL
#원자료 변수 이름 동일, 이중 필요한 것만, 주소(시도), 성별, 사망연월일, 사망연령(각세),사망원인 1, 사망원인 2
for(i in 1:length(dataset_label)){
  data.list[[i]]<-read.csv(dataset_label[i],fileEncoding = "euc-kr") %>% dplyr:: select("사망자.주소.시도.":"사망연월일","사망연령.각세.",
                                                                                        "사망원인1","사망원인2")
  names(data.list[[i]])=c("sido","sex","death_date","death_age","death1","death2")
  print(i)}

stat01_20<-as.data.frame(do.call(rbind,data.list));rm(data.list)

stat01_20$date =ymd(stat01_20$death_date)
stat01_20$year =year(stat01_20$date)
stat01_20$month=month(stat01_20$date)
stat01_20$day  =day(stat01_20$date)

nrow(stat01_20) #5,268,698

#시도, 사망연령, 사망원인 결측 검토 
table(is.na(stat01_20$sido))
table(is.na(stat01_20$death_age))
table(is.na(stat01_20$death1))

#사망연령은 "999"로 결측값 표기해서 확인 필요 
#missing value:999
summary(stat01_20$death_age)
addmargins(table(stat01_20$death_age))

subset(stat01_20,death_age==999)$year %>% table

stat01_20<-subset(stat01_20,death_age!=999)

nrow(stat01_20) #5,268,072
summary(stat01_20$death_age)

addmargins(substr(stat01_20$death1,1,1) %>% table)

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#

table(stat01_20$death_age)

#연령 65세 미만, 65-79세, 80세 이상 
stat01_20$ag01=with(stat01_20,ifelse(death_age<65,1,0))
stat01_20$ag02=with(stat01_20,ifelse(death_age>=65 & death_age<=79,1,0))
stat01_20$ag03=with(stat01_20,ifelse(death_age>=80,1,0))

addmargins(table(stat01_20$ag01))
addmargins(table(stat01_20$ag02))
addmargins(table(stat01_20$ag03))

#성별
stat01_20$sex_m=with(stat01_20,ifelse(sex==1,1,0))
stat01_20$sex_f=with(stat01_20,ifelse(sex==2,1,0))

#시도
stat01_20$sido01=with(stat01_20,ifelse(sido==11,1,0));stat01_20$sido02=with(stat01_20,ifelse(sido==21,1,0))
stat01_20$sido03=with(stat01_20,ifelse(sido==22,1,0));stat01_20$sido04=with(stat01_20,ifelse(sido==23,1,0))
stat01_20$sido05=with(stat01_20,ifelse(sido==24,1,0));stat01_20$sido06=with(stat01_20,ifelse(sido==25,1,0))
stat01_20$sido07=with(stat01_20,ifelse(sido==26,1,0));stat01_20$sido08=with(stat01_20,ifelse(sido==29,1,0))
stat01_20$sido09=with(stat01_20,ifelse(sido==31,1,0));stat01_20$sido10=with(stat01_20,ifelse(sido==32,1,0))
stat01_20$sido11=with(stat01_20,ifelse(sido==33,1,0));stat01_20$sido12=with(stat01_20,ifelse(sido==34,1,0))
stat01_20$sido13=with(stat01_20,ifelse(sido==35,1,0));stat01_20$sido14=with(stat01_20,ifelse(sido==36,1,0))
stat01_20$sido15=with(stat01_20,ifelse(sido==37,1,0));stat01_20$sido16=with(stat01_20,ifelse(sido==38,1,0))
stat01_20$sido17=with(stat01_20,ifelse(sido==39,1,0))

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#연령별 성별:남
stat01_20$ag01_m=with(stat01_20,ifelse(sex==1 & ag01==1,1,0))
stat01_20$ag02_m=with(stat01_20,ifelse(sex==1 & ag02==1,1,0))
stat01_20$ag03_m=with(stat01_20,ifelse(sex==1 & ag03==1,1,0))

#연령별 성별:여
stat01_20$ag01_f=with(stat01_20,ifelse(sex==2 & ag01==1,1,0))
stat01_20$ag02_f=with(stat01_20,ifelse(sex==2 & ag02==1,1,0))
stat01_20$ag03_f=with(stat01_20,ifelse(sex==2 & ag03==1,1,0))


#사망원인 검토, 사망원인 2는 V, W, X, Y 등임
table(substr(stat01_20$death2,1,1))

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#연도별-지역별  질환 자료 집계 

yr=data.frame(year=2001:2020)

label=c("전체","연령","<65","65-79","80+",
        "성별","남","여",
        "지역","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북",
        "전남","경북","경남","제주","연령그룹-성별","65세 미만 남","65-79세 남","80+ 남","65세 미만 여","65-79세 여","80+ 여")


#집계 테이블, 함수
agg_tb<-function(data){
  d.list=NULL
  for(i in 1:20){
    
    d<-subset(data,year==yr$year[i])
    
    d.list[[i]]<-data.frame(count=apply(d %>% dplyr:: select(ag01:ag03_f),2,sum))}
  
  agg_df<-as.data.frame(do.call(cbind,d.list))
  names(agg_df)=paste0(2001:2020)
  
  agg_df$Total=apply(agg_df %>% dplyr:: select(`2001`:`2020`),1,sum)
  
  year.tot<-apply(agg_df[1:3,] %>% dplyr:: select(`2001`:`2020`),2,sum)
  tot<-sum(year.tot)
  
  agg_df<-agg_df %>% select(Total,`2001`:`2020`)
  
  agg_df.r<-rbind(c(tot,year.tot),NA,agg_df[1:3,],NA,agg_df[4:5,],NA,agg_df[6:22,],NA,agg_df[23:28,])
  row.names(agg_df.r)=label
  agg_df.r}

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#추가한 부분
#전체 감염병 질환 (A00-A09)
z01<-subset(stat01_20,substr(death1,1,3) %in% c("A00","A01","A02","A03","A04","A05","A06","A07","A08","A09"))

#콜레라 (A00)
z02<-subset(stat01_20,substr(death1,1,3) %in% c("A00"))

#장티푸스 및 파라티푸스 (A01)
z03<-subset(stat01_20,substr(death1,1,3) %in% c("A01"))

#기타 살모넬라감염 (A02)
z04<-subset(stat01_20,substr(death1,1,3) %in% c("A02"))

#시겔라증 (A03)
z05<-subset(stat01_20,substr(death1,1,3) %in% c("A03"))

#기타 세균성 장감염 (A04)
z06<-subset(stat01_20,substr(death1,1,3) %in% c("A04"))

#달리 분류되지 않은 기타 세균성 음식매개중독 (A05)
z07<-subset(stat01_20,substr(death1,1,3) %in% c("A05"))

#아메바증 (A06)
z08<-subset(stat01_20,substr(death1,1,3) %in% c("A06"))

#기타 원충성 장질환 (A07)
z09<-subset(stat01_20,substr(death1,1,3) %in% c("A07"))

#바이러스성 및 기타 명시된 장감염 (A08)
z10<-subset(stat01_20,substr(death1,1,3) %in% c("A08"))

#감염성 및 상세불명 기원의 기타 위장염 및 결장염 (A09)
z11<-subset(stat01_20,substr(death1,1,3) %in% c("A09"))

#전체 심뇌혈관질환 (I00-I99)
z12<-subset(stat01_20,substr(death1,1,1) %in% c("I"))

#협심증 (I20)
z13<-subset(stat01_20,substr(death1,1,3) %in% c("I20"))

#전체 허혈성 심질환 (I20-5)
z14<-subset(stat01_20,substr(death1,1,3) %in% c("I20","I21","I22","I23","I24","I25"))

#심근경색(I21-5)
z15<-subset(stat01_20,substr(death1,1,3) %in% c("I21","I22","I23","I24","I25"))

#전체 뇌졸중 (I60-7, I690-4, G458-9)
z16<-rbind(subset(stat01_20,substr(death1,1,3) %in% c("I60","I61","I62","I63","I64","I65","I66","I67")),
           subset(stat01_20,substr(death1,1,4) %in% c("I690","I691","I692","I693","I694","G458","G459")))

#출혈성 뇌졸중  (I60-I62, I690-I692)
z17<-rbind(subset(stat01_20,substr(death1,1,3) %in% c("I60","I61","I62")),
           subset(stat01_20,substr(death1,1,4) %in% c("I690","I691","I692")))

#허혈성 뇌졸중  (I63,I65-I67, I693)
z18<-rbind(subset(stat01_20,substr(death1,1,3) %in% c("I63","I65","I66","I67")),
           subset(stat01_20,substr(death1,1,4) %in% c("I693")))

#기타 뇌졸중  (I64, I694)
z19<-rbind(subset(stat01_20,substr(death1,1,3) %in% c("I64")),
           subset(stat01_20,substr(death1,1,4) %in% c("I694")))

#일과성 뇌허혈질환 (G458-G459)
z20<-subset(stat01_20,substr(death1,1,4) %in% c("G458","G459"))

#자살 (X60-X84)
z21<-subset(stat01_20,substr(death2,1,3) %in% paste0("X",c(60:84)))

#우울증 (F31-F39)
z22<-subset(stat01_20,substr(death1,1,3) %in% paste0("F",c(31:39)))

#전체 호흡기 (J00-J99)
z23<-subset(stat01_20,substr(death1,1,1) %in% c("J"))

#급성 상기도 감염 (J00-J06)
z24<-subset(stat01_20,substr(death1,1,3) %in% c("J00","J01","J02","J03","J04","J05","J06"))

#인플루엔자 (J09-J11)
z25<-subset(stat01_20,substr(death1,1,3) %in% c("J09","J10","J11"))

#폐렴 (J12-J18)
z26<-subset(stat01_20,substr(death1,1,3) %in% c("J12","J13","J14","J15","J16","J17","J18"))

#급성하부호흡기감염 (J20-J22)
z27<-subset(stat01_20,substr(death1,1,3) %in% c("J20","J21","J22"))

#천식 (J45-46)
z28<-subset(stat01_20,substr(death1,1,3) %in% c("J45","J46"))

#용적고갈 탈수 (E86)
z29<-subset(stat01_20,substr(death1,1,3) %in% c("E86"))

#급성 신부전증 (N17)
z30<-subset(stat01_20,substr(death1,1,3) %in% c("N17"))

#동상 (T33T35)
z31<-subset(stat01_20,substr(death1,1,3) %in% c("T33","T34","T35"))

#고온관련 질환(열사병, 열피로) (T67)
z32<-subset(stat01_20,substr(death1,1,3) %in% c("T67"))

#저체온증 (T68)
z33<-subset(stat01_20,substr(death1,1,3) %in% c("T68"))

#비동결 및 기타 (T69)
z34<-subset(stat01_20,substr(death1,1,3) %in% c("T69"))

#혈관운동성 및 앨러지성 비염 
z35<-subset(stat01_20,substr(death1,1,3) %in% c("J30"))

#만성폐쇄성폐질환 (J40-J44)
z36<-subset(stat01_20,substr(death1,1,3) %in% c("J40","J41","J42","J43","J44"))

#당뇨 (E10-E14)
z37<-subset(stat01_20,substr(death1,1,3) %in% c("E10","E11","E12","E13","E14"))

#1형 당뇨 (E10)
z38<-subset(stat01_20,substr(death1,1,3) %in% c("E10"))

#2형 당뇨 (E11)
z39<-subset(stat01_20,substr(death1,1,3) %in% c("E11"))

#파킨슨병 (G20)
z40<-subset(stat01_20,substr(death1,1,3) %in% c("G20"))

#파킨슨증 (G20-G22)
z41<-subset(stat01_20,substr(death1,1,3) %in% c("G20","G21","G22"))

#알츠하이머병(G30)
z42<-subset(stat01_20,substr(death1,1,3) %in% c("G30"))

#치매 (F00-F03)
z43<-subset(stat01_20,substr(death1,1,3) %in% c("F00","F01","F02","F03"))

#기타 퇴행성 질환 (G30-G32)
z44<-subset(stat01_20,substr(death1,1,3) %in% c("G30","G31","G32"))

#노쇠 (R54)
z45<-subset(stat01_20,substr(death1,1,3) %in% c("R54"))

#골다공증 (M80-M85)
z46<-subset(stat01_20,substr(death1,1,3) %in% c("M80","M81","M82","M83","M84","M85"))

#병적 골절을 동반한 골다공증 (M80)
z47<-subset(stat01_20,substr(death1,1,3) %in% c("M80"))

#전체 원인 사망 (A00-Z99)
z48<-stat01_20

#비사고 사망 (A00-R99)
z49<-subset(stat01_20,!substr(death1,1,1) %in% c("S","T",'U'))


#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
out01<-agg_tb(z01);out02<-agg_tb(z02);out03<-agg_tb(z03);out04<-agg_tb(z04)
out05<-agg_tb(z05);out06<-agg_tb(z06);out07<-agg_tb(z07);out08<-agg_tb(z08)
out09<-agg_tb(z09);out10<-agg_tb(z10);out11<-agg_tb(z11);out12<-agg_tb(z12)
out13<-agg_tb(z13);out14<-agg_tb(z14);out15<-agg_tb(z15);out16<-agg_tb(z16)
out17<-agg_tb(z17);out18<-agg_tb(z18);out19<-agg_tb(z19);out20<-agg_tb(z20)
out21<-agg_tb(z21);out22<-agg_tb(z22);out23<-agg_tb(z23);out24<-agg_tb(z24)
out25<-agg_tb(z25);out26<-agg_tb(z26);out27<-agg_tb(z27);out28<-agg_tb(z28)
out29<-agg_tb(z29);out30<-agg_tb(z30);out31<-agg_tb(z31);out32<-agg_tb(z32)
out33<-agg_tb(z33);out34<-agg_tb(z34);out35<-agg_tb(z35);out36<-agg_tb(z36)
out37<-agg_tb(z37);out38<-agg_tb(z38);out39<-agg_tb(z39);out40<-agg_tb(z40)
out41<-agg_tb(z41);out42<-agg_tb(z42);out43<-agg_tb(z43);out44<-agg_tb(z44)
out45<-agg_tb(z45);out46<-agg_tb(z46);out47<-agg_tb(z47);out48<-agg_tb(z48)
out49<-agg_tb(z49)

#Save the resutls (01-30)
setwd("D:\\아주대\\기후보건_노령연구\\out")
write.csv(out01,file="out01.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out02,file="out02.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out03,file="out03.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out04,file="out04.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out05,file="out05.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out06,file="out06.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out07,file="out07.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out08,file="out08.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out09,file="out09.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out10,file="out10.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out11,file="out11.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out12,file="out12.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out13,file="out13.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out14,file="out14.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out15,file="out15.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out16,file="out16.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out17,file="out17.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out18,file="out18.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out19,file="out19.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out20,file="out20.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out21,file="out21.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out22,file="out22.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out23,file="out23.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out24,file="out24.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out25,file="out25.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out26,file="out26.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out27,file="out27.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out28,file="out28.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out29,file="out29.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out30,file="out30.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out31,file="out31.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out32,file="out32.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out33,file="out33.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out34,file="out34.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out35,file="out35.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out36,file="out36.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out37,file="out37.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out38,file="out38.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out39,file="out39.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out40,file="out40.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out41,file="out41.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out42,file="out42.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out43,file="out43.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out44,file="out44.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out45,file="out45.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out46,file="out46.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out47,file="out47.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out48,file="out48.csv",row.names=T,na="",fileEncoding = 'euc-kr')
write.csv(out49,file="out49.csv",row.names=T,na="",fileEncoding = 'euc-kr')
