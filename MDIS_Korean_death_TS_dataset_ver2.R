
daily_count<-function(data){
  z<-data %>% dplyr:: mutate(tot=Total   ,m=sex_m,f=sex_f,
                             age0064  =ag01,
                             age6579  =ag02,
                             age80    =ag03,
                             
                             age0064_m=ag01_m,age6579_m=ag02_m,age80_m  =ag03_m,
                             age0064_f=ag01_f,age6579_f=ag02_f,age80_f  =ag03_f,
                             ) %>%  select(tot:age80_f)
  tibble(z)}


tt01<-daily_count(ts01);names(tt01)=paste0("intestinal_infec_"  ,names(tt01))
tt02<-daily_count(ts02);names(tt02)=paste0("cholera_"           ,names(tt02))
tt03<-daily_count(ts03);names(tt03)=paste0("typhoid_para_"      ,names(tt03))
tt04<-daily_count(ts04);names(tt04)=paste0("othersalmone_"      ,names(tt04))
tt05<-daily_count(ts05);names(tt05)=paste0("shigellosis_"       ,names(tt05))
tt06<-daily_count(ts06);names(tt06)=paste0("otherbac_"          ,names(tt06))
tt07<-daily_count(ts07);names(tt07)=paste0("otherbac_food_"     ,names(tt07))
tt08<-daily_count(ts08);names(tt08)=paste0("amoebiasis_"        ,names(tt08))
tt09<-daily_count(ts09);names(tt09)=paste0("otherprotozoal_"    ,names(tt09))
tt10<-daily_count(ts10);names(tt10)=paste0("viral_intestinal_"  ,names(tt10))
tt11<-daily_count(ts11);names(tt11)=paste0("othergast_coli_"    ,names(tt11))
tt12<-daily_count(ts12);names(tt12)=paste0("cvd_"               ,names(tt12))
tt13<-daily_count(ts13);names(tt13)=paste0("angina_"            ,names(tt13))
tt14<-daily_count(ts14);names(tt14)=paste0("ischHD_"            ,names(tt14))
tt15<-daily_count(ts15);names(tt15)=paste0("MI_"                ,names(tt15))
tt16<-daily_count(ts16);names(tt16)=paste0("cerebvas_"          ,names(tt16))
tt17<-daily_count(ts17);names(tt17)=paste0("hemoStroke_"        ,names(tt17))
tt18<-daily_count(ts18);names(tt18)=paste0("ischStroke_"        ,names(tt18))
tt19<-daily_count(ts19);names(tt19)=paste0("otherStroke_"       ,names(tt19))
tt20<-daily_count(ts20);names(tt20)=paste0("TIA_"               ,names(tt20))
tt21<-daily_count(ts21);names(tt21)=paste0("suicide_"           ,names(tt21))
tt22<-daily_count(ts22);names(tt22)=paste0("mental_"            ,names(tt22))
tt23<-daily_count(ts23);names(tt23)=paste0("resp_"              ,names(tt23))
tt24<-daily_count(ts24);names(tt24)=paste0("acuteup_"           ,names(tt24))
tt25<-daily_count(ts25);names(tt25)=paste0("influenza_"         ,names(tt25))
tt26<-daily_count(ts26);names(tt26)=paste0("pneum_"             ,names(tt26))
tt27<-daily_count(ts27);names(tt27)=paste0("ALRI_"              ,names(tt27))
tt28<-daily_count(ts28);names(tt28)=paste0("asthma_"            ,names(tt28))
tt29<-daily_count(ts29);names(tt29)=paste0("voldep_"            ,names(tt29))
tt30<-daily_count(ts30);names(tt30)=paste0("aki_"               ,names(tt30))
tt31<-daily_count(ts31);names(tt31)=paste0("frost_"             ,names(tt31))
tt32<-daily_count(ts32);names(tt32)=paste0("heat_"              ,names(tt32))
tt33<-daily_count(ts33);names(tt33)=paste0("hypothermia_"       ,names(tt33))
tt34<-daily_count(ts34);names(tt34)=paste0("otherredutemp_"     ,names(tt34))
tt35<-daily_count(ts35);names(tt35)=paste0("vasomotor_allerg_"  ,names(tt35))
tt36<-daily_count(ts36);names(tt36)=paste0("copd_"              ,names(tt36))
tt37<-daily_count(ts37);names(tt37)=paste0("dm_"                ,names(tt37))
tt38<-daily_count(ts38);names(tt38)=paste0("dm_type1_"          ,names(tt38))
tt39<-daily_count(ts39);names(tt39)=paste0("dm_type2_"          ,names(tt39))
tt40<-daily_count(ts40);names(tt40)=paste0("parkinson_"         ,names(tt40))
tt41<-daily_count(ts41);names(tt41)=paste0("parkinsonism_"      ,names(tt41))
tt42<-daily_count(ts42);names(tt42)=paste0("alzheimer_"         ,names(tt42))
tt43<-daily_count(ts43);names(tt43)=paste0("dementia_"          ,names(tt43))
tt44<-daily_count(ts44);names(tt44)=paste0("other_degenerative_",names(tt44))
tt45<-daily_count(ts45);names(tt45)=paste0("senility_"          ,names(tt45))
tt46<-daily_count(ts46);names(tt46)=paste0("disorders_bone_"    ,names(tt46))
tt47<-daily_count(ts47);names(tt47)=paste0("osteoporosis_"      ,names(tt47))
tt48<-daily_count(ts48);names(tt48)=paste0("allcause_"          ,names(tt48))
tt49<-daily_count(ts49);names(tt49)=paste0("nonacc_"            ,names(tt49))

library(dplyr)
version2<-cbind(ts01 %>% select(key,year:dow),
                tt01,tt02,tt03,tt04,tt05,tt06,tt07,tt08,tt09,tt10,
                tt11,tt12,tt13,tt14,tt15,tt16,tt17,tt18,tt19,tt20,
                tt21,tt22,tt23,tt24,tt25,tt26,tt27,tt28,tt29,tt30,
                tt31,tt32,tt33,tt34,tt35,tt36,tt37,tt38,tt39,tt40,
                tt41,tt42,tt43,tt44,tt45,tt46,tt47,tt48,tt49,ts01 %>% select(mintemp:pm25_model))

sido_df<-data.frame(sido=c(11,21:25,26,29,31:39),
                    area=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북",
                           "전남","경북","경남","제주"),
                    EN_SIDO=c("seoul","busan","daegu","incheon","gwangju","daejeon","ulsan","sejong","ggyeonggi","gangwon","chungbuk","chungnam",
                              "jeonbuk","jeonnam","gyeongbuk","gyeongnam","jeju"))

version2$area=substr(version2$key,1,2)
version2<-version2 %>% left_join(sido_df,by="area")
head(version2)
version2$ddate=ymd(substr(version2$key,4,13))

#serial number
version2$ddd=as.numeric(as.factor(version2$ddate))

table(version2$EN_SIDO)
version2<-version2 %>% select(ddate,year,month,day,ddd,sido,dow,area,EN_SIDO,
                              intestinal_infec_tot:nonacc_age80_f,
                              mintemp:pm25_model)

setwd("D:\\아주대\\기후보건_노령연구")
write.csv(version2,file="version2.csv",row.names=F,na="",fileEncoding = "euc-kr")
