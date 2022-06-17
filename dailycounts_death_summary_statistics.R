library(dplyr)

setwd("D:\\")
dat<-read.csv("climate_change_elderly_death_dailycount_20220603.csv")

z<-dat[,c(1:9,grep("_tot",names(dat)))]
zz<-z %>% group_by(ddate) %>% dplyr:: summarise(t01=sum(intestinal_infec_tot),t02=sum(cholera_tot),
                                                t03=sum(typhoid_para_tot)    ,t04=sum(othersalmone_tot),
                                                t05=sum(shigellosis_tot)     ,t06=sum(otherbac_tot),
                                                t07=sum(otherbac_food_tot)   ,t08=sum(amoebiasis_tot),
                                                t09=sum(otherprotozoal_tot)  ,t10=sum(viral_intestinal_tot),
                                                t11=sum(othergast_coli_tot)  ,t12=sum(cvd_tot),
                                                t13=sum(angina_tot)          ,t14=sum(ischHD_tot),
                                                t15=sum(MI_tot)              ,t16=sum(cerebvas_tot),
                                                t17=sum(hemoStroke_tot)      ,t18=sum(ischStroke_tot),
                                                t19=sum(otherStroke_tot)     ,t20=sum(TIA_tot),
                                                t21=sum(suicide_tot)         ,t22=sum(mental_tot),
                                                t23=sum(resp_tot)            ,t24=sum(acuteup_tot),
                                                t25=sum(influenza_tot)       ,t26=sum(pneum_tot),
                                                t27=sum(ALRI_tot)            ,t28=sum(asthma_tot),
                                                t29=sum(voldep_tot)          ,t30=sum(aki_tot),
                                                t31=sum(frost_tot)           ,t32=sum(heat_tot),
                                                t33=sum(hypothermia_tot)     ,t34=sum(otherredutemp_tot),
                                                t35=sum(vasomotor_allerg_tot),t36=sum(copd_tot),
                                                t37=sum(dm_tot)              ,t38=sum(dm_type1_tot),
                                                t39=sum(dm_type2_tot)        ,t40=sum(parkinson_tot),
                                                t41=sum(parkinsonism_tot)    ,t42=sum(alzheimer_tot),
                                                t43=sum(dementia_tot)        ,t44=sum(other_degenerative_tot),
                                                t45=sum(senility_tot)        ,t46=sum(disorders_bone_tot),
                                                t47=sum(osteoporosis_tot)    ,t48=sum(allcause_tot),
                                                t49=sum(nonacc_tot))

disease<-names(z)[10:58]

summ01<-with(zz,data.frame(disease=disease[1],Total=sum(t01),Mean=mean(t01),SD=sd(t01),Median=median(t01),Min=min(t01),Max=max(t01)))
summ02<-with(zz,data.frame(disease=disease[2],Total=sum(t02),Mean=mean(t02),SD=sd(t02),Median=median(t02),Min=min(t02),Max=max(t02)))
summ03<-with(zz,data.frame(disease=disease[3],Total=sum(t03),Mean=mean(t03),SD=sd(t03),Median=median(t03),Min=min(t03),Max=max(t03)))
summ04<-with(zz,data.frame(disease=disease[4],Total=sum(t04),Mean=mean(t04),SD=sd(t04),Median=median(t04),Min=min(t04),Max=max(t04)))
summ05<-with(zz,data.frame(disease=disease[5],Total=sum(t05),Mean=mean(t05),SD=sd(t05),Median=median(t05),Min=min(t05),Max=max(t05)))
summ06<-with(zz,data.frame(disease=disease[6],Total=sum(t06),Mean=mean(t06),SD=sd(t06),Median=median(t06),Min=min(t06),Max=max(t06)))
summ07<-with(zz,data.frame(disease=disease[7],Total=sum(t07),Mean=mean(t07),SD=sd(t07),Median=median(t07),Min=min(t07),Max=max(t07)))
summ08<-with(zz,data.frame(disease=disease[8],Total=sum(t08),Mean=mean(t08),SD=sd(t08),Median=median(t08),Min=min(t08),Max=max(t08)))
summ09<-with(zz,data.frame(disease=disease[9],Total=sum(t09),Mean=mean(t09),SD=sd(t09),Median=median(t09),Min=min(t09),Max=max(t09)))
summ10<-with(zz,data.frame(disease=disease[10],Total=sum(t10),Mean=mean(t10),SD=sd(t10),Median=median(t10),Min=min(t10),Max=max(t10)))
summ11<-with(zz,data.frame(disease=disease[11],Total=sum(t11),Mean=mean(t11),SD=sd(t11),Median=median(t11),Min=min(t11),Max=max(t11)))
summ12<-with(zz,data.frame(disease=disease[12],Total=sum(t12),Mean=mean(t12),SD=sd(t12),Median=median(t12),Min=min(t12),Max=max(t12)))
summ13<-with(zz,data.frame(disease=disease[13],Total=sum(t13),Mean=mean(t13),SD=sd(t13),Median=median(t13),Min=min(t13),Max=max(t13)))
summ14<-with(zz,data.frame(disease=disease[14],Total=sum(t14),Mean=mean(t14),SD=sd(t14),Median=median(t14),Min=min(t14),Max=max(t14)))
summ15<-with(zz,data.frame(disease=disease[15],Total=sum(t15),Mean=mean(t15),SD=sd(t15),Median=median(t15),Min=min(t15),Max=max(t15)))
summ16<-with(zz,data.frame(disease=disease[16],Total=sum(t16),Mean=mean(t16),SD=sd(t16),Median=median(t16),Min=min(t16),Max=max(t16)))
summ17<-with(zz,data.frame(disease=disease[17],Total=sum(t17),Mean=mean(t17),SD=sd(t17),Median=median(t17),Min=min(t17),Max=max(t17)))
summ18<-with(zz,data.frame(disease=disease[18],Total=sum(t18),Mean=mean(t18),SD=sd(t18),Median=median(t18),Min=min(t18),Max=max(t18)))
summ19<-with(zz,data.frame(disease=disease[19],Total=sum(t19),Mean=mean(t19),SD=sd(t19),Median=median(t19),Min=min(t19),Max=max(t19)))
summ20<-with(zz,data.frame(disease=disease[20],Total=sum(t20),Mean=mean(t20),SD=sd(t20),Median=median(t20),Min=min(t20),Max=max(t20)))
summ21<-with(zz,data.frame(disease=disease[21],Total=sum(t21),Mean=mean(t21),SD=sd(t21),Median=median(t21),Min=min(t21),Max=max(t21)))
summ22<-with(zz,data.frame(disease=disease[22],Total=sum(t22),Mean=mean(t22),SD=sd(t22),Median=median(t22),Min=min(t22),Max=max(t22)))
summ23<-with(zz,data.frame(disease=disease[23],Total=sum(t23),Mean=mean(t23),SD=sd(t23),Median=median(t23),Min=min(t23),Max=max(t23)))
summ24<-with(zz,data.frame(disease=disease[24],Total=sum(t24),Mean=mean(t24),SD=sd(t24),Median=median(t24),Min=min(t24),Max=max(t24)))
summ25<-with(zz,data.frame(disease=disease[25],Total=sum(t25),Mean=mean(t25),SD=sd(t25),Median=median(t25),Min=min(t25),Max=max(t25)))
summ26<-with(zz,data.frame(disease=disease[26],Total=sum(t26),Mean=mean(t26),SD=sd(t26),Median=median(t26),Min=min(t26),Max=max(t26)))
summ27<-with(zz,data.frame(disease=disease[27],Total=sum(t27),Mean=mean(t27),SD=sd(t27),Median=median(t27),Min=min(t27),Max=max(t27)))
summ28<-with(zz,data.frame(disease=disease[28],Total=sum(t28),Mean=mean(t28),SD=sd(t28),Median=median(t28),Min=min(t28),Max=max(t28)))
summ29<-with(zz,data.frame(disease=disease[29],Total=sum(t29),Mean=mean(t29),SD=sd(t29),Median=median(t29),Min=min(t29),Max=max(t29)))
summ30<-with(zz,data.frame(disease=disease[30],Total=sum(t30),Mean=mean(t30),SD=sd(t30),Median=median(t30),Min=min(t30),Max=max(t30)))
summ31<-with(zz,data.frame(disease=disease[31],Total=sum(t31),Mean=mean(t31),SD=sd(t31),Median=median(t31),Min=min(t31),Max=max(t31)))
summ32<-with(zz,data.frame(disease=disease[32],Total=sum(t32),Mean=mean(t32),SD=sd(t32),Median=median(t32),Min=min(t32),Max=max(t32)))
summ33<-with(zz,data.frame(disease=disease[33],Total=sum(t33),Mean=mean(t33),SD=sd(t33),Median=median(t33),Min=min(t33),Max=max(t33)))
summ34<-with(zz,data.frame(disease=disease[34],Total=sum(t34),Mean=mean(t34),SD=sd(t34),Median=median(t34),Min=min(t34),Max=max(t34)))
summ35<-with(zz,data.frame(disease=disease[35],Total=sum(t35),Mean=mean(t35),SD=sd(t35),Median=median(t35),Min=min(t35),Max=max(t35)))
summ36<-with(zz,data.frame(disease=disease[36],Total=sum(t36),Mean=mean(t36),SD=sd(t36),Median=median(t36),Min=min(t36),Max=max(t36)))
summ37<-with(zz,data.frame(disease=disease[37],Total=sum(t37),Mean=mean(t37),SD=sd(t37),Median=median(t37),Min=min(t37),Max=max(t37)))
summ38<-with(zz,data.frame(disease=disease[38],Total=sum(t38),Mean=mean(t38),SD=sd(t38),Median=median(t38),Min=min(t38),Max=max(t38)))
summ39<-with(zz,data.frame(disease=disease[39],Total=sum(t39),Mean=mean(t39),SD=sd(t39),Median=median(t39),Min=min(t39),Max=max(t39)))
summ40<-with(zz,data.frame(disease=disease[40],Total=sum(t40),Mean=mean(t40),SD=sd(t40),Median=median(t40),Min=min(t40),Max=max(t40)))
summ41<-with(zz,data.frame(disease=disease[41],Total=sum(t41),Mean=mean(t41),SD=sd(t41),Median=median(t41),Min=min(t41),Max=max(t41)))
summ42<-with(zz,data.frame(disease=disease[42],Total=sum(t42),Mean=mean(t42),SD=sd(t42),Median=median(t42),Min=min(t42),Max=max(t42)))
summ43<-with(zz,data.frame(disease=disease[43],Total=sum(t43),Mean=mean(t43),SD=sd(t43),Median=median(t43),Min=min(t43),Max=max(t43)))
summ44<-with(zz,data.frame(disease=disease[44],Total=sum(t44),Mean=mean(t44),SD=sd(t44),Median=median(t44),Min=min(t44),Max=max(t44)))
summ45<-with(zz,data.frame(disease=disease[45],Total=sum(t45),Mean=mean(t45),SD=sd(t45),Median=median(t45),Min=min(t45),Max=max(t45)))
summ46<-with(zz,data.frame(disease=disease[46],Total=sum(t46),Mean=mean(t46),SD=sd(t46),Median=median(t46),Min=min(t46),Max=max(t46)))
summ47<-with(zz,data.frame(disease=disease[47],Total=sum(t47),Mean=mean(t47),SD=sd(t47),Median=median(t47),Min=min(t47),Max=max(t47)))
summ48<-with(zz,data.frame(disease=disease[48],Total=sum(t48),Mean=mean(t48),SD=sd(t48),Median=median(t48),Min=min(t48),Max=max(t48)))
summ49<-with(zz,data.frame(disease=disease[49],Total=sum(t49),Mean=mean(t49),SD=sd(t49),Median=median(t49),Min=min(t49),Max=max(t49)))

summ_mor<-rbind(summ01,summ02,summ03,summ04,summ05,summ06,summ07,summ08,summ09,summ10,
                summ11,summ12,summ13,summ14,summ15,summ16,summ17,summ18,summ19,summ20,
                summ21,summ22,summ23,summ24,summ25,summ26,summ27,summ28,summ29,summ30,
                summ31,summ32,summ33,summ34,summ35,summ36,summ37,summ38,summ39,summ40,
                summ41,summ42,summ43,summ44,summ45,summ46,summ47,summ48,summ49)


write.csv(summ_mor,file="summ_mor_elderly.csv",row.names=F,na="")
