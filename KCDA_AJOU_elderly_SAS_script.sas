

/*자격 자료 MERGE*/
DATA A.BFC; SET RAW.BFC_2017-RAW.BFC_2020;
AGE  =STD_YYYY-BYEAR;
SIDO =SUBSTR(RVSN_ADDR_CD,1,2);
SGG  =RVSN_ADDR_CD;
IF AGE >=65 THEN AGE65=1; ELSE AGE65=0;
IF AGE>=65 & AGE<80 THEN AGE6579=1; ELSE AGE6579=0;
IF AGE>=80 THEN AGE80=1; ELSE AGE80=0;
PKEY=COMPRESS(STD_YYYY)||("-")||COMPRESS(INDI_DSCM_NO); 

KEEP STD_YYYY INDI_DSCM_NO SEX_TYPE BYEAR AGE AGE65 AGE6579 AGE80 SIDO SGG PKEY; RUN;

proc freq data=a.bfc; tables std_yyyy*sido/list; run;
proc freq data=a.bfc; tables std_yyyy*age/list; run;

proc freq data=a.bfc; tables std_yyyy*age65/list; run;
proc freq data=a.bfc; tables std_yyyy*age6579/list; run;
proc freq data=a.bfc; tables std_yyyy*age80/list; run;

/*연도별 65세 이상 자료, 시도 결측 없는 자료 만들기: 자격 존재 대상자*/
DATA A.BFC_REV; SET RAW.BFC_2017-RAW.BFC_2020;
AGE  =STD_YYYY-BYEAR;
SIDO =SUBSTR(RVSN_ADDR_CD,1,2);
SGG  =RVSN_ADDR_CD;
IF SIDO^="";
IF AGE>=65;
IF AGE >=65 THEN AGE65=1; ELSE AGE65=0;
IF AGE>=65 & AGE<80 THEN AGE6579=1; ELSE AGE6579=0;
IF AGE>=80 THEN AGE80=1; ELSE AGE80=0;
IF AGE<80 THEN AGE_GROUP=1; ELSE AGE_GROUP=2; 
PKEY=COMPRESS(STD_YYYY)||("-")||COMPRESS(INDI_DSCM_NO); 
KEEP STD_YYYY INDI_DSCM_NO SEX_TYPE BYEAR AGE AGE65 AGE6579 AGE80 AGE_GROUP SIDO SGG PKEY; RUN;

proc freq data=a.bfc_REV; tables std_yyyy*sido/list; run;
proc freq data=a.bfc_REV; tables std_yyyy*age/list; run;

proc freq data=a.bfc_REV; tables std_yyyy*age65/list; run;
proc freq data=a.bfc_REV; tables std_yyyy*age6579/list; run;
proc freq data=a.bfc_REV; tables std_yyyy*age80/list; run;

/*요양기관 자료 MERGE*/
DATA A.INST; SET RAW.INST_2017-RAW.INST_2020; RUN;

/*진료내역명세서(T20) MERGE*/
DATA A.T20_2017; SET RAW.T20_201701-RAW.T20_201712; 
KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1-SICK_SYM5 MDCARE_STRT_DT FORM_CD VSHSP_DD_CNT MDCARE_DD_CNT TOT_PRSC_DD_CNT FST_HSPTZ_DT; RUN;
DATA A.T20_2018; SET RAW.T20_201801-RAW.T20_201812; 
KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1-SICK_SYM5 MDCARE_STRT_DT FORM_CD VSHSP_DD_CNT MDCARE_DD_CNT TOT_PRSC_DD_CNT FST_HSPTZ_DT; RUN;
DATA A.T20_2019; SET RAW.T20_201901-RAW.T20_201912; 
KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1-SICK_SYM5 MDCARE_STRT_DT FORM_CD VSHSP_DD_CNT MDCARE_DD_CNT TOT_PRSC_DD_CNT FST_HSPTZ_DT; RUN;
DATA A.T20_2020; SET RAW.T20_202001-RAW.T20_202012; 
KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1-SICK_SYM5 MDCARE_STRT_DT FORM_CD VSHSP_DD_CNT MDCARE_DD_CNT TOT_PRSC_DD_CNT FST_HSPTZ_DT; RUN;

DATA A.T20; SET A.T20_2017-A.T20_2020; RUN;
/*진료 내역 명세서 의과서식 입원인 경우만 정리 (DATASET커서 미리 줄이기)*/
DATA A.T20_HP; SET A.T20; 
IF FORM_CD="02"; RUN;

/*연구 대상자(타겟)*/
DATA A.TG; SET RAW.TARGET_REQ202202210; RUN;

/*사망DB*/
DATA A.DTH; SET RAW.TG_DTH; RUN;

/****************************************************************************************************************************/
/****************************************************************************************************************************/
/*1단계 질환 코드 추출*/

/*DISEASE: 질환명
  S_CODE:연속적인 상병코드중 처음
  E_CODE:연속적인 상병코드 중 마지막 
  K:  상병코드 자릿수 K=3이면 3단 상병, 4이면 4단 상병*/

%MACRO DISEASE(DISEASE,S_CODE,E_CODE,K);
DATA A.&DISEASE.; SET A.T20_HP;

/*입원이면서 연속적인 질환 일 때*/
IF FORM_CD IN ("02") AND (&S_CODE. <=SUBSTR(SICK_SYM1,1,&K.) <=&E_CODE. OR 
                                      &S_CODE. <=SUBSTR(SICK_SYM2,1,&K.) <=&E_CODE.);

/*주 부상병 우선 순위 */
IF FORM_CD IN ("02") AND (&S_CODE. <=SUBSTR(SICK_SYM1,1,&K.) <=&E_CODE.)  THEN M_CODE=2; ELSE M_CODE=0;
IF FORM_CD IN ("02") AND (&S_CODE. <=SUBSTR(SICK_SYM2,1,&K.) <=&E_CODE.)  THEN S_CODE=1; ELSE S_CODE=0;

CODE_SCORE=M_CODE+S_CODE;
PKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4))||COMPRESS("-")||COMPRESS(INDI_DSCM_NO);
DROP M_CODE S_CODE; RUN;
%MEND;
/****************************************************************************************************************************/
/****************************************************************************************************************************/
/*연속적인 질환별로 추출 */
/*장감염 질환*/
%DISEASE(intestinal,"A00","A09",3);
%DISEASE(cholera,"A00","A00",3);
%DISEASE(typhoid,"A01","A01",3);
%DISEASE(othersalmone,"A02","A02",3);
%DISEASE(shigellosis,"A03","A03",3);
%DISEASE(otherbac,"A04","A04",3);
%DISEASE(otherbac_food,"A05","A05",3);
%DISEASE(amoebiasis,"A06","A06",3);
%DISEASE(otherprotozoa,"A07","A07",3);
%DISEASE(viral_intestinal,"A08","A08",3);
%DISEASE(othergast_coli,"A09","A09",3);

/*심혈관계 질환*/
%DISEASE(cvd,"I00","I99",3);
%DISEASE(angina,"I20","I20",3);
%DISEASE(IHD,"I20","I25",3);
%DISEASE(MI,"I21","I25",3);
%DISEASE(TIA,"G458","G459",4);

/*자살*/
%DISEASE(suicide,"X60","X84",3);

/*우울증*/
%DISEASE(mental,"F31","F39",3);

/*호흡기계*/
%DISEASE(resp,"J00","J99",3); 
%DISEASE(acuteup,"J00","J06",3); 
%DISEASE(influenza,"J09","J11",3); 
%DISEASE(pneum,"J09","J18",3); 
%DISEASE(ALRI,"J20","J22",3); 
%DISEASE(asthma,"J45","J46",3); 
%DISEASE(copd  ,"J40","J44",3); 
%DISEASE(vasomotor,"J30","J30",3); 

/*탈수*/
%DISEASE(voldep,"E86","E86",3); 
/*급성신손상*/
%DISEASE(aki ,"N17","N17",3); 

/*기후변화관련*/
%DISEASE(frost  ,"T33","T35",3); 
%DISEASE(heat,"T67","T67",3); 
%DISEASE(hypothermia ,"T68","T68",3); 
%DISEASE(othertemp,"T69","T69",3); 

/*당뇨*/
%DISEASE(dm,"E10","E14",3); 
%DISEASE(dm_type1,"E10","E10",3);
%DISEASE(dm_type2,"E11","E11",3); 

/*노인질환*/
%DISEASE(parkinson,"G20","G20",3);
%DISEASE(parkinsonism,"G20","G22",3);
%DISEASE(alzheimer,"G30","G30",3);
%DISEASE(dementia,"F00","F03",3);
%DISEASE(other_degenerative,"G30","G32",3);
%DISEASE(senility,"R54","R54",3);
%DISEASE(disorders_bone,"M80","M85",3);
%DISEASE(osteoporosis,"M80","M80",3);
/****************************************************************************************************************************/
/****************************************************************************************************************************/
/*연속적이지 않고 혼재되어있는 자료 */
/*전체 뇌졸중*/
DATA A.cerebvas; SET A.T20_HP;

/*입원이면서 연속적인 질환 일 때*/

IF FORM_CD IN ("02") AND (SUBSTR(SICK_SYM1,1,3) in ("I60","I61","I62","I63","I64","I65","I66","I67") OR 
                                      SUBSTR(SICK_SYM1,1,4) in ("I690","I691","I692","I693","I694","G458","G459") OR
                                      SUBSTR(SICK_SYM2,1,3) in ("I60","I61","I62","I63","I64","I65","I66","I67") OR 
                                      SUBSTR(SICK_SYM2,1,4) in ("I690","I691","I692","I693","I694","G458","G459"));

/*주 부상병 우선 순위 */
IF FORM_CD IN ("02") AND (SUBSTR(SICK_SYM1,1,3) in ("I60","I61","I62","I63","I64","I65","I66","I67") OR
                                      SUBSTR(SICK_SYM1,1,4) in ("I690","I691","I692","I693","I694","G458","G459"))  THEN M_CODE=2; ELSE M_CODE=0;
IF FORM_CD IN ("02") AND (SUBSTR(SICK_SYM2,1,3) in ("I60","I61","I62","I63","I64","I65","I66","I67") OR
                                      SUBSTR(SICK_SYM2,1,4) in ("I690","I691","I692","I693","I694","G458","G459")) THEN S_CODE=1; ELSE S_CODE=0;

CODE_SCORE=M_CODE+S_CODE;
PKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4))||COMPRESS("-")||COMPRESS(INDI_DSCM_NO);
DROP M_CODE S_CODE; RUN;

/*출혈성 뇌졸중 I60-I62, I690-I692*/
DATA A.hemoStroke; SET A.T20_HP;

IF FORM_CD IN ("02") AND (SUBSTR(SICK_SYM1,1,3) in ("I60","I61","I62") OR 
                                      SUBSTR(SICK_SYM1,1,4) in ("I690","I691","I692") OR
                                      SUBSTR(SICK_SYM2,1,3) in ("I60","I61","I62") OR 
                                      SUBSTR(SICK_SYM2,1,4) in ("I690","I691","I692"));

/*주 부상병 우선 순위 */
IF FORM_CD IN ("02") AND (SUBSTR(SICK_SYM1,1,3) in ("I60","I61","I62") OR
                                      SUBSTR(SICK_SYM1,1,4) in ("I690","I691","I692"))  THEN M_CODE=2; ELSE M_CODE=0;
IF FORM_CD IN ("02") AND (SUBSTR(SICK_SYM2,1,3) in ("I60","I61","I62") OR
                                      SUBSTR(SICK_SYM2,1,4) in ("I690","I691","I692")) THEN S_CODE=1; ELSE S_CODE=0;

CODE_SCORE=M_CODE+S_CODE;
PKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4))||COMPRESS("-")||COMPRESS(INDI_DSCM_NO);
DROP M_CODE S_CODE; RUN;

/*허혈성 뇌졸중 I63, I65-I67, I693*/
DATA A.ischStroke; SET A.T20_HP;

IF FORM_CD IN ("02") AND (SUBSTR(SICK_SYM1,1,3) in ("I63","I65","I66","I67") OR 
                                      SUBSTR(SICK_SYM1,1,4) in ("I693") OR
                                      SUBSTR(SICK_SYM2,1,3) in ("I63","I65","I66","I67") OR 
                                      SUBSTR(SICK_SYM2,1,4) in ("I693"));

/*주 부상병 우선 순위 */
IF FORM_CD IN ("02") AND (SUBSTR(SICK_SYM1,1,3) in ("I63","I65","I66","I67") OR
                                      SUBSTR(SICK_SYM1,1,4) in ("I693"))  THEN M_CODE=2; ELSE M_CODE=0;
IF FORM_CD IN ("02") AND (SUBSTR(SICK_SYM2,1,3) in ("I63","I65","I66","I67") OR
                                      SUBSTR(SICK_SYM2,1,4) in ("I693")) THEN S_CODE=1; ELSE S_CODE=0;

CODE_SCORE=M_CODE+S_CODE;
PKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4))||COMPRESS("-")||COMPRESS(INDI_DSCM_NO);
DROP M_CODE S_CODE; RUN;

/*기타 뇌졸중 I64, I694*/ 
DATA A.otherStroke; SET A.T20_HP;

IF FORM_CD IN ("02") AND (SUBSTR(SICK_SYM1,1,3) in ("I64") OR 
                                      SUBSTR(SICK_SYM1,1,4) in ("I694") OR
                                      SUBSTR(SICK_SYM2,1,3) in ("I64") OR 
                                      SUBSTR(SICK_SYM2,1,4) in ("I694"));

/*주 부상병 우선 순위 */
IF FORM_CD IN ("02") AND (SUBSTR(SICK_SYM1,1,3) in ("I64") OR
                                      SUBSTR(SICK_SYM1,1,4) in ("I694"))  THEN M_CODE=2; ELSE M_CODE=0;
IF FORM_CD IN ("02") AND (SUBSTR(SICK_SYM2,1,3) in ("I64") OR
                                      SUBSTR(SICK_SYM2,1,4) in ("I694")) THEN S_CODE=1; ELSE S_CODE=0;

CODE_SCORE=M_CODE+S_CODE;
PKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4))||COMPRESS("-")||COMPRESS(INDI_DSCM_NO);
DROP M_CODE S_CODE; RUN;
/****************************************************************************************************************************/
/****************************************************************************************************************************/
DATA A.CVD2; SET A.CVD; IF CODE_SCORE^=1; RUN;
DATA A.angina2; SET A.angina; IF CODE_SCORE^=1; RUN;
DATA A.IHD2; SET A.IHD; IF CODE_SCORE^=1; RUN;
DATA A.MI2; SET A.MI; IF CODE_SCORE^=1; RUN;
DATA A.TIA2; SET A.TIA; IF CODE_SCORE^=1; RUN;
DATA A.suicide2; SET A.suicide; IF CODE_SCORE^=1; RUN;
DATA A.mental2; SET A.mental; IF CODE_SCORE^=1; RUN;
DATA A.resp2; SET A.resp; IF CODE_SCORE^=1; RUN;
DATA A.acuteup2; SET A.acuteup; IF CODE_SCORE^=1; RUN;
DATA A.influenza2; SET A.influenza; IF CODE_SCORE^=1; RUN;
DATA A.pneum2; SET A.pneum; IF CODE_SCORE^=1; RUN;
DATA A.ALRI2; SET A.ALRI; IF CODE_SCORE^=1; RUN;
DATA A.asthma2; SET A.asthma; IF CODE_SCORE^=1; RUN;
DATA A.copd2; SET A.copd; IF CODE_SCORE^=1; RUN;
DATA A.vasomotor2; SET A.vasomotor; IF CODE_SCORE^=1; RUN;
DATA A.voldep2; SET A.voldep; IF CODE_SCORE^=1; RUN;
DATA A.aki2; SET A.aki; IF CODE_SCORE^=1; RUN;
DATA A.frost2; SET A.frost; IF CODE_SCORE^=1; RUN;
DATA A.heat2; SET A.heat; IF CODE_SCORE^=1; RUN;
DATA A.hypothermia2; SET A.hypothermia; IF CODE_SCORE^=1; RUN;
DATA A.othertemp2; SET A.othertemp; IF CODE_SCORE^=1; RUN;
DATA A.dm2; SET A.dm; IF CODE_SCORE^=1; RUN;
DATA A.dm2_type1; SET A.dm_type1; IF CODE_SCORE^=1; RUN;
DATA A.dm2_type2; SET A.dm_type2; IF CODE_SCORE^=1; RUN;
DATA A.parkinson2; SET A.parkinson; IF CODE_SCORE^=1; RUN;
DATA A.parkinsonism2; SET A.parkinsonism; IF CODE_SCORE^=1; RUN;
DATA A.alzheimer2; SET A.alzheimer; IF CODE_SCORE^=1; RUN;
DATA A.dementia2; SET A.dementia; IF CODE_SCORE^=1; RUN;
DATA A.other_degenerative2; SET A.other_degenerative; IF CODE_SCORE^=1; RUN;
DATA A.disorders_bone2; SET A.disorders_bone; IF CODE_SCORE^=1; RUN;
DATA A.osteoporosis2; SET A.osteoporosis; IF CODE_SCORE^=1; RUN;
DATA A.cerebvas2; SET A.cerebvas; IF CODE_SCORE^=1; RUN;
DATA A.hemoStroke2 ;SET A.hemoStroke; IF CODE_SCORE^=1; RUN;
DATA A.ischStroke2 ;SET A.ischStroke; IF CODE_SCORE^=1; RUN;
DATA A.otherStroke2 ;SET A.otherStroke; IF CODE_SCORE^=1; RUN;
DATA A.senility2 ;SET A.senility; IF CODE_SCORE^=1; RUN;
/****************************************************************************************************************************/
/****************************************************************************************************************************/

/*날짜 만들어주기 */
DATA A.DDATE;
FORMAT DATE YYMMDD10.;
DO I= 1 TO 1461 BY 1;
DATE=MDY(01,01,2017)+i-1;  /*2017~2020 날짜 생성*/
OUTPUT;
END; 
DROP I;
RUN;

DATA A.DDATE ; SET A.DDATE;
DATE2=PUT(DATE,YYMMDDN8.); RUN;

/*시도랑 날짜 연계*/
PROC SORT DATA=A.BFC_REV OUT=A.SIDO NODUPKEY ; BY SIDO; RUN;

DATA A.SIDO; SET A.SIDO; KEEP SIDO; RUN;

/*시도랑 날짜 연계*/
PROC SQL; CREATE TABLE A.SIDODATE AS SELECT * FROM A.DDATE cross join A.sido; quit;
data A.sidodate; set A.sidodate; key=compress(sido)||("-")||compress(date2); keep key; run;

DATA Z.NULL_TB2;
INPUT CATEGROY $4. Y_2017 Y_2018 Y_2019 Y_2020;

CARDS;
. "" "" "" "" ""
;
RUN;

/****************************************************************************************************************************/
/****************************************************************************************************************************/
/*2단계: 데이터 클리닝 및  필요변수 정리*/
%MACRO dailycount(DISEASE,epi);
DATA Z.D1; SET A.&DISEASE.;
IF INDI_DSCM_NO="" THEN DELETE; /*ID 무효 제거 */
/*유효 진료 일자*/
IF "2017" <=SUBSTR(MDCARE_STRT_DT,1,4) <="2020" AND 
   "01"   <=SUBSTR(MDCARE_STRT_DT,5,2) <="12"   AND
   "01"   <=SUBSTR(MDCARE_STRT_DT,7,2) <="31" ;

 /*유효 최초 진료 일자*/
IF "1899" <=SUBSTR(LEFT(FST_HSPTZ_DT),1,4) <="2020" AND
   "01"   <=SUBSTR(LEFT(FST_HSPTZ_DT),5,2) <="12"   AND
   "01"   <=SUBSTR(LEFT(FST_HSPTZ_DT),7,2) <="31"   THEN FST_HSPTZ_DT=FST_HSPTZ_DT; ELSE FST_HSPTZ_DT="";

/*유효 입내원 일수*/
IF VSHSP_DD_CNT="" THEN DELETE;

/*입내원 일수 0 인 경우(입원뒤 검진받을시) ->1로 코딩*/
IF VSHSP_DD_CNT=0 THEN VSHSP_DD_CNT=1;

/*날짜 부여 (연월일)*/
YEAR    =SUBSTR(MDCARE_STRT_DT,1,4);
MONTH=SUBSTR(MDCARE_STRT_DT,5,2);
DAY    =SUBSTR(MDCARE_STRT_DT,7,2);

/*날짜 변경 */
MDCARE =MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
FST    =MDY(SUBSTR(FST_HSPTZ_DT,5,2)  ,SUBSTR(FST_HSPTZ_DT,7,2)  ,SUBSTR(FST_HSPTZ_DT,1,4));
IF FST^=" " THEN FST_STATUS=1; ELSE FST_STATUS=0;

/*진료개시일자 계산*/
IF MDCARE=" " THEN MDCARE_DATE=FST; ELSE MDCARE_DATE=MDCARE;
IF FST^  =" " THEN FST_DATE=FST   ; ELSE FST_DATE=MDCARE;

/*진료 개시일자*/
DATE1=MIN(FST_DATE,MDCARE_DATE); 
/*최초 진료 받은일-입원일*/
DIFF_PLUS=MDCARE_DATE-DATE1;
/*요양일 계산*/
CNT_DD=DIFF_PLUS+VSHSP_DD_CNT; 
/*진료일+개인 아이디 (중복일 제거 목적 만든 key)*/
DKEY=COMPRESS(MDCARE_DATE) || COMPRESS("-") || COMPRESS(INDI_DSCM_NO);
FORMAT MDCARE FST MDCARE_DATE FST_DATE DATE1 YYMMDD10.;
RUN;

/***************************************************************************/
/*3단계: 데이터 클리닝 및  필요변수 정리*/
/*질환 추출 단계  ->  주부상병 SCORE 추가 */
/*청구 건수가 연달아 발생하면 하나의 발생으로 간주하여 계산*/
/*(1) 동일 환자의 청구 건당 날짜가 같으면 가장 긴 입내원 일수만 keep*/
/*(2) 만약 청구 건당 날짜와 가장 긴 입내원 일수가 같다면 주상병인 것만 keep*/
/*(3) 만약 청구 건당 날짜와 가장 긴 입내원 일수가 같고 주상병이 같으면 그중 하나만 keep*/

PROC SORT DATA=Z.D1 ; by INDI_DSCM_NO MDCARE_DATE CNT_DD SICK_SYM1; RUN ; /*데이터 정렬*/

/*데이터 정렬 : 1) 날짜+개인식별 아이디  2) 입내원 일수 내림차순  3) 주+부상병순위 내림차순 (CODE_SCORE 변수)*/
PROC SORT DATA=Z.D1; BY DKEY DESCENDING CNT_DD DESCENDING CODE_SCORE; RUN;

/*위 정렬 데이터 중 날짜+개별 아이디 기준으로 첫행이 아니면 제외*/
DATA Z.D2; SET Z.D1; 
BY DKEY; 
IF FIRST.DKEY^=1 THEN DELETE ;
DROP DKEY CODE_SCORE; RUN;

proc sort data=Z.d2; by indi_dscm_no mdcare_date ; run;

/****************************************************************************************************************/
/*4단계: 에피소드 정리*/
DATA Z.D3;
FORMAT R START_DATE DATE1_DISCHARGE YYMMDD10.; 
RETAIN R D START_DATE;  SET Z.D2;

BY INDI_DSCM_NO ; 

IF FIRST.INDI_DSCM_NO=1 AND 
   LAST.INDI_DSCM_NO=1 THEN DO;

IKEEP=1; 
R=DATE1+CNT_DD-1;
D=CNT_DD;
START_DATE=DATE1; END; ELSE DO;

IF FIRST.INDI_DSCM_NO  =1 AND
   LAST.INDI_DSCM_NO ^=1 THEN DO;

IKEEP=1;
R=DATE1+CNT_DD-1;
D=CNT_DD;
START_DATE=DATE1; END; ELSE DO;

K=DATE1-R; /*연속된 입내원 일수 계산, 마지막 퇴원일 - 최초 입원 날짜 일*/
IF K<=&EPI. THEN DO; IKEEP=0;
IF DATE1+CNT_DD-1 < R THEN D=D; ELSE DO;
R=DATE1+CNT_DD-1; 
D=R-START_DATE+1;
END; END; ELSE DO;
IKEEP=1; 
R=DATE1+CNT_DD-1;
D=CNT_DD;
START_DATE=DATE1;
END; END; END;
DATE1_DISCHARGE=DATE1+CNT_DD-1; RUN;

/*개인식별아이디 기준 정렬 , MDCARE DATE 내림차순*/
PROC SORT DATA = Z.D3; BY INDI_DSCM_NO DESCENDING MDCARE_DATE DESCENDING CNT_DD; RUN ;

/*PUT MAX VALAUE FOR IKEEP=0*/
DATA Z.D4;
FORMAT DISCHARGEDATE YYMMDD10.; RETAIN MAXD; 
SET Z.D3;

BY INDI_DSCM_NO;
IF FIRST.INDI_DSCM_NO=1 AND IKEEP=0 THEN MAXD=D; ELSE DO;
IF FIRST.INDI_DSCM_NO=1 AND IKEEP=1 THEN MAXD=0;

ELSE DO; IF IKEEP=0 THEN DO; MAXD=MAX(D,MAXD);
END; ELSE DO; MAXD=0; 
END; END; END;

/*keep the first events record information with the total inpatient days from the last event*/
ikeep2=lag(IKEEP);
IF FIRST.INDI_DSCM_NO=1 THEN ILOGKEEP=2; ELSE DO;
IF IKEEP2=0 THEN ILOGKEEP=1; 
ELSE ILOGKEEP=2; END; D2=LAG(MAXD);
IF IKEEP=1 AND ILOGKEEP=2 THEN D=CNT_DD; ELSE DO;
IF IKEEP=1 AND ILOGKEEP=1 THEN D=D2; END;
IF IKEEP2="." THEN IKEEP2=1; /*첫행  lag로 빠지는 부분 채워주기*/
IF IKEEP2=1;
DISCHARGEDATE=START_DATE+D-1;
DROP R MAXD IKEEP  ILOGKEEP D2 K CNT_DD D DATE1_DISCHARGE ; RUN;


/*5단계 자격   MERGE*/
/*정리후 자격  merge */
PROC SQL; CREATE TABLE Z.D5 AS SELECT * FROM Z.d4 as a LEFT JOIN A.BFC_REV as b on a.pkey=b.pkey; quit;

/*6단계   자격 MERGE 후 시도 없거나 최초입원일 날짜가 2017년 미만인경우 삭제*/
DATA Z.D6 ; SET Z.D5;

DATE=PUT(START_DATE,YYMMDDN8.);
IF SUBSTR(DATE,1,4)>=2017;
IF SIDO^=" ";

/*성별 더미*/
IF SEX_TYPE=1 THEN M=1; ELSE M=0;
IF SEX_TYPE=2 THEN F=1; ELSE F=0;

IF SEX_TYPE=1 & AGE6579=1 THEN M_AGE6579=1; ELSE M_AGE6579=0;
IF SEX_TYPE=2 & AGE6579=1 THEN F_AGE6579=1; ELSE F_AGE6579=0;

IF SEX_TYPE=1 & AGE80=1 THEN M_AGE80=1; ELSE M_AGE80=0;
IF SEX_TYPE=2 & AGE80=1 THEN F_AGE80=1; ELSE F_AGE80=0;
RUN;

/*데일리 카운트 예시 */
PROC SQL; CREATE TABLE Z.DAILYCOUNT AS SELECT START_DATE AS DATE, 
SIDO, COUNT(START_DATE) AS TOT, SUM(AGE6579) AS AGE6579 ,SUM(AGE80) AS AGE80, 
SUM(M) AS M, SUM(F) AS F,
SUM(M_AGE6579) AS M_AGE6579, SUM(F_AGE6579) AS F_AGE6579,
SUM(M_AGE80) AS M_AGE80,        SUM(F_AGE80) AS F_AGE80
FROM Z.D6 GROUP BY START_DATE , SIDO; QUIT;


DATA Z.DAILYCOUNT; SET Z.DAILYCOUNT;
DATE2=PUT(DATE,YYMMDDN8.);
KEY=COMPRESS(SIDO)||("-")||COMPRESS(DATE2);
DROP DATE2;
RUN;

/*날짜 결측 채워주기 위해  merge 후 빈칸 채움*/
PROC SQL; CREATE TABLE Z.DAILYCOUNT AS SELECT * FROM A.SIDODATE AS A LEFT JOIN Z.DAILYCOUNT AS B ON A.KEY =B.KEY; QUIT;

data Z.&DISEASE._dailycount; set Z.dailycount;
format date yymmdd10.;
rename tot=&DISEASE._tot age6579=&DISEASE._age6579 age80=&DISEASE._age80
m=&DISEASE._m f=&DISEASE._f 
M_age6579=&DISEASE._M_age6579 F_age6579=&DISEASE._F_age6579
M_age80  =&DISEASE._M_age80     F_age80   =&DISEASE._F_age80
;
date=mdy(substr(key,8,2),substr(key,10,2),substr(key,4,4));
year=substr(key,4,4);
month=substr(key,8,2);
sido=substr(key,1,2);

if tot="." then tot=0;
if age6579="." then age6579=0;
if age80="." then age80=0;
if M="." then M=0;
if F="." then F=0;
IF M_age6579="." THEN M_age6579=0;
IF F_age6579="." THEN F_age6579=0;
IF M_age80="." THEN M_age80=0;
IF F_age80="." THEN F_age80=0;
RUN;

/*전체, 성별*/
proc sql; create table c1 as select year, sum(&DISEASE._tot) as cnt from Z.&DISEASE._dailycount group by year; quit;
proc sql; create table c2 as select year, sum(&DISEASE._m) as cnt from Z.&DISEASE._dailycount group by year; quit;
proc sql; create table c3 as select year, sum(&DISEASE._f) as cnt from Z.&DISEASE._dailycount group by year; quit;

proc transpose data=c1 prefix=y_ out=t_t1; id year; run;
proc transpose data=c2 prefix=y_ out=t_t2; id year; run;
proc transpose data=c3 prefix=y_ out=t_t3; id year; run;

/*성별*/
data t_t1; set t_t1; rename _name_=category; _name_="total"; run;
data t_t2; set t_t2; rename _name_=category; _name_="male"; run;
data t_t3; set t_t3; rename _name_=category; _name_="female"; run;

/*시도별*/
proc sql; create table c4 as select year,sido, sum(&DISEASE._tot) as cnt from Z.&DISEASE._dailycount group by year, sido; quit;
proc sort data=c4; by sido; run;
proc transpose data=c4 prefix=y_ out=t_t4;
by sido; id year; run;
data t_t4; set t_t4; rename sido=category; drop _name_; run;

/*월별*/
proc sql; create table c5 as select year,month, sum(&DISEASE._tot) as cnt from Z.&DISEASE._dailycount group by year, month; quit;
proc sort data=c5; by month; run;
proc transpose data=c5 prefix=y_ out=t_t5;
by month; id year; run;
data t_t5; set t_t5; rename month=category; drop _name_; run;

/*연령별 */
PROC SQL; CREATE TABLE Z.z AS SELECT START_DATE AS DATE, 
age_group, COUNT(START_DATE) AS TOT FROM z.D6 GROUP BY START_DATE , age_group; QUIT;

data Z.z2; set Z.z;
date2=put(date,yymmddn8.);
year=substr(date2,1,4); run;

/*연령 누락한거 테이블 추가  */
proc sql; create table c6 as select year,age_group, sum(tot) as cnt from Z.z2 group by year,age_group ; quit;
proc sort data=c6; by age_group; run;
proc transpose data=c6 prefix=y_ out=t_t6;
by age_group; id year; run;
data t_t6; set t_t6; rename sido=category; drop _name_; run;

/*전체, 성별,연령별, 시도 월별*/
data Z.&DISEASE._count_freq; set t_t1 z.null_tb2 t_t2 t_t3 z.null_tb2 Z.null_tb2 t_t4 z.null_tb2 t_t5 z.null_tb2 t_t6; run;

/*단계별 데이터  n수*/
proc sql; create table step0 as select count(*) as n from A.&DISEASE.; quit;
proc sql; create table step1 as select count(*) as n from Z.d1; quit;
proc sql; create table step2 as select count(*) as n from Z.d2; quit;
proc sql; create table step3 as select count(*) as n from Z.d3; quit;
proc sql; create table step4 as select count(*) as n from Z.d4; quit;
proc sql; create table step5 as select count(*) as n from Z.d5; quit;
proc sql; create table step6 as select count(*) as n from Z.d6; quit;

data step0; set step0; step="질환자료" ; label="빈도" ; run;
data step1; set step1; step="1단계" ; label="빈도" ; run;
data step2; set step2; step="2단계" ; label="빈도" ; run;
data step3; set step3; step="3단계" ; label="빈도" ; run;
data step4; set step4; step="4단계" ; label="빈도" ; run;
data step5; set step5; step="5단계" ; label="빈도" ; run;
data step6; set step6; step="6단계" ; label="빈도" ; run;

data Z.&DISEASE._step; set step0-step6; run;

/*고유 환자수 */
proc sql; create table ID0 as select count(DISTINCT(INDI_DSCM_NO)) as n from Z.&DISEASE.; quit;
proc sql; create table ID1 as select count(DISTINCT(INDI_DSCM_NO)) as n from Z.d1; quit;
proc sql; create table ID2 as select count(DISTINCT(INDI_DSCM_NO)) as n from Z.d2; quit;
proc sql; create table ID3 as select count(DISTINCT(INDI_DSCM_NO)) as n from Z.d3; quit;
proc sql; create table ID4 as select count(DISTINCT(INDI_DSCM_NO)) as n from Z.d4; quit;
proc sql; create table ID5 as select count(DISTINCT(INDI_DSCM_NO)) as n from Z.d5; quit;
proc sql; create table ID6 as select count(DISTINCT(INDI_DSCM_NO)) as n from Z.d6; quit;

data ID0; set ID0; step="질환자료" ; label="환자수" ; run;
data ID1; set ID1; step="1단계" ; label="환자수" ; run;
data ID2; set ID2; step="2단계" ; label="환자수" ; run;
data ID3; set ID3; step="3단계" ; label="환자수" ; run;
data ID4; set ID4; step="4단계" ; label="환자수" ; run;
data ID5; set ID5; step="5단계" ; label="환자수" ; run;
data ID6; set ID6; step="6단계" ; label="환자수" ; run;

data Z.&DISEASE._ID; set id0-id6; run;
%MEND;
/*입원: 주/부상병, 에피소드30*/
%dailycount(intestinal,30)
%dailycount(cholera,30)
%dailycount(typhoid,30)
%dailycount(othersalmone,30)
%dailycount(shigellosis,30)
%dailycount(otherbac,30)
%dailycount(otherbac_food,30)
%dailycount(amoebiasis,30)
%dailycount(otherprotozoa,30)
%dailycount(viral_intestinal,30)
%dailycount(othergast_coli,30)

/*입원: 주, 에피소드7*/
%dailycount(cvd2,7);         
%dailycount(angina2,7)
%dailycount(IHD2,7)
%dailycount(MI2,7)
%dailycount(TIA2,7)
%dailycount(suicide2,7)
%dailycount(mental2,7)
%dailycount(resp2,7)
%dailycount(acuteup2,7)
%dailycount(influenza2,7)
%dailycount(pneum2,7)
%dailycount(ALRI2,7)
%dailycount(asthma2,7)
%dailycount(copd2,7)
%dailycount(vasomotor2,7)
%dailycount(voldep2,7)
%dailycount(aki2,7)
%dailycount(frost2,7)
%dailycount(heat2,7)
%dailycount(hypothermia2,7)
%dailycount(othertemp2,7)
%dailycount(dm2,7)
%dailycount(dm2_type1,7)
%dailycount(dm2_type2,7)
%dailycount(parkinson2,7)
%dailycount(parkinsonism2,7)
%dailycount(alzheimer2,7)
%dailycount(dementia2,7)
%dailycount(other_degenerative2,7)
%dailycount(disorders_bone2,7)
%dailycount(osteoporosis2,7)
%dailycount(cerebvas2,7)
%dailycount(hemoStroke2 ,7)
%dailycount(ischStroke2 ,7)
%dailycount(otherStroke2 ,7)
%dailycount(senility2 ,7)
