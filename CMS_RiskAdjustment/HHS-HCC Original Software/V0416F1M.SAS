 %macro V0416F1M(INP=, IND=, OUTDATA=, 
                 IDVAR=, KEEPVAR=,
                 CCFMT=, AGEFMT=, SEXFMT=,
                 AGESEXMAC=AGESEXV6, I9EMACRO=I9V04ED1, I0EMACRO=I0V04ED1,
                 LABELMAC=V04127L1,  HIERMAC=V04127H1,  SCOREMAC=SCOREV3);

 %**********************************************************************
 * V0416F1M creates HHS HCCs and score variables for each enrollee
 * in the user-provided PERSON SAS dataset.
 *
 * If an enrollee has at least one diagnosis in the DIAG SAS dataset
 * (provided by the user) then HHS HCC variables are created, otherwise HHS HCCs
 * are set to 0 for the enrollee.
 *
 * Score variables are created using calibration coefficients from three
 * models (Adult, Child, Infant), each with five `metal` levels
 * (Platinum, Gold, Silver, Bronze, Catastrophic).
 *
 * Both input SAS datasets, PERSON and DIAG, must be sorted by person ID, i.e.,
 * the user must ensure that each dataset is sorted by the common identifier
 * named in macro variable &IDVAR.
 *
 * The PERSON SAS dataset must contain variables as noted in the comments for V0416F1P.
 *
 * The DIAG SAS dataset must contain variables as noted in the comments for V0416F1P.
 *
 * If an enrollee has N diagnoses, s/he will have N records in the DIAG dataset. If an
 *  enrollee has no diagnoses, s/he will have zero records in the DIAG dataset.
 *
 * User-provided parameters:
 *  INP      - SAS input PERSON SAS dataset
 *  IND      - SAS input DIAGnosis SAS dataset
 *  OUTDATA  - SAS output SAS dataset
 *  IDVAR    - name of enrollee ID variable (e.g., Medicare HICNO)
 *  KEEPVAR  - variables in output dataset in addition to &IDVAR
 *  CCFMT    - pointer to SAS formats that crosswalk ICD9/10s to HHS CCs
 *  AGEFMT   - pointer to SAS formats that crosswalk ICD9/10s to acceptable 
 *             age range in case age-related edits are to be performed
 *  SEXFMT   - pointer to SAS format that crosswalks ICD9/10s to acceptable 
 *             sex in case sex-related edits are to be performed
 *
 * Internal parameters (pointers to external macros):
 *  AGESEXMAC - external macro name: creates age/sex variables
 *  I9EMACRO  - external macro name: performs ICD9 diagnosis edits
 *  I0EMACRO  - external macro name: performs ICD10 diagnosis edits
 *  LABELMAC  - external macro name: assigns labels to HHS HCCs
 *  HIERMAC   - external macro name: applies HHS HCC hierarchy rules
 *  SCOREMAC  - external macro name: calculates score variables
 *
 * External macros for this release:
 *  AGESEXV6    - (named in AGESEXMAC) creates age/sex variables
 *  I9V04ED1    - (named in I9EMACRO ) performs ICD9 diagnosis edits
 *  I0V04ED1    - (named in I0EMACRO ) performs ICD10 diagnosis edits
 *  V04127L1    - (named in LABELMAC ) assigns labels to HHS HCCs
 *  V04127H1    - (named in HIERMAC  ) applies HHS HCC hierarchy rules
 *  SCOREV3     - (named in SCOREMAC ) calculates score variables
 *
 **********************************************************************;

**===========================================================================**;
** include external macros                                                   **;
**===========================================================================**;

 ** included files are assumed to have upper case filenames, and extension **;
 ** .SAS to accommodate all computing platforms, e.g., V04127L1.SAS        **;

 ** creates age/sex variables **;
 %if "&AGESEXMAC" ne "" %then %do;
  %inc IN0("&AGESEXMAC..SAS") / source2;
 %end;

 ** performs ICD9 diagnosis edits **;
 %if "&I9EMACRO" ne "" %then %do;
  %inc IN0("&I9EMACRO..SAS") / source2;
 %end;

 ** performs ICD10 diagnosis edits **;
 %if "&I0EMACRO" ne "" %then %do;
  %inc IN0("&I0EMACRO..SAS") / source2;
 %end;

 ** assigns HHS HCC labels **;
 %if "&LABELMAC" ne "" %then %do;
  %inc IN0("&LABELMAC..SAS") / source2;
 %end;

 ** applies hierarchy rules **;
 %if "&HIERMAC" ne "" %then %do;
  %inc IN0("&HIERMAC..SAS") / source2;
 %end;

 ** calculates score variables **;
 %if "&SCOREMAC" ne "" %then %do;
  %inc IN0("&SCOREMAC..SAS") / source2;
 %end;

**==========================================================================================**;
** AGESEXV6 goes here when testing macros inline;
**==========================================================================================**;

**==========================================================================================**;
** I9V04ED1 goes here when testing macros inline;
**==========================================================================================**;

**==========================================================================================**;
** I0V04ED1 goes here when testing macros inline;
**==========================================================================================**;

**==========================================================================================**;
** V04127L1 goes here when testing macros inline;
**==========================================================================================**;

**==========================================================================================**;
** V04127H1 goes here when testing macros inline;
**==========================================================================================**;

**==========================================================================================**;
** SCOREV3 goes here when testing macros inline;
**==========================================================================================**;

**===========================================================================**;
** define internal macro variables, formats, macros                          **;
**===========================================================================**;

**===========================================================================**;
** internal macro variables                                                  **;
**===========================================================================**;
** max number of HHS HCCs for sizing arrays, = payment + non-payment HCCs    **;
%let N_CC=266;

**===========================================================================**;
** internal formats                                                          **;
**===========================================================================**;

**===========================================================================**;
** if enrollee has any of these ICD9 diags and certain HHS_HCCs, it might    **;
** indicate a bundled claim -- infant diagnoses (147)                        **;
proc format;
 value $B9I
  "76401","76411","76421","76491","76501","76511","V2131","76402","76412","76422",
  "76492","76502","76512","76521","76522","76403","76413","76423","76493","76503",
  "76513","V2132","76523","76404","76405","76414","76415","76424","76425","76494",
  "76495","76504","76505","76514","76515","V2133","76524","76406","76407","76416",
  "76417","76426","76427","76496","76497","76506","76507","76516","76517","V2134",
  "76526","76525","76527","76408","76418","76428","76498","76508","76518","V2135",
  "V34"  ,"V340" ,"V3400","V3401","V341" ,"V342" ,"V35"  ,"V350" ,"V3500","V3501",
  "V351" ,"V352" ,"V36"  ,"V360" ,"V3600","V3601","V361" ,"V362" ,"V37"  ,"V370" ,
  "V3700","V3701","V371" ,"V372" ,"7594" ,"764"  ,"7640" ,"76400","76409","7641" ,
  "76410","76419","7642" ,"76420","76429","7649" ,"76490","76499","765"  ,"7650" ,
  "76500","76509","7651" ,"76510","76519","V213" ,"V2130","76528","V31"  ,"V310" ,
  "V3100","V3101","V311" ,"V312" ,"V32"  ,"V320" ,"V3200","V3201","V321" ,"V322" ,
  "V33"  ,"V330" ,"V3300","V3301","V331" ,"V332" ,"7652" ,"76520","76529","766"  ,
  "7660" ,"7661" ,"7662" ,"76621","76622","V30"  ,"V300" ,"V3000","V3001","V301" ,
  "V302" ,"V39"  ,"V390" ,"V3900","V3901","V391" ,"V392" 
  = "B9I"; run;

**===========================================================================**;
** if enrollee has any of these ICD10 diags and certain HHS_HCCs, it might   **;
** indicate a bundled claim -- infant diagnoses (70)                         **;
proc format;
 value $B10I
  "P0500","P0501","P0502","P0503","P0504","P0505","P0506","P0507","P0508","P0510",
  "P0511","P0512","P0513","P0514","P0515","P0516","P0517","P0518","P052" ,"P059" ,
  "P0700","P0701","P0702","P0703","P0710","P0714","P0715","P0716","P0717","P0718",
  "P0720","P0721","P0722","P0723","P0724","P0725","P0726","P0730","P0731","P0732",
  "P0733","P0734","P0735","P0736","P0737","P0738","P0739","P080" ,"P081" ,"P0821",
  "P0822","Q894" ,"Z3800","Z3801","Z381" ,"Z382" ,"Z3830","Z3831","Z384" ,"Z385" ,
  "Z3861","Z3862","Z3863","Z3864","Z3865","Z3866","Z3868","Z3869","Z387" ,"Z388"
  = "B10I"; run;

**===========================================================================**;
** if enrollee has any of these ICD9 diags and certain HHS_HCCs, it might    **;
** indicate a bundled claim -- mother diagnoses (617)                        **;
proc format;
 value $B9A
  "63430","63431","63432","63450","63451","63452","63460","63461","63462","6393" ,
  "6395" ,"6396" ,"64001","64081","64091","64101","64111","64120","64121","64131",
  "64181","64191","64201","64202","64204","64211","64212","64214","64221","64222",
  "64224","64231","64232","64234","64241","64242","64244","64250","64251","64252",
  "64254","64260","64261","64262","64264","64271","64272","64274","64291","64292",
  "64294","64301","64311","64321","64381","64391","64420","64421","64511","64521",
  "64601","64611","64612","64614","64621","64622","64624","64631","64641","64642",
  "64644","64651","64652","64654","64661","64662","64664","64671","64681","64682",
  "64684","64691","64701","64702","64704","64711","64712","64714","64721","64722",
  "64724","64731","64732","64734","64741","64742","64744","64751","64752","64754",
  "64761","64762","64764","64781","64782","64784","64791","64792","64794","64801",
  "64802","64804","64811","64812","64814","64821","64822","64824","64831","64832",
  "64834","64841","64842","64844","64851","64852","64854","64861","64862","64864",
  "64871","64872","64874","64881","64882","64884","64891","64892","64894","64901",
  "64902","64904","64911","64912","64914","64921","64922","64924","64931","64932",
  "64934","64941","64942","64944","64951","64961","64962","64964","64971","64981",
  "64982","650"  ,"65101","65111","65121","65131","65141","65151","65161","65171",
  "65181","65191","65201","65211","65221","65231","65241","65251","65261","65271",
  "65281","65291","65300","65301","65310","65311","65320","65321","65330","65331",
  "65340","65341","65350","65351","65360","65361","65370","65371","65380","65381",
  "65390","65391","65400","65401","65402","65404","65410","65411","65412","65414",
  "65420","65421","65430","65431","65432","65434","65440","65441","65442","65444",
  "65451","65452","65454","65460","65461","65462","65464","65470","65471","65472",
  "65474","65480","65481","65482","65484","65490","65491","65492","65494","65501",
  "65511","65521","65531","65541","65551","65561","65571","65581","65591","65601",
  "65611","65621","65631","65641","65651","65661","65671","65681","65691","65701",
  "65801","65810","65811","65820","65821","65830","65831","65840","65841","65880",
  "65881","65890","65891","65900","65901","65910","65911","65920","65921","65930",
  "65931","65940","65941","65950","65951","65960","65961","65970","65971","65980",
  "65981","65990","65991","66000","66001","66010","66011","66020","66021","66030",
  "66031","66040","66041","66050","66051","66060","66061","66070","66071","66080",
  "66081","66090","66091","66101","66111","66121","66131","66141","66191","66200",
  "66201","66210","66211","66220","66221","66230","66231","66233","66300","66301",
  "66310","66311","66320","66321","66330","66331","66340","66341","66350","66351",
  "66360","66361","66380","66381","66390","66391","66400","66401","66404","66410",
  "66411","66414","66420","66421","66424","66430","66431","66434","66440","66441",
  "66444","66450","66451","66454","66460","66461","66464","66480","66481","66484",
  "66490","66491","66494","66500","66501","66510","66511","66520","66522","66524",
  "66530","66531","66534","66540","66541","66544","66550","66551","66554","66560",
  "66561","66564","66570","66571","66572","66574","66580","66581","66582","66584",
  "66590","66591","66592","66594","66600","66602","66604","66610","66612","66614",
  "66620","66622","66624","66630","66632","66634","66700","66702","66704","66710",
  "66712","66714","66800","66801","66802","66804","66810","66811","66812","66814",
  "66820","66821","66822","66824","66880","66881","66882","66884","66890","66891",
  "66892","66894","66900","66901","66902","66904","66910","66911","66912","66914",
  "66920","66921","66922","66924","66930","66932","66934","66940","66941","66942",
  "66944","66950","66951","66960","66961","66970","66971","66980","66981","66982",
  "66984","66990","66991","66992","66994","67000","67002","67004","67010","67012",
  "67014","67020","67022","67024","67030","67032","67034","67080","67082","67084",
  "67101","67102","67104","67111","67112","67114","67121","67122","67124","67131",
  "67140","67142","67144","67151","67152","67154","67181","67182","67184","67191",
  "67192","67194","67202","67204","67301","67302","67304","67311","67312","67314",
  "67321","67322","67324","67331","67332","67334","67381","67382","67384","67400",
  "67401","67402","67404","67410","67412","67414","67420","67422","67424","67430",
  "67432","67434","67440","67442","67444","67450","67451","67452","67454","67480",
  "67482","67484","67490","67492","67494","67501","67502","67504","67511","67512",
  "67514","67521","67522","67524","67581","67582","67584","67591","67592","67594",
  "67601","67602","67604","67611","67612","67614","67621","67622","67624","67631",
  "67632","67634","67641","67642","67644","67651","67652","67654","67661","67662",
  "67664","67681","67682","67684","67691","67692","67694","67801","67811","67901",
  "67902","67904","67911","67912","67914","V240" ,"V241" ,"V242" ,"V270" ,"V271" ,
  "V272", "V273" ,"V274" ,"V275" ,"V276" ,"V277" ,"V279"
  = "B9A"; run;

**===========================================================================**;
** if enrollee has any of these ICD10 diags and certain HHS_HCCs, it might   **;
** indicate a bundled claim -- mother diagnoses (468)                        **;
proc format;
 value $B10A
  "O032"   ,"O0331"  ,"O0332"  ,"O0336"  ,"O037"   ,"O0381"  ,"O0382"  ,"O0386"  ,"O082"   ,"O083"   ,
  "O084"   ,"O0881"  ,"O1002"  ,"O1003"  ,"O1012"  ,"O1013"  ,"O1022"  ,"O1023"  ,"O1032"  ,"O1033"  ,
  "O1042"  ,"O1043"  ,"O1092"  ,"O1093"  ,"O151"   ,"O152"   ,"O159"   ,"O2402"  ,"O2403"  ,"O2412"  ,
  "O2413"  ,"O2432"  ,"O2433"  ,"O24420" ,"O24424" ,"O24429" ,"O24430" ,"O24434" ,"O24439" ,"O2482"  ,
  "O2483"  ,"O2492"  ,"O2493"  ,"O252"   ,"O253"   ,"O2662"  ,"O2663"  ,"O2672"  ,"O2673"  ,"O4202"  ,
  "O4212"  ,"O4292"  ,"O6010X0","O6010X1","O6010X2","O6010X3","O6010X4","O6010X5","O6010X9","O6012X0",
  "O6012X1","O6012X2","O6012X3","O6012X4","O6012X5","O6012X9","O6013X0","O6013X1","O6013X2","O6013X3",
  "O6013X4","O6013X5","O6013X9","O6014X0","O6014X1","O6014X2","O6014X3","O6014X4","O6014X5","O6014X9",
  "O6020X0","O6020X1","O6020X2","O6020X3","O6020X4","O6020X5","O6020X9","O6022X0","O6022X1","O6022X2",
  "O6022X3","O6022X4","O6022X5","O6022X9","O6023X0","O6023X1","O6023X2","O6023X3","O6023X4","O6023X5",
  "O6023X9","O610"   ,"O611"   ,"O618"   ,"O619"   ,"O620"   ,"O621"   ,"O622"   ,"O623"   ,"O624"   ,
  "O628"   ,"O629"   ,"O630"   ,"O631"   ,"O632"   ,"O639"   ,"O640XX0","O640XX1","O640XX2","O640XX3",
  "O640XX4","O640XX5","O640XX9","O641XX0","O641XX1","O641XX2","O641XX3","O641XX4","O641XX5","O641XX9",
  "O642XX0","O642XX1","O642XX2","O642XX3","O642XX4","O642XX5","O642XX9","O643XX0","O643XX1","O643XX2",
  "O643XX3","O643XX4","O643XX5","O643XX9","O644XX0","O644XX1","O644XX2","O644XX3","O644XX4","O644XX5",
  "O644XX9","O645XX0","O645XX1","O645XX2","O645XX3","O645XX4","O645XX5","O645XX9","O648XX0","O648XX1",
  "O648XX2","O648XX3","O648XX4","O648XX5","O648XX9","O649XX0","O649XX1","O649XX2","O649XX3","O649XX4",
  "O649XX5","O649XX9","O650"   ,"O651"   ,"O652"   ,"O653"   ,"O654"   ,"O655"   ,"O658"   ,"O659"   ,
  "O660"   ,"O661"   ,"O662"   ,"O663"   ,"O6640"  ,"O6641"  ,"O665"   ,"O666"   ,"O668"   ,"O669"   ,
  "O670"   ,"O678"   ,"O679"   ,"O68"    ,"O690XX0","O690XX1","O690XX2","O690XX3","O690XX4","O690XX5",
  "O690XX9","O691XX0","O691XX1","O691XX2","O691XX3","O691XX4","O691XX5","O691XX9","O692XX0","O692XX1",
  "O692XX2","O692XX3","O692XX4","O692XX5","O692XX9","O693XX0","O693XX1","O693XX2","O693XX3","O693XX4",
  "O693XX5","O693XX9","O694XX0","O694XX1","O694XX2","O694XX3","O694XX4","O694XX5","O694XX9","O695XX0",
  "O695XX1","O695XX2","O695XX3","O695XX4","O695XX5","O695XX9","O6981X0","O6981X1","O6981X2","O6981X3",
  "O6981X4","O6981X5","O6981X9","O6982X0","O6982X1","O6982X2","O6982X3","O6982X4","O6982X5","O6982X9",
  "O6989X0","O6989X1","O6989X2","O6989X3","O6989X4","O6989X5","O6989X9","O699XX0","O699XX1","O699XX2",
  "O699XX3","O699XX4","O699XX5","O699XX9","O700"   ,"O701"   ,"O702"   ,"O703"   ,"O704"   ,"O709"   ,
  "O711"   ,"O712"   ,"O713"   ,"O714"   ,"O715"   ,"O716"   ,"O717"   ,"O7181"  ,"O7182"  ,"O7189"  ,
  "O719"   ,"O720"   ,"O721"   ,"O722"   ,"O723"   ,"O730"   ,"O731"   ,"O740"   ,"O741"   ,"O742"   ,
  "O743"   ,"O744"   ,"O745"   ,"O746"   ,"O747"   ,"O748"   ,"O749"   ,"O750"   ,"O751"   ,"O752"   ,
  "O753"   ,"O754"   ,"O755"   ,"O7581"  ,"O7582"  ,"O7589"  ,"O759"   ,"O76"    ,"O770"   ,"O771"   ,
  "O778"   ,"O779"   ,"O80"    ,"O82"    ,"O85"    ,"O860"   ,"O8611"  ,"O8612"  ,"O8613"  ,"O8619"  ,
  "O8620"  ,"O8621"  ,"O8622"  ,"O8629"  ,"O864"   ,"O8681"  ,"O8689"  ,"O870"   ,"O871"   ,"O872"   ,
  "O873"   ,"O874"   ,"O878"   ,"O879"   ,"O8802"  ,"O8803"  ,"O8812"  ,"O8813"  ,"O8822"  ,"O8823"  ,
  "O8832"  ,"O8833"  ,"O8882"  ,"O8883"  ,"O8901"  ,"O8909"  ,"O891"   ,"O892"   ,"O893"   ,"O894"   ,
  "O895"   ,"O896"   ,"O898"   ,"O899"   ,"O900"   ,"O901"   ,"O902"   ,"O903"   ,"O904"   ,"O905"   ,
  "O906"   ,"O9081"  ,"O9089"  ,"O909"   ,"O9102"  ,"O9103"  ,"O9112"  ,"O9113"  ,"O9122"  ,"O9123"  ,
  "O9202"  ,"O9203"  ,"O9212"  ,"O9213"  ,"O9220"  ,"O9229"  ,"O923"   ,"O924"   ,"O925"   ,"O926"   ,
  "O9270"  ,"O9279"  ,"O9802"  ,"O9803"  ,"O9812"  ,"O9813"  ,"O9822"  ,"O9823"  ,"O9832"  ,"O9833"  ,
  "O9842"  ,"O9843"  ,"O9852"  ,"O9853"  ,"O9862"  ,"O9863"  ,"O9872"  ,"O9873"  ,"O9882"  ,"O9883"  ,
  "O9892"  ,"O9893"  ,"O9902"  ,"O9903"  ,"O9912"  ,"O9913"  ,"O99214" ,"O99215" ,"O99284" ,"O99285" ,
  "O99314" ,"O99315" ,"O99324" ,"O99325" ,"O99334" ,"O99335" ,"O99344" ,"O99345" ,"O99354" ,"O99355" ,
  "O9942"  ,"O9943"  ,"O9952"  ,"O9953"  ,"O9962"  ,"O9963"  ,"O9972"  ,"O9973"  ,"O99814" ,"O99815" ,
  "O99824" ,"O99825" ,"O99834" ,"O99835" ,"O99844" ,"O99845" ,"O9A12"  ,"O9A13"  ,"O9A22"  ,"O9A23"  ,
  "O9A32"  ,"O9A33"  ,"O9A42"  ,"O9A43"  ,"O9A52"  ,"O9A53"  ,"Z370"   ,"Z371"   ,"Z372"   ,"Z373"   ,
  "Z374"   ,"Z3750"  ,"Z3751"  ,"Z3752"  ,"Z3753"  ,"Z3754"  ,"Z3759"  ,"Z3760"  ,"Z3761"  ,"Z3762"  ,
  "Z3763"  ,"Z3764"  ,"Z3769"  ,"Z377"   ,"Z379"   ,"Z390"   ,"Z391"   ,"Z392"
  = "B10A"; run;

**===========================================================================**;
** internal macros                                                           **;
**===========================================================================**;

**===========================================================================**;
** age-sex variables list (32)                                               **;
%macro AGE_SEX_LIST;
MAGE_LAST_0_0   MAGE_LAST_1_1   MAGE_LAST_2_4   MAGE_LAST_5_9   MAGE_LAST_10_14
MAGE_LAST_15_20 MAGE_LAST_21_24 MAGE_LAST_25_29 MAGE_LAST_30_34 MAGE_LAST_35_39
MAGE_LAST_40_44 MAGE_LAST_45_49 MAGE_LAST_50_54 MAGE_LAST_55_59 MAGE_LAST_60_GT
FAGE_LAST_0_0   FAGE_LAST_1_1   FAGE_LAST_2_4   FAGE_LAST_5_9   FAGE_LAST_10_14
FAGE_LAST_15_20 FAGE_LAST_21_24 FAGE_LAST_25_29 FAGE_LAST_30_34 FAGE_LAST_35_39
FAGE_LAST_40_44 FAGE_LAST_45_49 FAGE_LAST_50_54 FAGE_LAST_55_59 FAGE_LAST_60_GT
Age0_Male 
Age1_Male
%mend AGE_SEX_LIST;

**===========================================================================**;
** HHS HCCs, payment model list (127)                                        **;
%macro HHS_HCC_PM_LIST;
HHS_HCC001 HHS_HCC002 HHS_HCC003 HHS_HCC004 HHS_HCC006 HHS_HCC008 HHS_HCC009
HHS_HCC010 HHS_HCC011 HHS_HCC012 HHS_HCC013 HHS_HCC018 HHS_HCC019 HHS_HCC020
HHS_HCC021 HHS_HCC023 HHS_HCC026 HHS_HCC027 HHS_HCC028 HHS_HCC029 HHS_HCC030
HHS_HCC034 HHS_HCC035 HHS_HCC036 HHS_HCC037 HHS_HCC038 HHS_HCC041 HHS_HCC042
HHS_HCC045 HHS_HCC046 HHS_HCC047 HHS_HCC048 HHS_HCC054 HHS_HCC055 HHS_HCC056
HHS_HCC057 HHS_HCC061 HHS_HCC062 HHS_HCC063 HHS_HCC064 HHS_HCC066 HHS_HCC067
HHS_HCC068 HHS_HCC069 HHS_HCC070 HHS_HCC071 HHS_HCC073 HHS_HCC074 HHS_HCC075
HHS_HCC081 HHS_HCC082 HHS_HCC087 HHS_HCC088 HHS_HCC089 HHS_HCC090 HHS_HCC094
HHS_HCC096 HHS_HCC097 HHS_HCC102 HHS_HCC103 HHS_HCC106 HHS_HCC107 HHS_HCC108
HHS_HCC109 HHS_HCC110 HHS_HCC111 HHS_HCC112 HHS_HCC113 HHS_HCC114 HHS_HCC115
HHS_HCC117 HHS_HCC118 HHS_HCC119 HHS_HCC120 HHS_HCC121 HHS_HCC122 HHS_HCC125
HHS_HCC126 HHS_HCC127 HHS_HCC128 HHS_HCC129 HHS_HCC130 HHS_HCC131 HHS_HCC132
HHS_HCC135 HHS_HCC137 HHS_HCC138 HHS_HCC139 HHS_HCC142 HHS_HCC145 HHS_HCC146
HHS_HCC149 HHS_HCC150 HHS_HCC151 HHS_HCC153 HHS_HCC154 HHS_HCC156 HHS_HCC158
HHS_HCC159 HHS_HCC160 HHS_HCC161 HHS_HCC162 HHS_HCC163 HHS_HCC183 HHS_HCC184
HHS_HCC187 HHS_HCC188 HHS_HCC203 HHS_HCC204 HHS_HCC205 HHS_HCC207 HHS_HCC208
HHS_HCC209 HHS_HCC217 HHS_HCC226 HHS_HCC227 HHS_HCC242 HHS_HCC243 HHS_HCC244
HHS_HCC245 HHS_HCC246 HHS_HCC247 HHS_HCC248 HHS_HCC249 HHS_HCC251 HHS_HCC253
HHS_HCC254
%mend HHS_HCC_PM_LIST;

**===========================================================================**;
** HCCs, payment model list, note numbering difference vs. HHS HCCs (127)    **;
%macro HCC_PM_LIST;
HCC1   HCC2   HCC3   HCC4   HCC6   HCC8   HCC9   HCC10  HCC11  HCC12
HCC13  HCC18  HCC19  HCC20  HCC21  HCC23  HCC26  HCC27  HCC28  HCC29
HCC30  HCC34  HCC35  HCC36  HCC37  HCC38  HCC41  HCC42  HCC45  HCC46
HCC47  HCC48  HCC54  HCC55  HCC56  HCC57  HCC61  HCC62  HCC63  HCC64
HCC66  HCC67  HCC68  HCC69  HCC70  HCC71  HCC73  HCC74  HCC75  HCC81
HCC82  HCC87  HCC88  HCC89  HCC90  HCC94  HCC96  HCC97  HCC102 HCC103
HCC106 HCC107 HCC108 HCC109 HCC110 HCC111 HCC112 HCC113 HCC114 HCC115
HCC117 HCC118 HCC119 HCC120 HCC121 HCC122 HCC125 HCC126 HCC127 HCC128
HCC129 HCC130 HCC131 HCC132 HCC135 HCC137 HCC138 HCC139 HCC142 HCC145
HCC146 HCC149 HCC150 HCC151 HCC153 HCC154 HCC156 HCC158 HCC159 HCC160
HCC161 HCC162 HCC163 HCC183 HCC184 HCC187 HCC188 HCC203 HCC204 HCC205
HCC207 HCC208 HCC209 HCC217 HCC226 HCC227 HCC242 HCC243 HCC244 HCC245
HCC246 HCC247 HCC248 HCC249 HCC251 HCC253 HCC254
%mend HCC_PM_LIST;

**===========================================================================**;
** HHS CCs, payment model list (127)                                         **;
%macro HHS_CC_PM_LIST;
HHS_CC001 HHS_CC002 HHS_CC003 HHS_CC004 HHS_CC006 HHS_CC008 HHS_CC009
HHS_CC010 HHS_CC011 HHS_CC012 HHS_CC013 HHS_CC018 HHS_CC019 HHS_CC020
HHS_CC021 HHS_CC023 HHS_CC026 HHS_CC027 HHS_CC028 HHS_CC029 HHS_CC030
HHS_CC034 HHS_CC035 HHS_CC036 HHS_CC037 HHS_CC038 HHS_CC041 HHS_CC042
HHS_CC045 HHS_CC046 HHS_CC047 HHS_CC048 HHS_CC054 HHS_CC055 HHS_CC056
HHS_CC057 HHS_CC061 HHS_CC062 HHS_CC063 HHS_CC064 HHS_CC066 HHS_CC067
HHS_CC068 HHS_CC069 HHS_CC070 HHS_CC071 HHS_CC073 HHS_CC074 HHS_CC075
HHS_CC081 HHS_CC082 HHS_CC087 HHS_CC088 HHS_CC089 HHS_CC090 HHS_CC094
HHS_CC096 HHS_CC097 HHS_CC102 HHS_CC103 HHS_CC106 HHS_CC107 HHS_CC108
HHS_CC109 HHS_CC110 HHS_CC111 HHS_CC112 HHS_CC113 HHS_CC114 HHS_CC115
HHS_CC117 HHS_CC118 HHS_CC119 HHS_CC120 HHS_CC121 HHS_CC122 HHS_CC125
HHS_CC126 HHS_CC127 HHS_CC128 HHS_CC129 HHS_CC130 HHS_CC131 HHS_CC132
HHS_CC135 HHS_CC137 HHS_CC138 HHS_CC139 HHS_CC142 HHS_CC145 HHS_CC146
HHS_CC149 HHS_CC150 HHS_CC151 HHS_CC153 HHS_CC154 HHS_CC156 HHS_CC158
HHS_CC159 HHS_CC160 HHS_CC161 HHS_CC162 HHS_CC163 HHS_CC183 HHS_CC184
HHS_CC187 HHS_CC188 HHS_CC203 HHS_CC204 HHS_CC205 HHS_CC207 HHS_CC208
HHS_CC209 HHS_CC217 HHS_CC226 HHS_CC227 HHS_CC242 HHS_CC243 HHS_CC244
HHS_CC245 HHS_CC246 HHS_CC247 HHS_CC248 HHS_CC249 HHS_CC251 HHS_CC253
HHS_CC254
%mend HHS_CC_PM_LIST;

**===========================================================================**;
** CCs, payment model list, note numbering difference vs. HHS CCs (127)      **;
%macro CC_PM_LIST;
CC1   CC2   CC3   CC4   CC6   CC8   CC9   CC10  CC11  CC12
CC13  CC18  CC19  CC20  CC21  CC23  CC26  CC27  CC28  CC29
CC30  CC34  CC35  CC36  CC37  CC38  CC41  CC42  CC45  CC46
CC47  CC48  CC54  CC55  CC56  CC57  CC61  CC62  CC63  CC64
CC66  CC67  CC68  CC69  CC70  CC71  CC73  CC74  CC75  CC81
CC82  CC87  CC88  CC89  CC90  CC94  CC96  CC97  CC102 CC103
CC106 CC107 CC108 CC109 CC110 CC111 CC112 CC113 CC114 CC115
CC117 CC118 CC119 CC120 CC121 CC122 CC125 CC126 CC127 CC128
CC129 CC130 CC131 CC132 CC135 CC137 CC138 CC139 CC142 CC145
CC146 CC149 CC150 CC151 CC153 CC154 CC156 CC158 CC159 CC160
CC161 CC162 CC163 CC183 CC184 CC187 CC188 CC203 CC204 CC205
CC207 CC208 CC209 CC217 CC226 CC227 CC242 CC243 CC244 CC245
CC246 CC247 CC248 CC249 CC251 CC253 CC254
%mend CC_PM_LIST;

**===========================================================================**;
** HCCs, all, note numbering difference vs. HHS HCCs (264)                   **;
%macro HCC_FULL_LIST;
HCC1-HCC&N_CC
%mend HCC_FULL_LIST;

**===========================================================================**;
** CCs, all, note numbering difference vs. HHS CCs (264)                     **;
%macro CC_FULL_LIST;
CC1-CC&N_CC
%mend CC_FULL_LIST;

**===========================================================================**;
** ADULT model HCC groups list, note 02A, no 05 (17)                         **;
%macro ADULT_GROUPS_LIST;
G01 G02A G03 G04 G06 G07 G08 G09 G10 G11 G12 G13 G14 G15 G16 G17 G18
%mend ADULT_GROUPS_LIST;

**===========================================================================**;
** CHILD model HCC groups list, note no 05 (17)                              **;
%macro CHILD_GROUPS_LIST;
G01 G02  G03 G04 G06 G07 G08 G09 G10 G11 G12 G13 G14 G15 G16 G17 G18
%mend CHILD_GROUPS_LIST;

**===========================================================================**;
** INFANT model HCC groups list, placeholder, NONE at this time (0)          **;
%macro INFANT_GROUPS_LIST;
%mend INFANT_GROUPS_LIST;

**===========================================================================**;
** ADULT variables list, 127 + 18 + 17 + 2 = (164)                           **;
%macro ADULT_VARIABLES_LIST;
%HHS_HCC_PM_LIST
FAGE_LAST_21_24 FAGE_LAST_25_29 FAGE_LAST_30_34 FAGE_LAST_35_39
FAGE_LAST_40_44 FAGE_LAST_45_49 FAGE_LAST_50_54 FAGE_LAST_55_59
FAGE_LAST_60_GT
MAGE_LAST_21_24 MAGE_LAST_25_29 MAGE_LAST_30_34 MAGE_LAST_35_39
MAGE_LAST_40_44 MAGE_LAST_45_49 MAGE_LAST_50_54 MAGE_LAST_55_59
MAGE_LAST_60_GT
%ADULT_GROUPS_LIST
INT_GROUP_H
INT_GROUP_M
%mend ADULT_VARIABLES_LIST;

**===========================================================================**;
** CHILD variables list, 127 + 8 + 17 = (152)                                **;
%macro CHILD_VARIABLES_LIST;
%HHS_HCC_PM_LIST
FAGE_LAST_2_4 FAGE_LAST_5_9 FAGE_LAST_10_14 FAGE_LAST_15_20
MAGE_LAST_2_4 MAGE_LAST_5_9 MAGE_LAST_10_14 MAGE_LAST_15_20
%CHILD_GROUPS_LIST
%mend CHILD_VARIABLES_LIST;

**===========================================================================**;
** INFANT vars list (27)                                                     **;
%macro INFANT_VARIABLES_LIST;
AGE0_MALE AGE1_MALE
%SEV_MAT_INTERACTIONS_LIST
%mend INFANT_VARIABLES_LIST;

**===========================================================================**;
** ADULT coeff list, parameter is prefix, e.g., AP for adult platinum  (164) **;
%macro ADULT_COEFFICIENTS_LIST(P);
&P.HHS_HCC001 &P.HHS_HCC002 &P.HHS_HCC003 &P.HHS_HCC004 &P.HHS_HCC006
&P.HHS_HCC008 &P.HHS_HCC009 &P.HHS_HCC010 &P.HHS_HCC011 &P.HHS_HCC012
&P.HHS_HCC013 &P.HHS_HCC018 &P.HHS_HCC019 &P.HHS_HCC020 &P.HHS_HCC021
&P.HHS_HCC023 &P.HHS_HCC026 &P.HHS_HCC027 &P.HHS_HCC028 &P.HHS_HCC029
&P.HHS_HCC030 &P.HHS_HCC034 &P.HHS_HCC035 &P.HHS_HCC036 &P.HHS_HCC037
&P.HHS_HCC038 &P.HHS_HCC041 &P.HHS_HCC042 &P.HHS_HCC045 &P.HHS_HCC046
&P.HHS_HCC047 &P.HHS_HCC048 &P.HHS_HCC054 &P.HHS_HCC055 &P.HHS_HCC056
&P.HHS_HCC057 &P.HHS_HCC061 &P.HHS_HCC062 &P.HHS_HCC063 &P.HHS_HCC064
&P.HHS_HCC066 &P.HHS_HCC067 &P.HHS_HCC068 &P.HHS_HCC069 &P.HHS_HCC070
&P.HHS_HCC071 &P.HHS_HCC073 &P.HHS_HCC074 &P.HHS_HCC075 &P.HHS_HCC081
&P.HHS_HCC082 &P.HHS_HCC087 &P.HHS_HCC088 &P.HHS_HCC089 &P.HHS_HCC090
&P.HHS_HCC094 &P.HHS_HCC096 &P.HHS_HCC097 &P.HHS_HCC102 &P.HHS_HCC103
&P.HHS_HCC106 &P.HHS_HCC107 &P.HHS_HCC108 &P.HHS_HCC109 &P.HHS_HCC110
&P.HHS_HCC111 &P.HHS_HCC112 &P.HHS_HCC113 &P.HHS_HCC114 &P.HHS_HCC115
&P.HHS_HCC117 &P.HHS_HCC118 &P.HHS_HCC119 &P.HHS_HCC120 &P.HHS_HCC121
&P.HHS_HCC122 &P.HHS_HCC125 &P.HHS_HCC126 &P.HHS_HCC127 &P.HHS_HCC128
&P.HHS_HCC129 &P.HHS_HCC130 &P.HHS_HCC131 &P.HHS_HCC132 &P.HHS_HCC135
&P.HHS_HCC137 &P.HHS_HCC138 &P.HHS_HCC139 &P.HHS_HCC142 &P.HHS_HCC145
&P.HHS_HCC146 &P.HHS_HCC149 &P.HHS_HCC150 &P.HHS_HCC151 &P.HHS_HCC153
&P.HHS_HCC154 &P.HHS_HCC156 &P.HHS_HCC158 &P.HHS_HCC159 &P.HHS_HCC160
&P.HHS_HCC161 &P.HHS_HCC162 &P.HHS_HCC163 &P.HHS_HCC183 &P.HHS_HCC184
&P.HHS_HCC187 &P.HHS_HCC188 &P.HHS_HCC203 &P.HHS_HCC204 &P.HHS_HCC205
&P.HHS_HCC207 &P.HHS_HCC208 &P.HHS_HCC209 &P.HHS_HCC217 &P.HHS_HCC226
&P.HHS_HCC227 &P.HHS_HCC242 &P.HHS_HCC243 &P.HHS_HCC244 &P.HHS_HCC245
&P.HHS_HCC246 &P.HHS_HCC247 &P.HHS_HCC248 &P.HHS_HCC249 &P.HHS_HCC251
&P.HHS_HCC253 &P.HHS_HCC254
&P.FAGE_LAST_21_24 &P.FAGE_LAST_25_29 &P.FAGE_LAST_30_34 &P.FAGE_LAST_35_39
&P.FAGE_LAST_40_44 &P.FAGE_LAST_45_49 &P.FAGE_LAST_50_54 &P.FAGE_LAST_55_59
&P.FAGE_LAST_60_GT
&P.MAGE_LAST_21_24 &P.MAGE_LAST_25_29 &P.MAGE_LAST_30_34 &P.MAGE_LAST_35_39
&P.MAGE_LAST_40_44 &P.MAGE_LAST_45_49 &P.MAGE_LAST_50_54 &P.MAGE_LAST_55_59
&P.MAGE_LAST_60_GT
&P.G01 &P.G02A &P.G03 &P.G04 &P.G06 &P.G07 &P.G08 &P.G09 &P.G10 &P.G11 &P.G12
&P.G13 &P.G14  &P.G15 &P.G16 &P.G17 &P.G18
&P.INT_GROUP_H &P.INT_GROUP_M
%mend ADULT_COEFFICIENTS_LIST;

**===========================================================================**;
** CHILD coeff list, parameter is prefix, e.g., CG for child gold (152)      **;
%macro CHILD_COEFFICIENTS_LIST(P);
&P.HHS_HCC001 &P.HHS_HCC002 &P.HHS_HCC003 &P.HHS_HCC004 &P.HHS_HCC006 
&P.HHS_HCC008 &P.HHS_HCC009 &P.HHS_HCC010 &P.HHS_HCC011 &P.HHS_HCC012 
&P.HHS_HCC013 &P.HHS_HCC018 &P.HHS_HCC019 &P.HHS_HCC020 &P.HHS_HCC021 
&P.HHS_HCC023 &P.HHS_HCC026 &P.HHS_HCC027 &P.HHS_HCC028 &P.HHS_HCC029 
&P.HHS_HCC030 &P.HHS_HCC034 &P.HHS_HCC035 &P.HHS_HCC036 &P.HHS_HCC037 
&P.HHS_HCC038 &P.HHS_HCC041 &P.HHS_HCC042 &P.HHS_HCC045 &P.HHS_HCC046 
&P.HHS_HCC047 &P.HHS_HCC048 &P.HHS_HCC054 &P.HHS_HCC055 &P.HHS_HCC056 
&P.HHS_HCC057 &P.HHS_HCC061 &P.HHS_HCC062 &P.HHS_HCC063 &P.HHS_HCC064 
&P.HHS_HCC066 &P.HHS_HCC067 &P.HHS_HCC068 &P.HHS_HCC069 &P.HHS_HCC070 
&P.HHS_HCC071 &P.HHS_HCC073 &P.HHS_HCC074 &P.HHS_HCC075 &P.HHS_HCC081 
&P.HHS_HCC082 &P.HHS_HCC087 &P.HHS_HCC088 &P.HHS_HCC089 &P.HHS_HCC090 
&P.HHS_HCC094 &P.HHS_HCC096 &P.HHS_HCC097 &P.HHS_HCC102 &P.HHS_HCC103 
&P.HHS_HCC106 &P.HHS_HCC107 &P.HHS_HCC108 &P.HHS_HCC109 &P.HHS_HCC110 
&P.HHS_HCC111 &P.HHS_HCC112 &P.HHS_HCC113 &P.HHS_HCC114 &P.HHS_HCC115 
&P.HHS_HCC117 &P.HHS_HCC118 &P.HHS_HCC119 &P.HHS_HCC120 &P.HHS_HCC121 
&P.HHS_HCC122 &P.HHS_HCC125 &P.HHS_HCC126 &P.HHS_HCC127 &P.HHS_HCC128 
&P.HHS_HCC129 &P.HHS_HCC130 &P.HHS_HCC131 &P.HHS_HCC132 &P.HHS_HCC135 
&P.HHS_HCC137 &P.HHS_HCC138 &P.HHS_HCC139 &P.HHS_HCC142 &P.HHS_HCC145 
&P.HHS_HCC146 &P.HHS_HCC149 &P.HHS_HCC150 &P.HHS_HCC151 &P.HHS_HCC153 
&P.HHS_HCC154 &P.HHS_HCC156 &P.HHS_HCC158 &P.HHS_HCC159 &P.HHS_HCC160 
&P.HHS_HCC161 &P.HHS_HCC162 &P.HHS_HCC163 &P.HHS_HCC183 &P.HHS_HCC184 
&P.HHS_HCC187 &P.HHS_HCC188 &P.HHS_HCC203 &P.HHS_HCC204 &P.HHS_HCC205 
&P.HHS_HCC207 &P.HHS_HCC208 &P.HHS_HCC209 &P.HHS_HCC217 &P.HHS_HCC226 
&P.HHS_HCC227 &P.HHS_HCC242 &P.HHS_HCC243 &P.HHS_HCC244 &P.HHS_HCC245 
&P.HHS_HCC246 &P.HHS_HCC247 &P.HHS_HCC248 &P.HHS_HCC249 &P.HHS_HCC251 
&P.HHS_HCC253 &P.HHS_HCC254 
&P.FAGE_LAST_2_4 &P.FAGE_LAST_5_9 &P.FAGE_LAST_10_14 &P.FAGE_LAST_15_20 
&P.MAGE_LAST_2_4 &P.MAGE_LAST_5_9 &P.MAGE_LAST_10_14 &P.MAGE_LAST_15_20 
&P.G01 &P.G02 &P.G03 &P.G04 &P.G06 &P.G07 &P.G08 &P.G09 &P.G10 &P.G11 
&P.G12 &P.G13 &P.G14 &P.G15 &P.G16 &P.G17 &P.G18 
%mend CHILD_COEFFICIENTS_LIST;

**===========================================================================**;
** INFANT coeff list, parameter is prefix, e.g., IS for infant silver (27)   **;
** some are abbreviated to comply with variable name length restrictions     **;
** e.g., IBPREMATURE_MULTIPLES_X_SEVERITY1 is longer than 32                 **;
%macro INFANT_COEFFICIENTS_LIST(P);
&P.AGE0_MALE                      &P.AGE1_MALE 
&P.AGE1_X_SEVERITY1               &P.AGE1_X_SEVERITY2 
&P.AGE1_X_SEVERITY3               &P.AGE1_X_SEVERITY4 
&P.AGE1_X_SEVERITY5 
&P.EMATURE_MULTIPLES_X_SEVERITY1  &P.EMATURE_MULTIPLES_X_SEVERITY2 
&P.EMATURE_MULTIPLES_X_SEVERITY3  &P.EMATURE_MULTIPLES_X_SEVERITY4 
&P.EMATURE_MULTIPLES_X_SEVERITY5 
&P.EXTREMELY_IMMATURE_X_SEVERITY1 &P.EXTREMELY_IMMATURE_X_SEVERITY2 
&P.EXTREMELY_IMMATURE_X_SEVERITY3 &P.EXTREMELY_IMMATURE_X_SEVERITY4 
&P.EXTREMELY_IMMATURE_X_SEVERITY5 
&P.IMMATURE_X_SEVERITY1           &P.IMMATURE_X_SEVERITY2 
&P.IMMATURE_X_SEVERITY3           &P.IMMATURE_X_SEVERITY4 
&P.IMMATURE_X_SEVERITY5 
&P.TERM_X_SEVERITY1               &P.TERM_X_SEVERITY2 
&P.TERM_X_SEVERITY3               &P.TERM_X_SEVERITY4 
&P.TERM_X_SEVERITY5 
%mend INFANT_COEFFICIENTS_LIST;

**===========================================================================**;
** ADULT severe illness list (8)                                             **;
%macro SEVERE_V3_LIST;
HHS_HCC002
HHS_HCC042
HHS_HCC120
HHS_HCC122
HHS_HCC125
HHS_HCC126
HHS_HCC127
HHS_HCC156
%mend SEVERE_V3_LIST;

**===========================================================================**;
** ADULT severe illness interaction H list (9)                               **;
%macro INT_GROUP_H_LIST;
SEVERE_V3_x_HHS_HCC006 
SEVERE_V3_x_HHS_HCC008 
SEVERE_V3_x_HHS_HCC009
SEVERE_V3_x_HHS_HCC010 
SEVERE_V3_x_HHS_HCC115 
SEVERE_V3_x_HHS_HCC135 
SEVERE_V3_x_HHS_HCC145 
SEVERE_V3_x_G06 
SEVERE_V3_x_G08 
%mend INT_GROUP_H_LIST;

**===========================================================================**;
** ADULT severe illness interaction M list (7)                               **;
%macro INT_GROUP_M_LIST;
SEVERE_V3_x_HHS_HCC035 
SEVERE_V3_x_HHS_HCC038 
SEVERE_V3_x_HHS_HCC153 
SEVERE_V3_x_HHS_HCC154 
SEVERE_V3_x_HHS_HCC163 
SEVERE_V3_x_HHS_HCC253 
SEVERE_V3_x_G03 
%mend INT_GROUP_M_LIST;

**===========================================================================**;
** ADULT scores list (12)                                                    **;
%macro ADULT_SCORES_LIST;
SCORE_ADULT_PLATINUM 
SCORE_ADULT_GOLD 
SCORE_ADULT_SILVER 
SCORE_ADULT_BRONZE 
SCORE_ADULT_CATASTROPHIC 
CSR_ADJ_SCR_ADULT_PLATINUM 
CSR_ADJ_SCR_ADULT_GOLD 
CSR_ADJ_SCR_ADULT_SILVER 
CSR_ADJ_SCR_ADULT_BRONZE 
CSR_ADJ_SCR_ADULT_CATASTROPHIC
SCORE_ADULT
CSR_ADJ_SCR_ADULT
%mend ADULT_SCORES_LIST;

**===========================================================================**;
** CHILD scores list (12)                                                    **;
%macro CHILD_SCORES_LIST;
SCORE_CHILD_PLATINUM 
SCORE_CHILD_GOLD 
SCORE_CHILD_SILVER 
SCORE_CHILD_BRONZE 
SCORE_CHILD_CATASTROPHIC 
CSR_ADJ_SCR_CHILD_PLATINUM 
CSR_ADJ_SCR_CHILD_GOLD 
CSR_ADJ_SCR_CHILD_SILVER 
CSR_ADJ_SCR_CHILD_BRONZE 
CSR_ADJ_SCR_CHILD_CATASTROPHIC
SCORE_CHILD
CSR_ADJ_SCR_CHILD
%mend CHILD_SCORES_LIST;

**===========================================================================**;
** INFANT severity list (5)                                                  **;
%macro IHCC_SEVERITY_LIST;
IHCC_Severity5
IHCC_Severity4
IHCC_Severity3
IHCC_Severity2
IHCC_Severity1
%mend IHCC_SEVERITY_LIST;

**===========================================================================**;
** INFANT maturity list (5)                                                  **;
%macro IHCC_MATURITY_LIST;
IHCC_Age1
IHCC_Extremely_Immature
IHCC_Immature
IHCC_Premature_Multiples
IHCC_Term
%mend IHCC_MATURITY_LIST;

**===========================================================================**;
** INFANT severity maturity interactions list (25)                           **;
%macro SEV_MAT_INTERACTIONS_LIST;
Age1_x_Severity1 
Age1_x_Severity2 
Age1_x_Severity3 
Age1_x_Severity4 
Age1_x_Severity5 
Premature_Multiples_x_Severity1 
Premature_Multiples_x_Severity2 
Premature_Multiples_x_Severity3 
Premature_Multiples_x_Severity4 
Premature_Multiples_x_Severity5 
Extremely_Immature_x_Severity1 
Extremely_Immature_x_Severity2 
Extremely_Immature_x_Severity3 
Extremely_Immature_x_Severity4 
Extremely_Immature_x_Severity5 
Immature_x_Severity1 
Immature_x_Severity2 
Immature_x_Severity3 
Immature_x_Severity4 
Immature_x_Severity5 
Term_x_Severity1 
Term_x_Severity2 
Term_x_Severity3 
Term_x_Severity4 
Term_x_Severity5 
%mend SEV_MAT_INTERACTIONS_LIST;

**===========================================================================**;
** INFANT scores list (12)                                                   **;
%macro INFANT_SCORES_LIST;
SCORE_INFANT_PLATINUM 
SCORE_INFANT_GOLD 
SCORE_INFANT_SILVER 
SCORE_INFANT_BRONZE 
SCORE_INFANT_CATASTROPHIC 
CSR_ADJ_SCR_INFANT_PLATINUM 
CSR_ADJ_SCR_INFANT_GOLD 
CSR_ADJ_SCR_INFANT_SILVER 
CSR_ADJ_SCR_INFANT_BRONZE 
CSR_ADJ_SCR_INFANT_CATASTROPHIC
SCORE_INFANT
CSR_ADJ_SCR_INFANT
%mend INFANT_SCORES_LIST;

**===========================================================================**;
** all coefficients list, expands to 343 coeff. x 5 metal = (1715)           **;
** some are abbreviated to comply with variable name length restrictions     **;
** e.g., IBPREMATURE_MULTIPLES_X_SEVERITY1 is longer than 32                 **;
%macro ALL_COEFFICIENTS_LIST;
APMAGE_LAST_: AGMAGE_LAST_: ASMAGE_LAST_: ABMAGE_LAST_: ACMAGE_LAST_:                     
APFAGE_LAST_: AGFAGE_LAST_: ASFAGE_LAST_: ABFAGE_LAST_: ACFAGE_LAST_:                     
APHHS_HCC:    AGHHS_HCC:    ASHHS_HCC:    ABHHS_HCC:    ACHHS_HCC:                        
APG:          AGG:          ASG:          ABG:          ACG:                              
APINT_GROUP_: AGINT_GROUP_: ASINT_GROUP_: ABINT_GROUP_: ACINT_GROUP_: 
CPMAGE_LAST_: CGMAGE_LAST_: CSMAGE_LAST_: CBMAGE_LAST_: CCMAGE_LAST_: 
CPFAGE_LAST_: CGFAGE_LAST_: CSFAGE_LAST_: CBFAGE_LAST_: CCFAGE_LAST_: 
CPHHS_HCC:    CGHHS_HCC:    CSHHS_HCC:    CBHHS_HCC:    CCHHS_HCC: 
CPG:          CGG:          CSG:          CBG:          CCG: 
IPEXTREMELY_IMMATURE_X_SEVERITY:          IGEXTREMELY_IMMATURE_X_SEVERITY:  
ISEXTREMELY_IMMATURE_X_SEVERITY:          IBEXTREMELY_IMMATURE_X_SEVERITY:  
ICEXTREMELY_IMMATURE_X_SEVERITY:  
IPIMMATURE_X_SEVERITY:                    IGIMMATURE_X_SEVERITY:            
ISIMMATURE_X_SEVERITY:                    IBIMMATURE_X_SEVERITY:            
ICIMMATURE_X_SEVERITY:            
IPEMATURE_MULTIPLES_X_SEVERITY:           IGEMATURE_MULTIPLES_X_SEVERITY: 
ISEMATURE_MULTIPLES_X_SEVERITY:           IBEMATURE_MULTIPLES_X_SEVERITY: 
ICEMATURE_MULTIPLES_X_SEVERITY: 
IPTERM_X_SEVERITY:                        IGTERM_X_SEVERITY:                
ISTERM_X_SEVERITY:                        IBTERM_X_SEVERITY:                
ICTERM_X_SEVERITY:          
IPAGE1_X_SEVERITY:                        IGAGE1_X_SEVERITY:                
ISAGE1_X_SEVERITY:                        IBAGE1_X_SEVERITY:                
ICAGE1_X_SEVERITY:                
IPAGE0_MALE   IGAGE0_MALE   ISAGE0_MALE   IBAGE0_MALE   ICAGE0_MALE 
IPAGE1_MALE   IGAGE1_MALE   ISAGE1_MALE   IBAGE1_MALE   ICAGE1_MALE 
%mend ALL_COEFFICIENTS_LIST;

**===========================================================================**;
** generate variables specific to ADULT model                                **;
%macro GENERATE_ADULT_VARIABLES;

** severe illness indicator **;
SEVERE_V3=0;
do i=1 to dim(_SEVERE_V3);
 if _SEVERE_V3(i)=1 then SEVERE_V3=1;
end;

** adult HCC groups **;
do i=1 to dim(_ADULT_GROUPS);
 _ADULT_GROUPS(i)=0;
end;

** mandatory for adults **;
HHS_HCC028 = 0;
HHS_HCC064 = 0;
HHS_HCC137 = 0;
HHS_HCC138 = 0;
HHS_HCC139 = 0;
HHS_HCC242 = 0;
HHS_HCC243 = 0;
HHS_HCC244 = 0;
HHS_HCC245 = 0;
HHS_HCC246 = 0;
HHS_HCC247 = 0;
HHS_HCC248 = 0;
HHS_HCC249 = 0;

** these are not mutex **;
if HHS_HCC019=1 or HHS_HCC020=1 or HHS_HCC021=1 then do;
 HHS_HCC019=0;
 HHS_HCC020=0;
 HHS_HCC021=0;
 G01=1;
end;
if HHS_HCC026=1 or HHS_HCC027=1 or HHS_HCC029=1 or HHS_HCC030=1 then do;
 HHS_HCC026=0;
 HHS_HCC027=0;
 HHS_HCC029=0;
 HHS_HCC030=0;
 G02A=1;
end;
if HHS_HCC054=1 or HHS_HCC055=1 then do;
 HHS_HCC054=0;
 HHS_HCC055=0;
 G03=1;
end;
if HHS_HCC061=1 or HHS_HCC062=1 then do;
 HHS_HCC061=0;
 HHS_HCC062=0;
 G04=1;
end;
if HHS_HCC067=1 or HHS_HCC068=1 then do;
 HHS_HCC067=0;
 HHS_HCC068=0;
 G06=1;
end;
if HHS_HCC069=1 or HHS_HCC070=1 or HHS_HCC071=1 then do;
 HHS_HCC069=0;
 HHS_HCC070=0;
 HHS_HCC071=0;
 G07=1;
end;
if HHS_HCC073=1 or HHS_HCC074=1 then do;
 HHS_HCC073=0;
 HHS_HCC074=0;
 G08=1;
end;
if HHS_HCC081=1 or HHS_HCC082=1 then do;
 HHS_HCC081=0;
 HHS_HCC082=0;
 G09=1;
end;
if HHS_HCC106=1 or HHS_HCC107=1 then do;
 HHS_HCC106=0;
 HHS_HCC107=0;
 G10=1;
end;
if HHS_HCC108=1 or HHS_HCC109=1 then do;
 HHS_HCC108=0;
 HHS_HCC109=0;
 G11=1;
end;
if HHS_HCC117=1 or HHS_HCC119=1 then do;
 HHS_HCC117=0;
 HHS_HCC119=0;
 G12=1;
end;
if HHS_HCC126=1 or HHS_HCC127=1 then do;
 HHS_HCC126=0;
 HHS_HCC127=0;
 G13=1;
end;
if HHS_HCC128=1 or HHS_HCC129=1 then do;
 HHS_HCC128=0;
 HHS_HCC129=0;
 G14=1;
end;
if HHS_HCC160=1 or HHS_HCC161=1 then do;
 HHS_HCC160=0;
 HHS_HCC161=0;
 G15=1;
end;
if HHS_HCC187=1 or HHS_HCC188=1 then do;
 HHS_HCC187=0;
 HHS_HCC188=0;
 G16=1;
end;
if HHS_HCC203=1 or HHS_HCC204=1 or HHS_HCC205=1 then do;
 HHS_HCC203=0;
 HHS_HCC204=0;
 HHS_HCC205=0;
 G17=1;
end;
if HHS_HCC207=1 or HHS_HCC208=1 or HHS_HCC209=1 then do;
 HHS_HCC207=0;
 HHS_HCC208=0;
 HHS_HCC209=0;
 G18=1;
end;

** severe illness interactions **;
SEVERE_V3_x_HHS_HCC006  = SEVERE_V3 * HHS_HCC006;
SEVERE_V3_x_HHS_HCC008  = SEVERE_V3 * HHS_HCC008;
SEVERE_V3_x_HHS_HCC009  = SEVERE_V3 * HHS_HCC009;
SEVERE_V3_x_HHS_HCC010  = SEVERE_V3 * HHS_HCC010;
SEVERE_V3_x_HHS_HCC035  = SEVERE_V3 * HHS_HCC035;
SEVERE_V3_x_HHS_HCC038  = SEVERE_V3 * HHS_HCC038;
SEVERE_V3_x_HHS_HCC115  = SEVERE_V3 * HHS_HCC115;
SEVERE_V3_x_HHS_HCC135  = SEVERE_V3 * HHS_HCC135;
SEVERE_V3_x_HHS_HCC145  = SEVERE_V3 * HHS_HCC145;
SEVERE_V3_x_HHS_HCC153  = SEVERE_V3 * HHS_HCC153;
SEVERE_V3_x_HHS_HCC154  = SEVERE_V3 * HHS_HCC154;
SEVERE_V3_x_HHS_HCC163  = SEVERE_V3 * HHS_HCC163;
SEVERE_V3_x_HHS_HCC253  = SEVERE_V3 * HHS_HCC253;
SEVERE_V3_x_G03         = SEVERE_V3 * G03;
SEVERE_V3_x_G06         = SEVERE_V3 * G06;
SEVERE_V3_x_G08         = SEVERE_V3 * G08;

** severe illness interaction groups H and M **;
INT_GROUP_H=0;
do i=1 to dim(_INT_GROUP_H);
 if _INT_GROUP_H(i)=1 then INT_GROUP_H=1;
end;

INT_GROUP_M=0;
if INT_GROUP_H=0 then do; 
 do i=1 to dim(_INT_GROUP_M);
  if _INT_GROUP_M(i)=1 then INT_GROUP_M=1;
 end;
end;
%mend GENERATE_ADULT_VARIABLES;

**===========================================================================**;
** generate variables specific to CHILD model                                **;
%macro GENERATE_CHILD_VARIABLES;

** child HCC groups **;
do i=1 to dim(_CHILD_GROUPS);
 _CHILD_GROUPS(i)=0;
end;

** mandatory for children **;
HHS_HCC064 = 0;
HHS_HCC242 = 0;
HHS_HCC243 = 0;
HHS_HCC244 = 0;
HHS_HCC245 = 0;
HHS_HCC246 = 0;
HHS_HCC247 = 0;
HHS_HCC248 = 0;
HHS_HCC249 = 0;

** these are not mutex **;
if HHS_HCC019=1 or HHS_HCC020=1 or HHS_HCC021=1 then do;
 HHS_HCC019=0;
 HHS_HCC020=0;
 HHS_HCC021=0;
 G01=1;
end;
if HHS_HCC026=1 or HHS_HCC027=1 or HHS_HCC028=1 or HHS_HCC029=1 or HHS_HCC030=1 then do;
 HHS_HCC026=0;
 HHS_HCC027=0;
 HHS_HCC028=0;
 HHS_HCC029=0;
 HHS_HCC030=0;
 G02=1;
end;
if HHS_HCC054=1 or HHS_HCC055=1 then do;
 HHS_HCC054=0;
 HHS_HCC055=0;
 G03=1;
end;
if HHS_HCC061=1 or HHS_HCC062=1 then do;
 HHS_HCC061=0;
 HHS_HCC062=0;
 G04=1;
end;
if HHS_HCC067=1 or HHS_HCC068=1 then do;
 HHS_HCC067=0;
 HHS_HCC068=0;
 G06=1;
end;
if HHS_HCC069=1 or HHS_HCC070=1 or HHS_HCC071=1 then do;
 HHS_HCC069=0;
 HHS_HCC070=0;
 HHS_HCC071=0;
 G07=1;
end;
if HHS_HCC073=1 or HHS_HCC074=1 then do;
 HHS_HCC073=0;
 HHS_HCC074=0;
 G08=1;
end;
if HHS_HCC081=1 or HHS_HCC082=1 then do;
 HHS_HCC081=0;
 HHS_HCC082=0;
 G09=1;
end;
if HHS_HCC106=1 or HHS_HCC107=1 then do;
 HHS_HCC106=0;
 HHS_HCC107=0;
 G10=1;
end;
if HHS_HCC108=1 or HHS_HCC109=1 then do;
 HHS_HCC108=0;
 HHS_HCC109=0;
 G11=1;
end;
if HHS_HCC117=1 or HHS_HCC119=1 then do;
 HHS_HCC117=0;
 HHS_HCC119=0;
 G12=1;
end;
if HHS_HCC126=1 or HHS_HCC127=1 then do;
 HHS_HCC126=0;
 HHS_HCC127=0;
 G13=1;
end;
if HHS_HCC128=1 or HHS_HCC129=1 then do;
 HHS_HCC128=0;
 HHS_HCC129=0;
 G14=1;
end;
if HHS_HCC160=1 or HHS_HCC161=1 then do;
 HHS_HCC160=0;
 HHS_HCC161=0;
 G15=1;
end;
if HHS_HCC187=1 or HHS_HCC188=1 then do;
 HHS_HCC187=0;
 HHS_HCC188=0;
 G16=1;
end;
if HHS_HCC203=1 or HHS_HCC204=1 or HHS_HCC205=1 then do;
 HHS_HCC203=0;
 HHS_HCC204=0;
 HHS_HCC205=0;
 G17=1;
end;
if HHS_HCC207=1 or HHS_HCC208=1 or HHS_HCC209=1 then do;
 HHS_HCC207=0;
 HHS_HCC208=0;
 HHS_HCC209=0;
 G18=1;
end;
%mend GENERATE_CHILD_VARIABLES;

**===========================================================================**;
** generate variables specific to INFANT model                               **;
%macro GENERATE_INFANT_VARIABLES;

** severity levels, 5 is highest, enrollee may have more than one level **;
do i=1 to dim(_IHCC_SEVERITY);
 _IHCC_SEVERITY(i)=0;
end;

** mandatory for infants **;
HHS_HCC087 = 0;
HHS_HCC088 = 0;
HHS_HCC089 = 0;
HHS_HCC090 = 0;
HHS_HCC094 = 0;
HHS_HCC203 = 0;
HHS_HCC204 = 0;
HHS_HCC205 = 0;
HHS_HCC207 = 0;
HHS_HCC208 = 0; 
HHS_HCC209 = 0;

** these are not mutex **;
select;
 when (HHS_HCC008  =1) IHCC_Severity5=1;
 when (HHS_HCC018  =1) IHCC_Severity5=1;
 when (HHS_HCC034  =1) IHCC_Severity5=1;
 when (HHS_HCC035  =1) IHCC_Severity5=1;
 when (HHS_HCC041  =1) IHCC_Severity5=1;
 when (HHS_HCC042  =1) IHCC_Severity5=1;
 when (HHS_HCC125  =1) IHCC_Severity5=1;
 when (HHS_HCC128  =1) IHCC_Severity5=1;
 when (HHS_HCC129  =1) IHCC_Severity5=1;
 when (HHS_HCC130  =1) IHCC_Severity5=1;
 when (HHS_HCC137  =1) IHCC_Severity5=1;
 when (HHS_HCC158  =1) IHCC_Severity5=1;
 when (HHS_HCC183  =1) IHCC_Severity5=1;
 when (HHS_HCC184  =1) IHCC_Severity5=1;
 when (HHS_HCC251  =1) IHCC_Severity5=1;
 otherwise;
end;

select;
 when (HHS_HCC002  =1) IHCC_Severity4=1;
 when (HHS_HCC009  =1) IHCC_Severity4=1;
 when (HHS_HCC026  =1) IHCC_Severity4=1;
 when (HHS_HCC064  =1) IHCC_Severity4=1;
 when (HHS_HCC067  =1) IHCC_Severity4=1;
 when (HHS_HCC068  =1) IHCC_Severity4=1;
 when (HHS_HCC073  =1) IHCC_Severity4=1;
 when (HHS_HCC106  =1) IHCC_Severity4=1;
 when (HHS_HCC107  =1) IHCC_Severity4=1;
 when (HHS_HCC111  =1) IHCC_Severity4=1;
 when (HHS_HCC112  =1) IHCC_Severity4=1;
 when (HHS_HCC115  =1) IHCC_Severity4=1;
 when (HHS_HCC122  =1) IHCC_Severity4=1;
 when (HHS_HCC126  =1) IHCC_Severity4=1;
 when (HHS_HCC127  =1) IHCC_Severity4=1;
 when (HHS_HCC131  =1) IHCC_Severity4=1;
 when (HHS_HCC135  =1) IHCC_Severity4=1;
 when (HHS_HCC138  =1) IHCC_Severity4=1;
 when (HHS_HCC145  =1) IHCC_Severity4=1;
 when (HHS_HCC146  =1) IHCC_Severity4=1;
 when (HHS_HCC154  =1) IHCC_Severity4=1;
 when (HHS_HCC156  =1) IHCC_Severity4=1;
 when (HHS_HCC163  =1) IHCC_Severity4=1;
 when (HHS_HCC187  =1) IHCC_Severity4=1;
 when (HHS_HCC226  =1) IHCC_Severity4=1;
 when (HHS_HCC253  =1) IHCC_Severity4=1;
 otherwise;
end;

select;
 when (HHS_HCC001  =1) IHCC_Severity3=1;
 when (HHS_HCC003  =1) IHCC_Severity3=1;
 when (HHS_HCC006  =1) IHCC_Severity3=1;
 when (HHS_HCC010  =1) IHCC_Severity3=1;
 when (HHS_HCC011  =1) IHCC_Severity3=1;
 when (HHS_HCC012  =1) IHCC_Severity3=1;
 when (HHS_HCC027  =1) IHCC_Severity3=1;
 when (HHS_HCC030  =1) IHCC_Severity3=1;
 when (HHS_HCC038  =1) IHCC_Severity3=1;
 when (HHS_HCC045  =1) IHCC_Severity3=1;
 when (HHS_HCC054  =1) IHCC_Severity3=1;
 when (HHS_HCC055  =1) IHCC_Severity3=1;
 when (HHS_HCC061  =1) IHCC_Severity3=1;
 when (HHS_HCC063  =1) IHCC_Severity3=1;
 when (HHS_HCC066  =1) IHCC_Severity3=1;
 when (HHS_HCC074  =1) IHCC_Severity3=1;
 when (HHS_HCC075  =1) IHCC_Severity3=1;
 when (HHS_HCC096  =1) IHCC_Severity3=1;
 when (HHS_HCC108  =1) IHCC_Severity3=1;
 when (HHS_HCC109  =1) IHCC_Severity3=1;
 when (HHS_HCC110  =1) IHCC_Severity3=1;
 when (HHS_HCC113  =1) IHCC_Severity3=1;
 when (HHS_HCC117  =1) IHCC_Severity3=1;
 when (HHS_HCC119  =1) IHCC_Severity3=1;
 when (HHS_HCC121  =1) IHCC_Severity3=1;
 when (HHS_HCC132  =1) IHCC_Severity3=1;
 when (HHS_HCC139  =1) IHCC_Severity3=1;
 when (HHS_HCC142  =1) IHCC_Severity3=1;
 when (HHS_HCC149  =1) IHCC_Severity3=1;
 when (HHS_HCC150  =1) IHCC_Severity3=1;
 when (HHS_HCC159  =1) IHCC_Severity3=1;
 when (HHS_HCC162  =1) IHCC_Severity3=1;
 when (HHS_HCC227  =1) IHCC_Severity3=1;
 otherwise;
end;

select;
 when (HHS_HCC004  =1) IHCC_Severity2=1;
 when (HHS_HCC013  =1) IHCC_Severity2=1;
 when (HHS_HCC019  =1) IHCC_Severity2=1;
 when (HHS_HCC020  =1) IHCC_Severity2=1;
 when (HHS_HCC021  =1) IHCC_Severity2=1;
 when (HHS_HCC023  =1) IHCC_Severity2=1;
 when (HHS_HCC028  =1) IHCC_Severity2=1;
 when (HHS_HCC029  =1) IHCC_Severity2=1;
 when (HHS_HCC036  =1) IHCC_Severity2=1;
 when (HHS_HCC046  =1) IHCC_Severity2=1;
 when (HHS_HCC048  =1) IHCC_Severity2=1;
 when (HHS_HCC056  =1) IHCC_Severity2=1;
 when (HHS_HCC057  =1) IHCC_Severity2=1;
 when (HHS_HCC062  =1) IHCC_Severity2=1;
 when (HHS_HCC069  =1) IHCC_Severity2=1;
 when (HHS_HCC070  =1) IHCC_Severity2=1;
 when (HHS_HCC081  =1) IHCC_Severity2=1;
 when (HHS_HCC082  =1) IHCC_Severity2=1;
 when (HHS_HCC097  =1) IHCC_Severity2=1;
 when (HHS_HCC114  =1) IHCC_Severity2=1;
 when (HHS_HCC120  =1) IHCC_Severity2=1;
 when (HHS_HCC151  =1) IHCC_Severity2=1;
 when (HHS_HCC153  =1) IHCC_Severity2=1;
 when (HHS_HCC160  =1) IHCC_Severity2=1;
 when (HHS_HCC217  =1) IHCC_Severity2=1;
 otherwise;
end;

select;
 when (HHS_HCC037  =1) IHCC_Severity1=1;
 when (HHS_HCC047  =1) IHCC_Severity1=1;
 when (HHS_HCC071  =1) IHCC_Severity1=1;
 when (HHS_HCC102  =1) IHCC_Severity1=1;
 when (HHS_HCC103  =1) IHCC_Severity1=1;
 when (HHS_HCC118  =1) IHCC_Severity1=1;
 when (HHS_HCC161  =1) IHCC_Severity1=1;
 when (HHS_HCC188  =1) IHCC_Severity1=1;
 when (HHS_HCC254  =1) IHCC_Severity1=1;
 otherwise;
end;

** impose hierarchy as enrollee may have more than one level **;
if IHCC_Severity5=1 then do;
 IHCC_Severity4=0;
 IHCC_Severity3=0;
 IHCC_Severity2=0; 
 IHCC_Severity1=0;
end;
else if IHCC_Severity4=1 then do;
 IHCC_Severity3=0;
 IHCC_Severity2=0;
 IHCC_Severity1=0;
end;
else if IHCC_Severity3=1 then do;
 IHCC_Severity2=0;
 IHCC_Severity1=0;
end;
else if IHCC_Severity2=1 then IHCC_Severity1=0;
** default level **;
else IHCC_Severity1=1;

** maturity levels, enrollee may have more than one level **;
do i=1 to dim(_IHCC_MATURITY);
 _IHCC_MATURITY(i)=0;
end;

if AGE_LAST=1 then IHCC_Age1=1;
else do;
 ** age 0, not mutually exclusive so no ELSE **;
 if HHS_HCC242=1 or HHS_HCC243=1 or HHS_HCC244=1 then IHCC_Extremely_Immature=1;
 if HHS_HCC245=1 or HHS_HCC246=1 then IHCC_Immature=1;
 if HHS_HCC247=1 or HHS_HCC248=1 then IHCC_Premature_Multiples=1;
 if HHS_HCC249=1 then IHCC_Term=1;
 ** impose hierarchy as enrollee may have more than one level **;
 if IHCC_Extremely_Immature=1 then do;
  IHCC_Immature           =0;
  IHCC_Premature_Multiples=0;
  IHCC_Term               =0;
 end;
 else if IHCC_Immature=1 then do;
  IHCC_Premature_Multiples=0;
  IHCC_Term               =0;
 end;
 else if IHCC_Premature_Multiples=1 then IHCC_Term=0;
 ** age 0 default level, as age 0 should have at least one HCC 242-249 **;
 else IHCC_Term=1;
end;

do i=1 to dim(_SEV_MAT_INTERACTIONS);
 _SEV_MAT_INTERACTIONS(i)=0;
end;

** severity level x maturity interactions **;
Extremely_Immature_x_Severity5     = IHCC_Severity5 * IHCC_Extremely_Immature;
Immature_x_Severity5               = IHCC_Severity5 * IHCC_Immature;
Premature_Multiples_x_Severity5    = IHCC_Severity5 * IHCC_Premature_Multiples;
Term_x_Severity5                   = IHCC_Severity5 * IHCC_Term;
Age1_x_Severity5                   = IHCC_Severity5 * IHCC_Age1;
Extremely_Immature_x_Severity4     = IHCC_Severity4 * IHCC_Extremely_Immature;
Immature_x_Severity4               = IHCC_Severity4 * IHCC_Immature;
Premature_Multiples_x_Severity4    = IHCC_Severity4 * IHCC_Premature_Multiples;
Term_x_Severity4                   = IHCC_Severity4 * IHCC_Term;
Age1_x_Severity4                   = IHCC_Severity4 * IHCC_Age1;
Extremely_Immature_x_Severity3     = IHCC_Severity3 * IHCC_Extremely_Immature;
Immature_x_Severity3               = IHCC_Severity3 * IHCC_Immature;
Premature_Multiples_x_Severity3    = IHCC_Severity3 * IHCC_Premature_Multiples;
Term_x_Severity3                   = IHCC_Severity3 * IHCC_Term;
Age1_x_Severity3                   = IHCC_Severity3 * IHCC_Age1;
Extremely_Immature_x_Severity2     = IHCC_Severity2 * IHCC_Extremely_Immature;
Immature_x_Severity2               = IHCC_Severity2 * IHCC_Immature;
Premature_Multiples_x_Severity2    = IHCC_Severity2 * IHCC_Premature_Multiples;
Term_x_Severity2                   = IHCC_Severity2 * IHCC_Term;
Age1_x_Severity2                   = IHCC_Severity2 * IHCC_Age1;
Extremely_Immature_x_Severity1     = IHCC_Severity1 * IHCC_Extremely_Immature;
Immature_x_Severity1               = IHCC_Severity1 * IHCC_Immature;
Premature_Multiples_x_Severity1    = IHCC_Severity1 * IHCC_Premature_Multiples;
Term_x_Severity1                   = IHCC_Severity1 * IHCC_Term;
Age1_x_Severity1                   = IHCC_Severity1 * IHCC_Age1;
%mend GENERATE_INFANT_VARIABLES;

**===========================================================================**;
** check existence of user-provided variable                                 **;
%macro CHECK_VARIABLE_EXISTS(LONGFILE,VAR);
Check=varnum(dsid,"&VAR");
if Check=0 then do;
 &MSG01 put "ERROR  : [Msg01] Variable &VAR is not in &LONGFILE file";
 Flag=1; 
end;
%mend CHECK_VARIABLE_EXISTS;

**===========================================================================**;
** check data type of user-provided variable                                 **;
%macro CHECK_DATA_TYPE(VAR,TYPE,LONGTYPE,LONGFILE);
if vtype(&VAR) ne "&TYPE" then do; 
 &MSG02 put "ERROR  : [Msg02] User-provided variable &VAR in &LONGFILE file must be &LONGTYPE type"; 
 Flag=1; 
end;
%mend CHECK_DATA_TYPE;

**===========================================================================**;
** zero HCCs, used in hierarchy checking macro                               **;
%macro SET0(CC=, HIER=);
if HCC&CC=1 then do i = &HIER; 
 _HCC(i) = 0; 
end;
%mend SET0; 

**===========================================================================**;
** lookup CCs for primary and secondary assignment, VC is 9/0                **;
%macro FIND_CCs(VC);
** 
set elements of 0/1 array _C(*) to 1.                                     
steps:                                                               
 o CC initialized to 9999.                                                 
 o DIAG is sent to diagnoses edit macro. If CC is changed to -1 it means   
    DIAG did not pass edits and nothing else needs to be done with it.     
 o If CC set to another number - nothing else needs to be done.            
 o If CC is still 9999, primary assignment and secondary assigment       
    must be done.                                                           
**;
if CC ne "-1.0" and CC ne "9999" then do;
 ** c to n conversion **;
 IND=input(CC,8.);
 ** poke returned valid array pointer CC into 0/1 array with value=1 **;
 if 1 <= IND <= dim(_C) then _C(IND)=1;
 ** for debugging **;
 *file print; *put "Find " DIAG= CC= IND=;
end;
else if CC="9999" then do; 
 ** some diagnoses get multiple CCs **;
 ** CC creation depends on ICD version code **;
 ** primary assignment **;
 IND = input(left(put(DIAG,$I&VC.&CCFMT..)),8.);
 if 1 <= IND <= dim(_C) then _C(IND)=1;
 ** for debugging **;
 *file print; *put "Find " DIAG= CC= IND=;
 ** duplicate assignment **;
 IND = input(left(put(DIAG,$I&VC.dup_&CCFMT..)),8.);
 if 1 <= IND <= dim(_C) then _C(IND)=1;
 ** for debugging **;
 *file print; *put "Find " DIAG= CC= IND=;
end;
** CC=-1 means diag did not pass edits **;
%mend FIND_CCs;

**===========================================================================**;
** check existance of dataset                                                **;
%macro CHECKDS(FILE,LONGFILE);
%if %sysfunc(exist(&FILE)) %then;
%else %do;
 data _null_;
  file print ls=255;
  &MSG30 put "ERROR  : [Msg30] Program halted, file &LONGFILE does not exist"; 
  abort; run;
%end;
%mend CHECKDS;

**===========================================================================**;
**===========================================================================**;
**===========================================================================**;

**===========================================================================**;
** merge person and diagnosis datasets, outputting one record                **;
** per person containing score and HHS-HCC variables for each input          **;
** person-level record fully compliant with validity filters                 **;
**===========================================================================**;
title2 "Errors/warnings/notes log";
title3 " ";

** pre-pass failsafe check that PERSON dataset exists **;
%CHECKDS(&INP,PERSON);

** pre-pass failsafe check that DIAG dataset exists **;
%CHECKDS(&IND,DIAG);

** pre-pass failsafe check for multiple PERSON records **;
data _null_;
 length Flag 3;
 retain Flag 0;
 set &INP(keep=&IDVAR) END=END;
 by &IDVAR;
 file print ls=255;
 if first.&IDVAR and last.&IDVAR then;
 else do;
  Flag=1;
  &MSG03 put "ERROR  : [Msg03] Duplicate &IDVAR.s in PERSON file " &IDVAR=; 
 end;
 if END then do;
  if Flag=1 then do;
   &MSG04 put "ERROR  : [Msg04] Program halted due to duplicate &IDVAR.s in PERSON file"; 
   abort; 
  end; 
  else do;
   &MSG05 put "OK     : [Msg05] PERSON file is free of duplicate &IDVAR.s"; 
  end;
 end; run;

**===========================================================================**;
** pre-pass failsafe check of PERSON file for non-existent user-provided variables **;
data _null_;
 length Flag 3;
 file print ls=255;
 Flag=0;
 dsid=open("&INP");
 %CHECK_VARIABLE_EXISTS(PERSON,&IDVAR       );
 %CHECK_VARIABLE_EXISTS(PERSON,SEX          );
 %CHECK_VARIABLE_EXISTS(PERSON,DOB          );
 %CHECK_VARIABLE_EXISTS(PERSON,AGE_LAST     );
 %CHECK_VARIABLE_EXISTS(PERSON,METAL        );
 %CHECK_VARIABLE_EXISTS(PERSON,CSR_INDICATOR); 
 if Flag=1 then do;
  &MSG06 put "ERROR  : [Msg06] Program halted due to non-existent variable(s) in PERSON file";
  abort; 
 end;
 else do;
  &MSG07 put "OK     : [Msg07] PERSON file contains all requisite variables"; 
 end; run;

**===========================================================================**;
** pre-pass failsafe check of PERSON file for invalid data types in user-provided variables **;
data _null_;
 length Flag 3;
 set &INP(obs=1);
 file print ls=255;
 ** IDVAR can be either type **;
 %CHECK_DATA_TYPE(SEX          ,C,character,PERSON);
 %CHECK_DATA_TYPE(DOB          ,N,numeric  ,PERSON);
 %CHECK_DATA_TYPE(AGE_LAST     ,N,numeric  ,PERSON);
 %CHECK_DATA_TYPE(METAL        ,C,character,PERSON);
 %CHECK_DATA_TYPE(CSR_INDICATOR,N,numeric  ,PERSON);
 if Flag=1 then do;
  &MSG08 put "ERROR  : [Msg08] Program halted due to incorrect user-provided variable type(s) in PERSON file"; 
  abort; 
 end;
 else do;
  &MSG09 put "OK     : [Msg09] PERSON file`s variables have the correct type"; 
 end; run;

**===========================================================================**;
** pre-pass failsafe check of DIAG file for non-existent user-provided variables **;
data _null_;
 length Flag 3;
 file print ls=255;
 Flag=0;
 dsid=open("&IND");
 %CHECK_VARIABLE_EXISTS(DIAG,&IDVAR                );
 %CHECK_VARIABLE_EXISTS(DIAG,DIAG                  );
 %CHECK_VARIABLE_EXISTS(DIAG,DIAGNOSIS_VERSION_CODE);
 %CHECK_VARIABLE_EXISTS(DIAG,DIAGNOSIS_SERVICE_DATE);
 if Flag=1 then do;
  &MSG10 put "ERROR  : [Msg10] Program halted due to non-existent variable(s) in DIAG file";
  abort; 
 end;
 else do;
  &MSG11 put "OK     : [Msg11] DIAG file contains all requisite variables"; 
 end; run;

**===========================================================================**;
** pre-pass failsafe check of DIAG file for invalid data types in user-provided variables **;
data _null_;
 length Flag 3;
 set &IND(obs=1);
 file print ls=255;
 ** IDVAR can be either type **;
 %CHECK_DATA_TYPE(DIAG                  ,C,character,DIAG);
 %CHECK_DATA_TYPE(DIAGNOSIS_VERSION_CODE,C,character,DIAG);
 %CHECK_DATA_TYPE(DIAGNOSIS_SERVICE_DATE,N,numeric  ,DIAG);
 if Flag=1 then do;
  &MSG12 put "ERROR  : [Msg12] Program halted due to incorrect user-provided variable type(s) in DIAG file"; 
  abort; 
 end;
 else do;
  &MSG13 put "OK     : [Msg13] DIAG file`s variables have the correct type"; 
 end; run;

**===========================================================================**;
data &OUTDATA(keep=&IDVAR &KEEPVAR);

 **==========================================================================**;
 ** declare lengths, retained variables, arrays                              **;
 **==========================================================================**;
 length
  %AGE_SEX_LIST
  %HCC_FULL_LIST 
  %CC_FULL_LIST
  %HHS_HCC_PM_LIST
  %HCC_PM_LIST
  %HHS_CC_PM_LIST
  %CC_PM_LIST
  %ADULT_GROUPS_LIST
  %CHILD_GROUPS_LIST
  %ADULT_VARIABLES_LIST
  %CHILD_VARIABLES_LIST
  %INFANT_VARIABLES_LIST
  %SEVERE_V3_LIST
  %INT_GROUP_H_LIST
  %INT_GROUP_M_LIST
  %IHCC_SEVERITY_LIST
  %IHCC_MATURITY_LIST
  %SEV_MAT_INTERACTIONS_LIST
  SEVERE_V3
  ICD9_10Newborn 
  DeleteRecord
  IND
  I
  K
  BadHCC 
  NumberOfHCCs 
  DeltaAge 3

  OriginalDOB 
  OriginalDIAGNOSIS_SERVICE_DATE 
  %ALL_COEFFICIENTS_LIST 8

  ICD9_10NewbornDiag $ 7

  CC $ 4 

  SEX_1_2 $ 1
 ;

 retain 
  %CC_FULL_LIST
  ICD9_10Newborn 
  ICD9_10NewbornDiag
  SEX_1_2
  OriginalDOB
  OriginalDIAGNOSIS_SERVICE_DATE;

 file print ls=255;

 **==========================================================================**;
 ** arrays, naming convention is prefix `_` followed by upper case name      **;
 **==========================================================================**;
 array _AGE_SEX(*)                %AGE_SEX_LIST;

 ** PM sets used in internal macros, suffix is 001,002,...  r.t. 1,2,...  **;
 array _HHS_HCC_PM(*)             %HHS_HCC_PM_LIST;
 array _HCC_PM(*)                 %HCC_PM_LIST;
 array _HHS_CC_PM(*)              %HHS_CC_PM_LIST;
 array _CC_PM(*)                  %CC_PM_LIST;

 ** full sets used in external macros, suffix is 1,2,...  r.t. 001,002,... **;
 array _C(*)                      %CC_FULL_LIST;
 array _HCC(*)                    %HCC_FULL_LIST;

 array _ADULT_COEFFICIENTS_AP(*)  %ADULT_COEFFICIENTS_LIST(AP);
 array _ADULT_COEFFICIENTS_AG(*)  %ADULT_COEFFICIENTS_LIST(AG);
 array _ADULT_COEFFICIENTS_AS(*)  %ADULT_COEFFICIENTS_LIST(AS);
 array _ADULT_COEFFICIENTS_AB(*)  %ADULT_COEFFICIENTS_LIST(AB);
 array _ADULT_COEFFICIENTS_AC(*)  %ADULT_COEFFICIENTS_LIST(AC);
 array _ADULT_VARIABLES(*)        %ADULT_VARIABLES_LIST;
 
 array _CHILD_COEFFICIENTS_CP(*)  %CHILD_COEFFICIENTS_LIST(CP);
 array _CHILD_COEFFICIENTS_CG(*)  %CHILD_COEFFICIENTS_LIST(CG);
 array _CHILD_COEFFICIENTS_CS(*)  %CHILD_COEFFICIENTS_LIST(CS);
 array _CHILD_COEFFICIENTS_CB(*)  %CHILD_COEFFICIENTS_LIST(CB);
 array _CHILD_COEFFICIENTS_CC(*)  %CHILD_COEFFICIENTS_LIST(CC);
 array _CHILD_VARIABLES(*)        %CHILD_VARIABLES_LIST;
 
 array _INFANT_COEFFICIENTS_IP(*) %INFANT_COEFFICIENTS_LIST(IP);
 array _INFANT_COEFFICIENTS_IG(*) %INFANT_COEFFICIENTS_LIST(IG);
 array _INFANT_COEFFICIENTS_IS(*) %INFANT_COEFFICIENTS_LIST(IS);
 array _INFANT_COEFFICIENTS_IB(*) %INFANT_COEFFICIENTS_LIST(IB);
 array _INFANT_COEFFICIENTS_IC(*) %INFANT_COEFFICIENTS_LIST(IC);
 array _INFANT_VARIABLES(*)       %INFANT_VARIABLES_LIST;

 array _ADULT_SCORES(*)           %ADULT_SCORES_LIST;
 array _CHILD_SCORES(*)           %CHILD_SCORES_LIST;
 array _INFANT_SCORES(*)          %INFANT_SCORES_LIST;
 
 ** adult **;
 array _SEVERE_V3(*)              %SEVERE_V3_LIST;
 array _ADULT_GROUPS(*)           %ADULT_GROUPS_LIST;
 array _INT_GROUP_M(*)            %INT_GROUP_M_LIST;
 array _INT_GROUP_H(*)            %INT_GROUP_H_LIST;
 
 ** child **;
 array _CHILD_GROUPS(*)           %CHILD_GROUPS_LIST;
 
 ** infant **;
 array _IHCC_SEVERITY(*)          %IHCC_SEVERITY_LIST;
 array _IHCC_MATURITY(*)          %IHCC_MATURITY_LIST;
 array _SEV_MAT_INTERACTIONS(*)   %SEV_MAT_INTERACTIONS_LIST;

 **==========================================================================**;
 ** append calibration coefficients once                                     **;
 **==========================================================================**;
 if _n_=1 then set INCOEF.Coefficients;
 
 **==========================================================================**;
 ** merge person and diagnoses datasets by &IDVAR                            **;
 ** this assumes user has sorted both datasets by &IDVAR                     **;
 ** and that person dataset contains no duplicates                           **;
 **==========================================================================**;
 merge 
  &INP(in=InPerson)
  &IND(in=InDiagnosis);
 by &IDVAR;

 **==========================================================================**;
 ** diagnosis but no person record, diagnosis rejected **;
 if not InPerson then do;
  &MSG14 put "WARNING: [Msg14] Diagnosis matches no enrollee, diagnosis ignored " 
    @71 &IDVAR= DIAG= DIAGNOSIS_VERSION_CODE= DIAGNOSIS_SERVICE_DATE=;
 end;

 **==========================================================================**;
 ** person record exists, may or may not have diagnoses **;
 else do;

  **==========================================================================**;
  ** first record for person **;
  if first.&IDVAR then do;
   SEX=upcase(substr(left(SEX),1,1));
   ** remap SEX (M/F) to comply with age/sex edits macro (1/2) **;
   SEX_1_2=" ";
   if      SEX in("M","1") then SEX_1_2="1";
   else if SEX in("F","2") then SEX_1_2="2";
   ** convert yyyymmdd to SAS date, suppress error message if invalid **;
   OriginalDOB=DOB;
   DOB=input(put(DOB,z8.),?? yymmdd8.); 
   METAL=upcase(substr(left(METAL),1,1));
   ** used to check for parent with newborn records **;
   ICD9_10Newborn=0;
   ICD9_10NewbornDiag=" ";
   ** set CCs to 0 **;
   do i=1 to dim(_C);
    _C(i)=0;
   end;
  end /*if first.IDVAR*/;

  **=========================================================================**;
  ** if there are diagnoses for enrollee then do:                            **;
  **  - create CCs using format &CCFMT                                      **;
  **  - perform ICD9 edits using macro &I9EMACRO                             **;
  **  - perform ICD10 edits using macro &I0EMACRO                            **;
  **  - assign additional CCs if applicable                                  **;
  **=========================================================================**;
  ** person has no diagnoses, OK, note only **;
  if not InDiagnosis then do;
    &MSG17 put "NOTE   : [Msg17] Enrollee has no diagnoses, risk score based on demographic information " 
      @91 &IDVAR=;
  end;

  ** diagnosis record found **;
  else do;
   DIAG=upcase(left(DIAG));
   DIAGNOSIS_VERSION_CODE=upcase(left(DIAGNOSIS_VERSION_CODE));
   ** convert yyyymmdd to SAS date, suppress error message if invalid **;
   OriginalDIAGNOSIS_SERVICE_DATE=DIAGNOSIS_SERVICE_DATE;
   DIAGNOSIS_SERVICE_DATE=input(put(DIAGNOSIS_SERVICE_DATE,z8.),?? yymmdd8.); 
   ** compute age at diagnosis for MCE edits **;
   if DOB ne . and DIAGNOSIS_SERVICE_DATE ne . then 
     AGE_AT_DIAGNOSIS=floor(yrdif(DOB,DIAGNOSIS_SERVICE_DATE,"AGE"));
   ** no negatives **;
   if (. < AGE_AT_DIAGNOSIS < 0) then AGE_AT_DIAGNOSIS=0;
 
   **=========================================================================**;
   ** errors, do not perform lookup **;
   DeltaAge=0;
   if AGE_AT_DIAGNOSIS ne . and AGE_LAST ne . then DeltaAge=AGE_LAST-AGE_AT_DIAGNOSIS;
   if missing(DIAG)                          or 
      DIAGNOSIS_VERSION_CODE not in("9","0") or 
      missing(DIAGNOSIS_SERVICE_DATE)        or 
      AGE_AT_DIAGNOSIS < 0                   or 
      SEX_1_2 not in("1","2")                or 
      missing(AGE_LAST)                      or
      AGE_AT_DIAGNOSIS > AGE_LAST            or
      (not missing(DOB) and not missing(DIAGNOSIS_SERVICE_DATE) and DOB > DIAGNOSIS_SERVICE_DATE) or
      DeltaAge > 1 then do;
    ** signal only DIAG errors, these are not mutex so no ELSE **;
    if missing(DIAG) then do;
     &MSG15 put "WARNING: [Msg15] Blank diagnosis code, diagnosis ignored " @71 &IDVAR= DIAG=;
    end;
    if DIAGNOSIS_VERSION_CODE not in("9","0") then do;
     &MSG26 put "WARNING: [Msg26] Invalid DIAGNOSIS_VERSION_CODE, diagnosis ignored " 
      @71 &IDVAR= DIAGNOSIS_VERSION_CODE=;
    end;
    if missing(DIAGNOSIS_SERVICE_DATE) then do;
     &MSG27 put "WARNING: [Msg27] Invalid DIAGNOSIS_SERVICE_DATE, diagnosis ignored " 
      @71 &IDVAR= OriginalDIAGNOSIS_SERVICE_DATE=;
    end;
    if AGE_AT_DIAGNOSIS<0 then do;
     &MSG28 put "WARNING: [Msg28] Invalid AGE_AT_DIAGNOSIS, diagnosis ignored " 
      @71 &IDVAR= OriginalDOB= OriginalDIAGNOSIS_SERVICE_DATE= AGE_AT_DIAGNOSIS=;
    end;
    if AGE_AT_DIAGNOSIS>AGE_LAST then do;
     &MSG29 put "WARNING: [Msg29] AGE_AT_DIAGNOSIS > AGE_LAST, diagnosis ignored " 
      @71 &IDVAR= AGE_AT_DIAGNOSIS= AGE_LAST=;
    end;
    if (not missing(DOB) and not missing(DIAGNOSIS_SERVICE_DATE) and DOB > DIAGNOSIS_SERVICE_DATE) then do;
     &MSG32 put "WARNING: [Msg32] DOB > DIAGNOSIS_SERVICE_DATE, diagnosis ignored " 
      @71 &IDVAR= OriginalDOB= OriginalDIAGNOSIS_SERVICE_DATE=;
    end;
    if DeltaAge>1 then do;
     &MSG31 put "WARNING: [Msg31] AGE_LAST minus AGE_AT_DIAGNOSIS > 1, diagnosis ignored " 
      @81 &IDVAR= AGE_AT_DIAGNOSIS= AGE_LAST=;
    end;
   end;

   **=========================================================================**;
   ** no errors, attempt lookup **;
   else do;

    ** ICD9 bundled claim check **;
    if DIAGNOSIS_VERSION_CODE="9" then do;
     ** flag if ICD9 code is on bundled claim list **;
     ** infant with adult maternity diagnoses **;
     if AGE_LAST < 2 and put(left(DIAG),$B9A.)="B9A" then do;
      ICD9_10Newborn=2;
      ICD9_10NewbornDiag=DIAG;
     end;
     ** adult with infant newborn diagnoses **;
     else if AGE_LAST >= 2 and put(left(DIAG),$B9I.)="B9I" then do;
      ICD9_10Newborn=1;
      ICD9_10NewbornDiag=DIAG;
     end;
    end;
    ** ICD10 bundled claim check **;
    else if DIAGNOSIS_VERSION_CODE="0" then do;
     ** flag if ICD10 code is on bundled claim list **;
     ** infant with adult maternity diagnoses **;
     if AGE_LAST < 2 and put(left(DIAG),$B10A.)="B10A" then do;
      ICD9_10Newborn=2;
      ICD9_10NewbornDiag=DIAG;
     end;
     ** adult with infant newborn diagnoses **;
     else if AGE_LAST >= 2 and put(left(DIAG),$B10I.)="B10I" then do;
      ICD9_10Newborn=1;
      ICD9_10NewbornDiag=DIAG;
     end;
    end;

    ** CC assignment depends on diagnosis version code **;
    ** ICD9_10 age/sex edits, returns updated array pointer CC (string), passes SEX as 1/2 **;
    ** use AGE_AT_DIAGNOSIS for MCE edit, use AGE_LAST for CC (RTI) edit **;
    CC="9999";
    ** ICD9 **;
    if DIAGNOSIS_VERSION_CODE="9" then do;
     %if "&I9EMACRO" ne "" %then %&I9EMACRO(AGERTI=AGE_LAST,AGEMCE=AGE_AT_DIAGNOSIS,SEX=SEX_1_2,ICD9=DIAG); 
     %FIND_CCS(9);
    end;
    ** ICD10 **;
    else if DIAGNOSIS_VERSION_CODE="0" then do;
     ** for debugging **;
     *file print; *put;
     %if "&I0EMACRO" ne "" %then %&I0EMACRO(AGERTI=AGE_LAST,AGEMCE=AGE_AT_DIAGNOSIS,SEX=SEX_1_2,ICD0=DIAG); 
     %FIND_CCS(0);
    end;

    **=========================================================================**;
    ** lookup unsuccessful **;
    if CC in("-1.0") then do;
     &MSG16 put "WARNING: [Msg16] Diagnosis lookup failed, diagnosis ignored " @71 &IDVAR= DIAG= AGE_LAST= AGE_AT_DIAGNOSIS= SEX_1_2=;
    end;

   end; /*no errors, attempt lookup*/

  end; /*if InDiagnosis*/

  **=========================================================================**;
  ** if last record for enrollee:                                            **;
  **  - create age/sex variables (macro &AGESEXMAC)                          **;
  **  - create HCCs using hierarchies (macro &HIERMAC)                       **;
  **  - set HCCs to zero if there are no diagnoses for enrollee              **;
  **  - apply validity filters                                               **;
  **  - create additional model-specific variables                           **;
  **          - create scores for models                                     **;
  **=========================================================================**;
  if last.&IDVAR then do;
   ** create age/sex variables **;
   if "&AGESEXMAC" ne "" then do;
    %&AGESEXMAC; 
   end;

   ** at least one diagnosis, even if rejected as invalid **;
   if InDiagnosis then do;
    ** map CCs to HCCs, apply HCC hierarchies **;
    if "&HIERMAC" ne "" then do;
     %&HIERMAC; 
    end;
   end;

   ** no diagnoses, zero all CCs and HCCs **;
   else do i=1 to dim(_HCC);
    _HCC(i)=0;
    _C(i)=0;
   end;

   ** remap payment model subset to HHS HCCs & CCs with different numbering **;
   do i=1 to dim(_HCC_PM);
    _HHS_HCC_PM(i)=_HCC_PM(i);
    _HHS_CC_PM(i) =_CC_PM(i);
   end;

   **========================================================================**;
   ** enrollee validity filters - note these are performed LAST              **;
   **========================================================================**;
   DeleteRecord=0;

   ** confirm values, bypass enrollee if not compliant with every filter **;
   if missing(&IDVAR) then do;
    &MSG18 put "WARNING: [Msg18] Missing &IDVAR, enrollee rejected " @71 &IDVAR=;
    DeleteRecord=1;
   end;

   ** M/1 and F/2 are valid, upcase is forced above **;
   if not(SEX in("M","F","1","2")) then do;
    &MSG19 put "WARNING: [Msg19] Invalid SEX, enrollee rejected " @71 &IDVAR= SEX=;
    DeleteRecord=1;
   end;

   if missing(DOB) then do;
    &MSG20 put "WARNING: [Msg20] Invalid DOB, enrollee rejected " @71 &IDVAR= OriginalDOB=;
    DeleteRecord=1;
   end;

   if (AGE_LAST ne int(AGE_LAST)) or (AGE_LAST < 0) then do;
    &MSG21 put "WARNING: [Msg21] Invalid AGE_LAST, enrollee rejected " @71 &IDVAR= AGE_LAST=;
    DeleteRecord=1;
   end;

   ** enrollee metal level: plat, gold, silv, bron, cata, already upcased **;
   if not(METAL in("P","G","S","B","C")) then do;
    &MSG22 put "WARNING: [Msg22] Invalid METAL, enrollee rejected " @71 &IDVAR= METAL=;
    DeleteRecord=1;
   end;

   ** enrollee cost sharing reduction indicator **;
   if not(CSR_INDICATOR in(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) then do;
    &MSG23 put "WARNING: [Msg23] Invalid CSR_INDICATOR, enrollee rejected " @71 &IDVAR= 
      CSR_INDICATOR=;
    DeleteRecord=1;
   end;

   ** HHS HCCs must be 0/1 **;
   BadHCC=0;
   do i=1 to dim(_HHS_HCC_PM);
    if _HHS_HCC_PM(i) not in(0,1) then do;
     BadHCC=1;
     leave;
    end;
   end;
   if BadHCC=1 then do;
    &MSG24 put "WARNING: [Msg24] Failed HHS HCC filter, enrollee rejected " @71 &IDVAR=;
    DeleteRecord=1;
   end;

   ** FLAG adults/children with both completed pregnancy & newborn recs    **;
   ** to warn of an enrollee having a mix of mother & infant records       **;
   ** this only traps ONE diagnosis so DIAG code is not printed            **;
   if ((AGE_LAST >= 2) and 
       (HHS_HCC207=1 or HHS_HCC208=1 or HHS_HCC209=1) and 
       (ICD9_10Newborn=1)) then do;
    &MSG25 put "WARNING: [Msg25] Possible bundled mother/infant claim(s) -- mother" 
      @71 &IDVAR= AGE_LAST= HHS_HCC207= HHS_HCC208= HHS_HCC209= /*ICD9_10NewbornDiag=*/;
    ** this does NOT set DeleteRecord to 1 **;
   end;

   ** FLAG infants with both completed pregnancy & newborn recs            **;
   ** to warn of an enrollee having a mix of mother & infant records       **;
   ** this only traps ONE diagnosis so DIAG code is not printed            **;
   if ((AGE_LAST < 2) and 
       (HHS_HCC242=1 or HHS_HCC243=1 or HHS_HCC244=1 or HHS_HCC245=1 or HHS_HCC246=1 or 
        HHS_HCC247=1 or HHS_HCC248=1 or HHS_HCC249=1) and 
       (ICD9_10Newborn=2)) then do;
    &MSG25 put "WARNING: [Msg25] Possible bundled mother/infant claim(s) -- infant" 
      @71 &IDVAR= AGE_LAST= HHS_HCC242= HHS_HCC243= HHS_HCC244= HHS_HCC245= 
      HHS_HCC246= HHS_HCC247= HHS_HCC248= HHS_HCC249= /*ICD9_10NewbornDiag=*/;
    ** this does NOT set DeleteRecord to 1 **;
   end;
 
   ** continue if compliant with all validity filters **;
   if DeleteRecord=0;

   **========================================================================**;
   ** ADULT enrollee scores                                                  **;
   **========================================================================**;
   if AGE_LAST >= 21 then do;
    %GENERATE_ADULT_VARIABLES;
    %SCOREV3(ADULT);
   end;

   **========================================================================**;
   ** CHILD enrollee scores                                                  **;
   **========================================================================**;
   else if 2 <= AGE_LAST <=  20 then do;
    %GENERATE_CHILD_VARIABLES;
    %SCOREV3(CHILD);
   end;

   **========================================================================**;
   ** INFANT enrollee scores                                                 **;
   **========================================================================**;
   else if 0 <= AGE_LAST <=  1 then do;
    %GENERATE_INFANT_VARIABLES;
    %SCOREV3(INFANT);
   end;

   ** count HCCs **;
   NumberOfHCCs=0;
   do i=1 to dim(_HHS_HCC_PM);
    if _HHS_HCC_PM(i)=1 then NumberOfHCCs+1;
   end;

   ** write ONE record per enrollee **;
   output &OUTDATA;

  end; /*if last.IDVAR*/

 end; /*if InPerson*/

 **==========================================================================**;
 ** variable formats                                                         **;
 **==========================================================================**;
 format SCORE: CSR_: comma9.3 CSR_INDICATOR 2. DOB DIAGNOSIS_SERVICE_DATE date9.;

 **==========================================================================**;
 ** variable labels                                                          **;
 **==========================================================================**;
 label
  &IDVAR                     = "Unique enrollee identifier"
  SEX                        = "Sex ([1 or M]ale, [2 or F]emale)"
  SEX_1_2                    = "Sex (1=male, 2=female)"
  DOB                        = "Enrollee`s date of birth"
  OriginalDOB                = "Enrollee`s date of birth (as input)"
  AGE_LAST =
    "Age (in integer years) as of last day of enrollment in benefit year"
  METAL = 
    "Enrollee plan level: P[lat.], G[old], S[ilver], B[ronze], C[atastroph.]"
  CSR_INDICATOR =
    "Enrollees qualified for cost-sharing reductions have values 1-10, else 0"

  CSR_ADJ_SCR_ADULT =
    "Adult enrollee`s cost-sharing reduction adjusted risk score"
  CSR_ADJ_SCR_CHILD =
    "Child enrollee`s cost-sharing reduction adjusted risk score"
  CSR_ADJ_SCR_INFANT =
    "Infant enrollee`s cost-sharing reduction adjusted risk score"

  CSR_ADJ_SCR_ADULT_PLATINUM =
    "Adult platinum enrollee`s cost-sharing reduction adjusted risk score"
  CSR_ADJ_SCR_ADULT_GOLD =
    "Adult gold enrollee`s cost-sharing reduction adjusted risk score"
  CSR_ADJ_SCR_ADULT_SILVER =
    "Adult silver enrollee`s cost-sharing reduction adjusted risk score"
  CSR_ADJ_SCR_ADULT_BRONZE =
    "Adult bronze enrollee`s cost-sharing reduction adjusted risk score"
  CSR_ADJ_SCR_ADULT_CATASTROPHIC =
    "Adult catastrophic enrollee`s cost-sharing reduction adjusted risk score"

  CSR_ADJ_SCR_CHILD_PLATINUM =
    "Child platinum enrollee`s cost-sharing reduction adjusted risk score"
  CSR_ADJ_SCR_CHILD_GOLD =
    "Child gold enrollee`s cost-sharing reduction adjusted risk score"
  CSR_ADJ_SCR_CHILD_SILVER =
    "Child silver enrollee`s cost-sharing reduction adjusted risk score"
  CSR_ADJ_SCR_CHILD_BRONZE =
    "Child bronze enrollee`s cost-sharing reduction adjusted risk score"
  CSR_ADJ_SCR_CHILD_CATASTROPHIC =
    "Child catastrophic enrollee`s cost-sharing reduction adjusted risk score"

  CSR_ADJ_SCR_INFANT_PLATINUM =
    "Infant platinum enrollee`s cost-sharing reduction adjusted risk score"
  CSR_ADJ_SCR_INFANT_GOLD =
    "Infant gold enrollee`s cost-sharing reduction adjusted risk score"
  CSR_ADJ_SCR_INFANT_SILVER =
    "Infant silver enrollee`s cost-sharing reduction adjusted risk score"
  CSR_ADJ_SCR_INFANT_BRONZE =
    "Infant bronze enrollee`s cost-sharing reduction adjusted risk score"
  CSR_ADJ_SCR_INFANT_CATASTROPHIC =
    "Infant catastroph. enrollee`s cost-sharing reduction adjusted risk score"

  SCORE_ADULT                = "Adult enrollee`s risk score"
  SCORE_CHILD                = "Child enrollee`s risk score"
  SCORE_INFANT               = "Infant enrollee`s risk score"

  SCORE_ADULT_PLATINUM       = "Adult platinum enrollee`s risk score"
  SCORE_ADULT_GOLD           = "Adult gold enrollee`s risk score"
  SCORE_ADULT_SILVER         = "Adult silver enrollee`s risk score"
  SCORE_ADULT_BRONZE         = "Adult bronze enrollee`s risk score"
  SCORE_ADULT_CATASTROPHIC   = "Adult catastrophic enrollee`s risk score"

  SCORE_CHILD_PLATINUM       = "Child platinum enrollee`s risk score"
  SCORE_CHILD_GOLD           = "Child gold enrollee`s risk score"
  SCORE_CHILD_SILVER         = "Child silver enrollee`s risk score"
  SCORE_CHILD_BRONZE         = "Child bronze enrollee`s risk score"
  SCORE_CHILD_CATASTROPHIC   = "Child catastrophic enrollee`s risk score"

  SCORE_INFANT_PLATINUM      = "Infant platinum enrollee`s risk score"
  SCORE_INFANT_GOLD          = "Infant gold enrollee`s risk score"
  SCORE_INFANT_SILVER        = "Infant silver enrollee`s risk score"
  SCORE_INFANT_BRONZE        = "Infant bronze enrollee`s risk score"
  SCORE_INFANT_CATASTROPHIC  = "Infant catastroph. enrollee`s risk score"

  NumberOfHCCs               = "Number of non-zero HCCs"

  FAGE_LAST_0_0              = "Female,  0 <= AGE_LAST <=  0"
  FAGE_LAST_1_1              = "Female,  1 <= AGE_LAST <=  1"
  FAGE_LAST_2_4              = "Female,  2 <= AGE_LAST <=  4"
  FAGE_LAST_5_9              = "Female,  5 <= AGE_LAST <=  9"     
  FAGE_LAST_10_14            = "Female, 10 <= AGE_LAST <= 14"          
  FAGE_LAST_15_20            = "Female, 15 <= AGE_LAST <= 20"          
  FAGE_LAST_21_24            = "Female, 21 <= AGE_LAST <= 24"          
  FAGE_LAST_25_29            = "Female, 25 <= AGE_LAST <= 29"  
  FAGE_LAST_30_34            = "Female, 30 <= AGE_LAST <= 34"          
  FAGE_LAST_35_39            = "Female, 35 <= AGE_LAST <= 39"          
  FAGE_LAST_40_44            = "Female, 40 <= AGE_LAST <= 44"          
  FAGE_LAST_45_49            = "Female, 45 <= AGE_LAST <= 49"          
  FAGE_LAST_50_54            = "Female, 50 <= AGE_LAST <= 54"          
  FAGE_LAST_55_59            = "Female, 55 <= AGE_LAST <= 59"          
  FAGE_LAST_60_GT            = "Female, 60 <= AGE_LAST      "    

  MAGE_LAST_0_0              = "Male,    0 <= AGE_LAST <=  0"
  MAGE_LAST_1_1              = "Male,    1 <= AGE_LAST <=  1"
  MAGE_LAST_2_4              = "Male,    2 <= AGE_LAST <=  4"
  MAGE_LAST_5_9              = "Male,    5 <= AGE_LAST <=  9"     
  MAGE_LAST_10_14            = "Male,   10 <= AGE_LAST <= 14"          
  MAGE_LAST_15_20            = "Male,   15 <= AGE_LAST <= 20"          
  MAGE_LAST_21_24            = "Male,   21 <= AGE_LAST <= 24"          
  MAGE_LAST_25_29            = "Male,   25 <= AGE_LAST <= 29"  
  MAGE_LAST_30_34            = "Male,   30 <= AGE_LAST <= 34"          
  MAGE_LAST_35_39            = "Male,   35 <= AGE_LAST <= 39"          
  MAGE_LAST_40_44            = "Male,   40 <= AGE_LAST <= 44"          
  MAGE_LAST_45_49            = "Male,   45 <= AGE_LAST <= 49"          
  MAGE_LAST_50_54            = "Male,   50 <= AGE_LAST <= 54"          
  MAGE_LAST_55_59            = "Male,   55 <= AGE_LAST <= 59"          
  MAGE_LAST_60_GT            = "Male,   60 <= AGE_LAST      "   
  Age0_Male                  = "Male, AGE_LAST = 0          " 
  Age1_Male                  = "Male, AGE_LAST = 1          " 

  AGE_AT_DIAGNOSIS                   = "Age (in FLOOR integer years) when diagnosis was collected"
  DIAG                               = "ICD-9 diagnosis code"
  DIAGNOSIS_SERVICE_DATE             = "Date diagnosis was collected"
  OriginalDIAGNOSIS_SERVICE_DATE     = "Date diagnosis was collected (as input)"     
  DIAGNOSIS_VERSION_CODE             = "9 for ICD-9"

  Extremely_Immature_x_Severity5     = "1 if IHCC_Severity5 and IHCC_Extremely_Immature, else 0"
  Immature_x_Severity5               = "1 if IHCC_Severity5 and IHCC_Immature, else 0"
  Premature_Multiples_x_Severity5    = "1 if IHCC_Severity5 and IHCC_Premature_Multiples, else 0"
  Term_x_Severity5                   = "1 if IHCC_Severity5 and IHCC_Term, else 0"
  Age1_x_Severity5                   = "1 if IHCC_Severity5 and IHCC_Age1, else 0"
  Extremely_Immature_x_Severity4     = "1 if IHCC_Severity4 and IHCC_Extremely_Immature, else 0"
  Immature_x_Severity4               = "1 if IHCC_Severity4 and IHCC_Immature, else 0"
  Premature_Multiples_x_Severity4    = "1 if IHCC_Severity4 and IHCC_Premature_Multiples, else 0"
  Term_x_Severity4                   = "1 if IHCC_Severity4 and IHCC_Term, else 0"
  Age1_x_Severity4                   = "1 if IHCC_Severity4 and IHCC_Age1, else 0"
  Extremely_Immature_x_Severity3     = "1 if IHCC_Severity3 and IHCC_Extremely_Immature, else 0"
  Immature_x_Severity3               = "1 if IHCC_Severity3 and IHCC_Immature, else 0"
  Premature_Multiples_x_Severity3    = "1 if IHCC_Severity3 and IHCC_Premature_Multiples, else 0"
  Term_x_Severity3                   = "1 if IHCC_Severity3 and IHCC_Term, else 0"
  Age1_x_Severity3                   = "1 if IHCC_Severity3 and IHCC_Age1, else 0"
  Extremely_Immature_x_Severity2     = "1 if IHCC_Severity2 and IHCC_Extremely_Immature, else 0"
  Immature_x_Severity2               = "1 if IHCC_Severity2 and IHCC_Immature, else 0"
  Premature_Multiples_x_Severity2    = "1 if IHCC_Severity2 and IHCC_Premature_Multiples, else 0"
  Term_x_Severity2                   = "1 if IHCC_Severity2 and IHCC_Term, else 0"
  Age1_x_Severity2                   = "1 if IHCC_Severity2 and IHCC_Age1, else 0"
  Extremely_Immature_x_Severity1     = "1 if IHCC_Severity1 and IHCC_Extremely_Immature, else 0"
  Immature_x_Severity1               = "1 if IHCC_Severity1 and IHCC_Immature, else 0"
  Premature_Multiples_x_Severity1    = "1 if IHCC_Severity1 and IHCC_Premature_Multiples, else 0"
  Term_x_Severity1                   = "1 if IHCC_Severity1 and IHCC_Term, else 0"
  Age1_x_Severity1                   = "1 if IHCC_Severity1 and IHCC_Age1, else 0"

  IHCC_Extremely_Immature            = "Infant maturity level - extremely immature "
  IHCC_Immature                      = "Infant maturity level - immature           "
  IHCC_Premature_Multiples           = "Infant maturity level - premature multiples"
  IHCC_Term                          = "Infant maturity level - term               "
  IHCC_Age1                          = "Infant maturity level - age 1              "
      
  IHCC_Severity5                     = "Highest infant HCC severity level        - 5"      
  IHCC_Severity4                     = "Second highest infant HCC severity level - 4"     
  IHCC_Severity3                     = "Middle infant HCC severity level         - 3"     
  IHCC_Severity2                     = "Second lowest infant HCC severity level  - 2"     
  IHCC_Severity1                     = "Lowest infant HCC severity level         - 1"   

  SEVERE_V3                          = "Adult severe illness 0/1 marker"
  SEVERE_V3_x_G03                    = "1 if adult severe illness and group G03, else 0"
  SEVERE_V3_x_G06                    = "1 if adult severe illness and group G06, else 0"
  SEVERE_V3_x_G08                    = "1 if adult severe illness and group G08, else 0"
  SEVERE_V3_x_HHS_HCC006             = "1 if adult severe illness and HHS_HCC006, else 0"
  SEVERE_V3_x_HHS_HCC008             = "1 if adult severe illness and HHS_HCC008, else 0"
  SEVERE_V3_x_HHS_HCC009             = "1 if adult severe illness and HHS_HCC009, else 0"
  SEVERE_V3_x_HHS_HCC010             = "1 if adult severe illness and HHS_HCC010, else 0"
  SEVERE_V3_x_HHS_HCC035             = "1 if adult severe illness and HHS_HCC035, else 0"
  SEVERE_V3_x_HHS_HCC038             = "1 if adult severe illness and HHS_HCC038, else 0"
  SEVERE_V3_x_HHS_HCC115             = "1 if adult severe illness and HHS_HCC115, else 0"
  SEVERE_V3_x_HHS_HCC135             = "1 if adult severe illness and HHS_HCC135, else 0"
  SEVERE_V3_x_HHS_HCC145             = "1 if adult severe illness and HHS_HCC145, else 0"
  SEVERE_V3_x_HHS_HCC153             = "1 if adult severe illness and HHS_HCC153, else 0"
  SEVERE_V3_x_HHS_HCC154             = "1 if adult severe illness and HHS_HCC154, else 0"
  SEVERE_V3_x_HHS_HCC163             = "1 if adult severe illness and HHS_HCC163, else 0"
  SEVERE_V3_x_HHS_HCC253             = "1 if adult severe illness and HHS_HCC253, else 0"

  INT_GROUP_H                        = "1 if adult high cost interaction, else 0"
  INT_GROUP_M                        = "1 if adult medium cost interaction, else 0"

  BadHCC                = "0/1 flag set if HCC remapping failed"  
  DeleteRecord          = "0/1 flag set if enrollee record is to be rejected"  
  DeltaAge              = "Used to test out-of-range condition, DOB vs. AGE_AT_DIAGNOSIS"  
  I                     = "Utility array index"  
  K                     = "Utility array index"  
  ICD9_10Newborn        = "0/1 flag set if bundled claim indicated"  
  ICD9_10NewbornDiag    = "Diagnosis when bundled claim indicated"  
  _tage                 = "Utility variable"  
  _tsex                 = "Utility variable"  
  CC                    = "Utility variable, array pointer"  
  IND                   = "Utility variable";

 /* HHS HCC labels */
 %V04127L1;

 ** labels for 343 x 5 = 1715 calibration coefficients read from coefficients dataset **;
 ** and other utility variables that can be references by a macro or range            **;
 attrib 
  %ALL_COEFFICIENTS_LIST 
   label="Calibration coefficient from coefficients dataset, length must be 8"
  %HCC_FULL_LIST
   label="Utility 0/1 variable, length can be 3"
  %CC_FULL_LIST
   label="Utility 0/1 variable, length can be 3"
  G01-G04 G06-G18 G02A
   label="Utility 0/1 variable, length can be 3";
run;

**==========================================================================**;
** contents and data dump                                                   **;
**==========================================================================**;
title2 "Output to SAS dataset &outdata";
title3 " ";

proc contents order=ignorecase data=&OUTDATA; run;

proc print heading=h u data=&OUTDATA(obs=5); 
 id &IDVAR;
 var &KEEPVAR; run;

 %mend V0416F1M;
