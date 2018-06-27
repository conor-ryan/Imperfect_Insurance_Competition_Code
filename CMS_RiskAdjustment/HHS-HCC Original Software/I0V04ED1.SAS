 %macro I0V04ED1(AGERTI=, AGEMCE=, SEX=, ICD0= );
 ** for debugging **;
 *file print; *put &AGERTI= &AGEMCE= &SEX= &ICD0=;
 %**********************************************************************
 1 MACRO NAME:  I0V04ED1
 2 PURPOSE:     age/sex edits on &ICD0 - are based on:
                1. RTI age/sex splits
                2. MCE list to check if age or sex
                 is within the range of acceptable age/sex, if not- CC 
                 is set to -1.0 - invalid
 
 3 PARAMETERS:  AGERTI- age for CC splits (=AGE_LAST) 
                AGEMCE- age for MCE edits (=AGE_AT_DIAGNOSIS) 
                SEX   - SEX
                &ICD0  - diagnosis variable in a diagnosis file
 4 COMMENTS:    1. Age format AGEFMT and sex format SEXFMT are
                   parameters in the main macro. They must correspond
                   to the years of data
                2. If &ICD0 code does not have any restriction on age
                   or sex then the corresponding format puts it in "-1"
                3. AGEL format sets lower limits for age
                   AGEU format sets upper limit for age
                   for specific edit categories:
                   "0"= "0 newborn (age 0)      "
                   "1"= "1 pediatric (age 0 -17)"
                   "2"= "2 maternity (age 12-55)"
                   "3"= "3 adult (age 14+)      "

 **********************************************************************;
 * RTI age/sex splits;
 * codes are not cross-walked to CC if age condition is not met;
 IF &AGERTI < 2  and 
      &ICD0 IN ("J430","J431","J432","J438","J439","J449","J982",
                "J983" )
   THEN CC="-1.0" ; /*exclude diagnoses of emphysema for enrollees age <2*/

   ELSE
   IF &AGERTI >= 2  and 
      &ICD0 IN ("K551","K558","K559","P041","P042","P043","P0441","P0449",
                "P045","P046","P048","P049","P930","P938","P961",
                "P962","Q390","Q391","Q392","Q393","Q394","Q6410",
                "Q6411","Q6412","Q6419","Q790","Q791","Q792","Q793",
                "Q794","Q7951" )
   THEN CC="-1.0" ; /*infant diagnoses in HCC42 Peritonitis/Gastrointestinal
 Perforation/Necrotizing Enterocolitis, HCC 64 Major Congenital Anomalies of
 Diaphragm, Abdominal Wall, and Esophagus, Age < 2, and HCC81 Drug Psychosis*/

   ELSE
   IF &AGERTI ne 0  and 
      &ICD0 IN ("P0500","P0501","P0502","P0503","P0504","P0505",
                "P0506","P0507","P0508","P0510","P0511","P0512",
                "P0513","P0514","P0515","P0516","P0517","P0518",
                "P052","P059","P0700","P0701","P0702","P0703",
                "P0710","P0714","P0715","P0716","P0717","P0718" )
   THEN CC="-1.0" ; /*edit for newborn low birthweight*/

 * CC is changed from originally assigned by format depending on age;
   IF &AGERTI < 50  and 
      &ICD0 IN ("C50011","C50012","C50019","C50021","C50022","C50029",
                "C50111","C50112","C50119","C50121","C50122","C50129",
                "C50211","C50212","C50219","C50221","C50222","C50229",
                "C50311","C50312","C50319","C50321","C50322","C50329",
                "C50411","C50412","C50419","C50421","C50422","C50429",
                "C50511","C50512","C50519","C50521","C50522","C50529",
                "C50611","C50612","C50619","C50621","C50622","C50629",
                "C50811","C50812","C50819","C50821","C50822","C50829",
                "C50911","C50912","C50919","C50921","C50922","C50929" )
   THEN CC="11" ; /*split breast cancer diagnoses to age 50+ and age <50*/

   ELSE
   IF &AGERTI >= 2  and 
      &ICD0 IN ("P270","P271","P278","P279" )
   THEN CC="162" ; /*split chronic respiratory disease arising in the perinatal
 period diagnosis to age 2+ and age <2*/

   ELSE
   IF &AGERTI < 2  and 
      &ICD0 IN ("K550" )
   THEN CC="42" ; /*split acute vascular insufficiency of intestine diagnosis
 to age 2+ and age <2*/

   ELSE
   IF &AGERTI < 18  and 
      &ICD0 IN ("J410","J411","J418","J42","J440","J441" )
   THEN CC="161" ; /*split chronic bronchitis diagnoses to age 18+ and age <18*/

   ELSE
   IF &AGERTI >= 1  and 
      &ICD0 IN ("Q894" )
   THEN CC="97" ; /*split conjoined twins diagnosis to age 1+ and age <1*/

   ELSE
   IF &AGERTI < 18  and 
      &ICD0 IN ("C7400","C7401","C7402","C7410","C7411","C7412",
                "C7490","C7491","C7492" )
   THEN CC="9" ; /*split adrenal gland cancer diagnosis to age 18+ and age <18*/

   ELSE
   IF &AGERTI < 18  and 
      &ICD0 IN ("C7400","C7401","C7402","C7410","C7411","C7412",
                "C7490","C7491","C7492",
                "C9100","C9101","C9102","C9300","C9301","C9302",
                "C9400","C9401","C9402",
                "C9420","C9421","C9422","C9440","C9441","C9442",
                "C9500","C9501","C9502" )
   THEN CC="9" ; /*split acute lymphoid and other acute leukemias, except
 myeloid diagnoses to age 18+ and age <18*/

   IF &sex="2"  and 
      &ICD0 IN ("D66","D67" )
   THEN CC="75" ; /*split hemophilia diagnosis to male and female*/

 * MCE edits;
 * check if age is within acceptable range;
 _tage=put(&ICD0, $I0&AGEFMT..);
 if _tage ne "-1" and
  (&AGEMCE < input(put(_tage, $AGEL.),8.) or
   &AGEMCE > input(put(_tage, $AGEU.),8.)) then cc="-1.0";
 * check if sex for a person is the one in the MCE file;
 _tsex=put(&ICD0, $I0&SEXFMT..);
 if _tsex ne "-1"  and _tsex ne &SEX then cc="-1.0";

 ** for debugging **;
 *file print; *if CC ne "-1.0" and CC ne "9999" then put CC=;
 %mend I0V04ED1;
