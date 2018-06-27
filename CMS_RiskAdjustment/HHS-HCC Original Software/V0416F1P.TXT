*******************************************************************************;
* This distribution is:       V0416.127.L2, October 2015
*******************************************************************************;

*******************************************************************************
*For z/OS mainframe, use the following guide:
*
*//JOBCARD JOB etc.
*//HHSHCC EXEC SAS9,REGION=8M,
*// OPTIONS='ERRORS=0,NOCENTER,NEWS'
*//WORK  DD SPACE=(CYL,(1000,2))
*//WORK1 DD SPACE=(CYL,(2000,2))
*//LIBRARY DD DISP=SHR,DSN=XX.FORMATS.LIBRARY /*location of formats*/
*//IN0 DD DISP=SHR,DSN=XX.MACROS      /*location of external macros*/
*//IN1 DD DISP=SHR,DSN=XX.PERSON      /*location of person-level SAS dataset*/
*//IN2 DD DISP=SHR,DSN=XX.DIAG        /*location of diagnosis SAS dataset*/ 
*//INCOEF DD DISP=SHR,DSN=XX.COEFF    /*location of coefficients SAS dataset*/
*//OUT DD DISP=(NEW,CATLG,KEEP),      /*output SAS dataset, person-level scores*/
*//    DSN=XX.PERSON.OUTPUT, 
*//    SPACE=(TRK,(200,10),RLSE)
*//SYSIN  DD *
*
*******************************************************************************
*For MS Windows, Linux, Unix, use the following guide;
*;

LIBNAME  LIBRARY "location of formats";
FILENAME IN0     "location of external macros"; 
LIBNAME  IN1     "location of person-level SAS dataset";
LIBNAME  IN2     "location of diagnosis SAS dataset";
LIBNAME  INCOEF  "location of coefficients SAS dataset";
LIBNAME  OUT     "output SAS dataset of person-level scores";

dm "out;clear;log;clear;";

options linesize=132 errors=30 center pagesize=50 msglevel=I pageno=1
 formchar="|=|-/\+>*" nodate nomprint missing=".";

options dkricond=error dkrocond=error;

%let runday=%sysfunc(today(),date9.);
%let runtime=%sysfunc(time(),hhmm7.);
%let PROGRAM=%str(V0416F1P.SAS);

options compress=YES;

options dsoptions=nonote2err; run;

options obs=max;

title "&runday at &runtime, Pgm: &PROGRAM";
footnote;

 ***********************************************************************
 * 
 * Program V0416F1P creates 127 0/1 payment model HHS HCC variables
 * and a set of score variables for each enrollee in the PERSON
 * SAS dataset (provided by the user).
 *
 * If an enrollee has at least one diagnosis in the DIAG SAS dataset
 * (provided by the user) then HHS HCC variables are created, otherwise
 * HHS HCCs are set to 0 for the enrollee and score variables are based
 * on demographic variables (e.g., age, sex) and/or defaults.
 *
 * Score variables are created using calibration coefficients from three
 * models (Adult, Child, Infant), each with five `metal` levels
 * (Platinum, Gold, Silver, Bronze, Catastrophic):
 *
 *    1) Adult  Platinum        (AP)
 *    2) Adult  Gold            (AG)
 *    3) Adult  Silver          (AS)
 *    4) Adult  Bronze          (AB)
 *    5) Adult  Catastrophic    (AC)
 *
 *    6) Child  Platinum        (CP)
 *    7) Child  Gold            (CG)
 *    8) Child  Silver          (CS)
 *    9) Child  Bronze          (CB)
 *   10) Child  Catastrophic    (CC)
 *
 *   11) Infant Platinum        (IP)
 *   12) Infant Gold            (IG)
 *   13) Infant Silver          (IS)
 *   14) Infant Bronze          (IB)
 *   15) Infant Catastrophic    (IC).
 *
 * ICD-9-CM and ICD-10-CM diagnosis codes are accepted in this version. The
 * DIAGNOSIS_VERSION_CODE must be 9 or 0 as noted below. The diagnosis-to-CC
 * lookup tables contain both ICD-9 and ICD-10 codes.
 *
 * Both input SAS datasets, PERSON and DIAG, must be ordered by enrollee ID, i.e.,
 * the user must ensure that each dataset is sorted by the common identifier.
 * The PERSON dataset must not contain duplicate enrollee IDs. The enrollee ID 
 * is named in the user-provided macro variable &IDVAR.
 *
 * The PERSON SAS dataset must contain the following variables and values:
 *  &IDVAR        - unique enrollee ID (e.g., Medicare HICNO), character or numeric,
 *                   any valid length, not missing, no duplicates
 *  DOB           - date of birth, numeric, yyyymmdd, valid calendar date
 *  AGE_LAST      - age as of last day of enrollment in benefit year, numeric, 0/1/2/...
 *  SEX           - sex, character, M/F/m/f/1/2, if lower case automatically converted
 *  METAL         - enrollee`s plan level, character, P/G/S/B/C/p/g/s/b/c, if lower case 
 *                   automatically converted
 *  CSR_INDICATOR - cost sharing reduction indicator, numeric, 0/1/2/3/4/5/6/7/8/9/10/11/12/13
 *
 * The DIAG SAS dataset must contain the following variables and values:
 *  &IDVAR        - unique enrollee ID (e.g., Medicare HICNO), character or numeric,
 *                   any valid length, not missing
 *  DIAG          - ICD-9/10-CM diagnosis, character, 3-8 bytes, no decimal point, no
 *                   embedded blanks in code, if lower case automatically converted
 *  DIAGNOSIS_VERSION_CODE - 9 (ICD-9-CM) or 0 (ICD-10-CM), character, 1 byte
 *  DIAGNOSIS_SERVICE_DATE - date of diagnosis, numeric, yyyymmdd, valid calendar date
 *
 *  AGE_AT_DIAGNOSIS, the age as of the diagnosis service date, is calculated by the 
 *   software using DOB from the PERSON dataset and DIAGNOSIS_SERVICE_DATE from the  
 *   DIAGNOSIS dataset. It is used only for MCE diagnosis code age edits.
 *
 *  If an enrollee has N diagnoses, s/he will have N records in the DIAG dataset. If an
 *  enrollee has no diagnoses, s/he will have zero records in the DIAG dataset.
 *
 * V0416F1P passes parameters to main macro V0416F1M, which calls various internal and
 *    external macros. (A macro is a program fragment.) External macros are:
 *  I9V04ED1 - ICD9 edits based on age and/or sex
 *  I0V04ED1 - ICD10 edits based on age and/or sex
 *  AGESEXV6 - creates age/sex variables based on AGE_LAST
 *  V04127L1 - assigns labels to HHS HCCs
 *  V04127H1 - sets selected HHS HCCs to zero based on hierarchical relationships
 *  SCOREV3  - calculates score variables.
 *
 * A SAS format that maps ICD9/10s to CCs is required. The format is specified as
 * parameter CCFMT in the main macro call.
 *
 * Program steps: The program(s):
 * Include external macros. The external macros are most likely to vary
 *     among releases.
 * Define internal macro variables, formats, internal macros. The
 *     internal macros are least likely to vary among releases.
 * Merge PERSON and DIAGnosis datasets, and output one record for each
 *     enrollee record in the PERSON dataset. Output records must be fully
 *     compliant with validity rules (e.g., SEX must be M/m/F/f/1/2).
 *     Exceptions are ignored. Warning messages are generated for some 
 *     exceptions.
 * Declare lengths, retained variables, arrays
 * Append calibration coefficients
 * Merge PERSON and DIAGnosis datasets by &IDVAR
 * If there are any diagnoses for an enrollee:
 *   - perform ICD9/ICD10 edits
 *   - create additional (multiple) CCs for selected diagnoses
 *   - create age/sex variables
 *   - create HCCs with hierarchy rules
 *   - set HCCs to zero if there are no diagnoses for enrollee 
 *   - apply validity filters 
 *   - create additional model-specific variables 
 *   - create scores 
 *   - define formats, labels. 
 * Contents and data dump
 *
 * User inputs:
 * The user provides two SAS datasets with the variables described above,
 * each sorted by &IDVAR, and sets the following parameters in the macro call:
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
 * Platforms: software has been tested with SAS v9.3 on three platforms:
 *   MS Windows (Intel PC, MS Win 7), Linux (Intel server, Red Hat),
 *   z/OS (IBM mainframe).
 *
 **********************************************************************;

** housekeeping **;
proc delete data=work._all_; run;

**===========================================================================**;
** global macro variables                                                    **;
**===========================================================================**;
%global CV YY2;

** HHS HCC version, embedded in format names **;
%let CV=04;

** year, embedded in format names **;
%let YY2=16; 

** load and run primary macro **;
%inc IN0("V0416F1M.SAS") / source2;

**==========================================================================================**;
** V0416F1M goes here when testing macros inline;
**==========================================================================================**;

**==============================================================================**;
** print or suppress message -- (=blank) for print, (=*) for suppress           **;
** these are errors/warnings/notes, and are typically not                       **;
** suppressed (=blank) by the user                                              **;
** E.g., to suppress message 14, change MSG14= to MSG14=*                       **;
**==============================================================================**;

%let MSG01= ; /*ERROR  : [Msg01] Variable --- is not in --- file                                               */ 
%let MSG02= ; /*ERROR  : [Msg02] User-provided variable --- in --- file must be --- type                       */ 
%let MSG03= ; /*ERROR  : [Msg03] Duplicate IDVARs in PERSON file                                               */ 
%let MSG04= ; /*ERROR  : [Msg04] Program halted due to duplicate IDVARs in PERSON file                         */ 
%let MSG05= ; /*OK     : [Msg05] PERSON file is free of duplicate IDVARs                                       */ 
%let MSG06= ; /*ERROR  : [Msg06] Program halted due to non-existent variable(s) in PERSON file                 */ 
%let MSG07= ; /*OK     : [Msg07] PERSON file contains all requisite variables                                  */ 
%let MSG08= ; /*ERROR  : [Msg08] Program halted due to incorrect user-provided variable type(s) in PERSON file */ 
%let MSG09= ; /*OK     : [Msg09] PERSON file`s variables have the correct type                                 */ 
%let MSG10= ; /*ERROR  : [Msg10] Program halted due to non-existent variable(s) in DIAG file                   */ 
%let MSG11= ; /*OK     : [Msg11] DIAG file contains all requisite variables                                    */ 
%let MSG12= ; /*ERROR  : [Msg12] Program halted due to incorrect user-provided variable type(s) in DIAG file   */ 
%let MSG13= ; /*OK     : [Msg13] DIAG file`s variables have the correct type                                   */ 
%let MSG14= ; /*WARNING: [Msg14] Diagnosis matches no enrollee, diagnosis ignored                              */ 
%let MSG15= ; /*WARNING: [Msg15] Blank diagnosis code, diagnosis ignored                                       */ 
%let MSG18= ; /*WARNING: [Msg18] Missing IDVAR, enrollee rejected                                              */ 
%let MSG19= ; /*WARNING: [Msg19] Invalid SEX, enrollee rejected                                                */ 
%let MSG20= ; /*WARNING: [Msg20] Invalid DOB, enrollee rejected                                                */ 
%let MSG21= ; /*WARNING: [Msg21] Invalid AGE_LAST, enrollee rejected                                           */ 
%let MSG22= ; /*WARNING: [Msg22] Invalid METAL, enrollee rejected                                              */ 
%let MSG23= ; /*WARNING: [Msg23] Invalid CSR_INDICATOR, enrollee rejected                                      */ 
%let MSG24= ; /*WARNING: [Msg24] Failed HHS HCC filter, enrollee rejected                                      */ 
%let MSG26= ; /*WARNING: [Msg26] Invalid DIAGNOSIS_VERSION_CODE, diagnosis ignored                             */ 
%let MSG27= ; /*WARNING: [Msg27] Invalid DIAGNOSIS_SERVICE_DATE, diagnosis ignored                             */ 
%let MSG28= ; /*WARNING: [Msg28] Invalid AGE_AT_DIAGNOSIS, diagnosis ignored                                   */ 
%let MSG29= ; /*WARNING: [Msg29] AGE_AT_DIAGNOSIS > AGE_LAST, diagnosis ignored                                */ 
%let MSG30= ; /*ERROR  : [Msg30] Program halted, file --- does not exist                                       */ 
%let MSG31= ; /*WARNING: [Msg31] AGE_LAST minus AGE_AT_DIAGNOSIS > 1, diagnosis ignored                        */ 
%let MSG32= ; /*WARNING: [Msg32] DOB > DIAGNOSIS_SERVICE_DATE, diagnosis ignored                               */ 

**=======================================================================================**;
** these are typically suppressed (=*) by the user after testing, as they can            **;
** generate many messages. E.g., to suppress message 16, change MSG16= to MSG16=*        **;
**=======================================================================================**;
%let MSG16= ; /*WARNING: [Msg16] Diagnosis lookup failed, diagnosis ignored                                    */ 
%let MSG17= ; /*NOTE   : [Msg17] Enrollee has no diagnoses, risk score based on demographic information        */ 
%let MSG25= ; /*WARNING: [Msg25] Possible bundled mother/infant claim(s) -- ---                                */ 

**=======================================================================================**;
**=======================================================================================**;
**=======================================================================================**;

** user provides parameters below **;

%V0416F1M( INP      = IN1.PERSON,
           IND      = IN2.DIAG,
           OUTDATA  = OUT.Master,
           IDVAR    = ID,
           KEEPVAR  = DOB
                      AGE_LAST
                      SEX
                      METAL
                      CSR_INDICATOR
                      SCORE_:
                      CSR_ADJ_SCR_:
                      /*_ALL_*/,
           CCFMT    = HHS_V&CV.Y&YY2.OC,
           AGEFMT   = AGEY&YY2.MCE,
           SEXFMT   = SEXY&YY2.MCE
         ); run;

** end **;
