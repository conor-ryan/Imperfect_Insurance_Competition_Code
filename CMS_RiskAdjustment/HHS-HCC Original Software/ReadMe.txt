(1) Each program with a .SAS extension has a corresponding .TXT file.  

The .SAS and .TXT files are identical. The .TXT files are provided to make 
the SAS programs easier to view with some text editors. 

File names are case sensitive on some computing platforms, and software 
modules assume that file names are upper case (e.g., AGESEXV6.SAS).

---------------------

(2) The two transport files (each with extension .TRN) contain the format 
library and model coefficients dataset. 

The transport files may be used on any SAS® version 9 platform after 
uploading them and converting them using SAS® PROC CIMPORT. Program 
IMPORT.SAS is provided as an example.

If your computing platform is z/OS, both transport files should be uploaded 
using the following parameters:  RECFM(F or FB) LRECL(80) BLKSIZE(8000).

---------------------

(3) Contents:

Program components (SAS code):

1.	AGESEXV6.SAS
2.	I0V04ED1.SAS
3.	I9V04ED1.SAS
4.	SCOREV3.SAS
5.	V04127H1.SAS
6.	V04127L1.SAS
7.	V0416F1M.SAS
8.	V0416F1P.SAS – main program

Text versions of program components (ASCII text):

9.	AGESEXV6.TXT
10.	I0V04ED1.TXT
11.	I9V04ED1.TXT
12.	SCOREV3.TXT
13.	V04127H1.TXT
14.	V04127L1.TXT
15.	V0416F1M.TXT
16.	V0416F1P.TXT

Calibration coefficients (SAS transport file):

17.	C0310L1O.TRN

Diagnosis-to-CC crosswalks (SAS transport file containing SAS formats):

18.	H0416F1O.TRN

Text versions of diagnosis-to-CC crosswalks:

19.	H0416F1O.ICD10.TXT
20.	H0416F1O.ICD9.TXT
21.	H0416F1O_ICD10_MCE_age.TXT
22.	H0416F1O_ICD10_MCE_sex.TXT
23.	H0416F1O_ICD9_MCE_age.TXT
24.	H0416F1O_MCE_ICD9_sex.TXT

Sample program to read SAS transport files (SAS and text versions):

25.	IMPORT.SAS
26.	IMPORT.TXT

Documentation:

27.	Word file for this release
28.	Excel file for this release
