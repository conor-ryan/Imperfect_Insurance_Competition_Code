** Model coefficients **;
FILENAME INC     "user defined location of transport file C0310L1O.TRN";
LIBNAME  INCOEF  "user defined location of SAS coefficients dataset";

proc cimport infile=INC data=INCOEF.Coefficients; run;

** Format library **;
FILENAME INF     "user defined location of transport file H0416F1O.TRN";
LIBNAME LIBRARY  "user defined location of SAS format library";

proc cimport infile=INF library=LIBRARY; run;

** rename ICD-9 formats from Y14Y15 to Y16 for consistency with ICD-10 formats **;
proc catalog catalog=LIBRARY.FORMATS;
 change I9AGEY14Y15MCE.FORMATC        = I9AGEY16MCE.FORMATC; 
 change I9DUP_HHS_V04Y14Y15OC.FORMATC = I9DUP_HHS_V04Y16OC.FORMATC; 
 change I9HHS_V04Y14Y15OC.FORMATC     = I9HHS_V04Y16OC.FORMATC; 
 change I9SEXY14Y15MCE.FORMATC        = I9SEXY16MCE.FORMATC; 
quit; run;
