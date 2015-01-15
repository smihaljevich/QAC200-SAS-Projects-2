/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Thursday, January 15, 2015     TIME: 2:17:22 PM
PROJECT: mihaljevichs_SAS_project_011515
PROJECT PATH: P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp
---------------------------------------- */

/* Library assignment for Local.SEAN */
Libname SEAN BASE 'P:\QAC\qac200\students\smihaljevich\Assignments' ;
/* Library assignment for Local.SEAN */
Libname SEAN BASE 'P:\QAC\qac200\students\smihaljevich\Assignments' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (SEAN)   */
%LET _CLIENTTASKLABEL='Assign Project Library (SEAN)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
LIBNAME SEAN BASE "P:\QAC\qac200\students\smihaljevich\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Correct Marital Status Variable   */
LIBNAME EC100004 "P:\QAC\qac200\students\smihaljevich\Assignments";


%LET _CLIENTTASKLABEL='Correct Marital Status Variable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(SEAN.MEPS_FULLYR_ER_MERGED_FINAL_2);

PROC SQL;
   CREATE TABLE SEAN.MEPS_FULLYR_ER_MERGED_FINAL_2(label="MEPS_FULLYR_ER_MERGED_FINAL_2") AS 
   SELECT t1.DUPERSID, 
          t1.NUMBER_ER_VISITS, 
          t1.CATEGORICAL_ER_VISITS, 
          t1.COUNT_of_DUPERSID, 
          t1.INFULLYR, 
          t1.DUPERSID1, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.SAQELIG, 
          t1.ADPRX42, 
          t1.EDRECODE, 
          t1.ADILCR42, 
          t1.EDRECODE_RECODED, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.SAQWT12F, 
          t1.FAMS1231, 
          t1.PROXY12, 
          t1.INTVLANG, 
          t1.ELGRND12, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.MNHLTH31, 
          t1.MNHLTH42, 
          t1.MNHLTH53, 
          t1.HIBPDX, 
          t1.CHDDX, 
          t1.ANGIDX, 
          t1.MIDX, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CHOLDX, 
          t1.CANCERDX, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.PREGNT31, 
          t1.PREGNT42, 
          t1.PREGNT53, 
          t1.BMINDX53, 
          t1.LANGHM42, 
          t1.ENGCMF42, 
          t1.ENGSPK42, 
          t1.USBORN42, 
          t1.USLIVE42, 
          t1.HAVEUS42, 
          t1.YNOUSC42, 
          t1.PROVTY42, 
          t1.PLCTYP42, 
          t1.TYPEPE42, 
          t1.LANGPR42, 
          t1.MDUNAB42, 
          t1.DPOTSD12, 
          t1.TTLP12X, 
          t1.FAMINC12, 
          t1.POVCAT12, 
          t1.POVLEV12, 
          t1.UNINS12, 
          t1.INSCOV12, 
          t1.INSURC12, 
          t1.TRIST12X, 
          t1.TRIPR12X, 
          t1.TRIEX12X, 
          t1.TRILI12X, 
          t1.TRICH12X, 
          t1.MCRPD12, 
          t1.MCRPD12X, 
          t1.MCRPB12, 
          t1.MCRPHO12, 
          t1.MCDHMO12, 
          t1.MCDMC12, 
          t1.PRVHMO12, 
          t1.PRVMNC12, 
          t1.PRVDRL12, 
          t1.PHMONP12, 
          t1.PMNCNP12, 
          t1.PRDRNP12, 
          t1.TRICR12X, 
          t1.TRIAT12X, 
          t1.MCAID12, 
          t1.MCAID12X, 
          t1.MCARE12, 
          t1.MCARE12X, 
          t1.MCDAT12X, 
          t1.OTPAAT12, 
          t1.OTPBAT12, 
          t1.OTPUBA12, 
          t1.OTPUBB12, 
          t1.PRIDK12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PRIVAT12, 
          t1.PROUT12, 
          t1.PUB12X, 
          t1.PUBAT12X, 
          t1.INS12X, 
          t1.INSAT12X, 
          t1.STAPR12, 
          t1.STPRAT12, 
          t1.DNTINS12, 
          t1.PMDINS12, 
          t1.PMEDUP31, 
          t1.PMEDUP42, 
          t1.PMEDUP53, 
          t1.PMEDPY31, 
          t1.PMEDPY42, 
          t1.PMEDPY53, 
          t1.PMEDPP31, 
          t1.PMEDPP42, 
          t1.PMEDPP53, 
          t1.TOTTCH12, 
          t1.TOTEXP12, 
          t1.TOTSLF12, 
          t1.TOTMCR12, 
          t1.TOTMCD12, 
          t1.TOTPRV12, 
          t1.TOTVA12, 
          t1.TOTTRI12, 
          t1.TOTOFD12, 
          t1.TOTSTL12, 
          t1.TOTWCP12, 
          t1.TOTOPR12, 
          t1.TOTOPU12, 
          t1.TOTOSR12, 
          t1.TOTPTR12, 
          t1.TOTOTH12, 
          t1.ERTOT12, 
          t1.IPZERO12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.RXTOT12, 
          t1.PERWT12F, 
          t1.HEALTH_GENERAL, 
          t1.HEALTH_LIMITS_ACTIVITY, 
          t1.HEALTH_LIMITS_STAIRS, 
          t1.ACCOMPLISH_PHYSICAL_PROBLEMS, 
          t1.WORK_LIMIT_PHYSICAL_PROBLEMS, 
          t1.ACCOMPLISH_MENTAL_PROBLEMS, 
          t1.WORK_LIMIT_MENTAL_PROBLEMS, 
          t1.PAIN_LIMIT_WORK, 
          t1.CALM, 
          t1.ENERGY, 
          t1.DEPRESSED, 
          t1.HEALTH_SOCIAL, 
          t1.EDUCYR_RECODED, 
          t1.EDUYRDEG_RECODED, 
          t1.MARITAL_STATUS, 
          t1.EMPLOY_STATUS_31, 
          t1.EMPLOY_STATUS_42, 
          t1.EMPLOY_STATUS_53, 
          t1.NOT_NEED_H_INSURANCE, 
          t1.HEALTH_WORTH_COST, 
          t1.LIKELY_RISK, 
          t1.ILLS_NO_HELP, 
          t1.SAQINTERVIEW_LANG, 
          t1.SPEC_REFERRAL, 
          t1.SEE_SPECIALIST, 
          t1.CHECK_BLOOD_PRESSURE, 
          t1.SMOKE, 
          t1.RATE_HEALTH_CARE, 
          t1.PERC_HEALTH_31, 
          t1.PERC_HEALTH_42, 
          t1.PERC_HEALTH_53, 
          t1.BMINDX_RECODED, 
          t1.ENGSPK_RECODED, 
          t1.PLCTYP_RECODED, 
          t1.TYPEPE_RECODED, 
          t1.PROVIDER_TYPE, 
          t1.HAVE_USC, 
          t1.WHY_NO_USC, 
          t1.HEALTH_GENERAL_REVERSED, 
          t1.PAIN_LIMIT_WORK_REVERSED, 
          t1.CALM_REVERSED, 
          t1.ENERGY_REVERSED, 
          t1.PERC_HEALTH_31_REVERSED, 
          t1.PERC_HEALTH_42_REVERSED, 
          t1.PERC_HEALTH_53_REVERSED, 
          t1.AGGREGATE_HEALTH_SCORE, 
          t1.AGGREGATE_PERCEIVED_HEALTH, 
          t1.CATEGORICAL_AGGREGATE_HEALTH, 
          t1.CATEGORICAL_EDUCATION, 
          t1.CATEGORICAL_PERCEIVED_HEALTH, 
          t1.INER, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID11, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F1, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.XRAY_RECODED, 
          t1.MRI_RECODED, 
          /* CATEGORICAL_MARITAL_STATUS */
            (t1.'CATEGORICAL_MARITAL STATUS'n) LABEL="Correction for CATEGORIAL_MARITAL STATUS" AS 
            CATEGORICAL_MARITAL_STATUS
      FROM EC100004.meps_fullyr_er_merged_final t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Filter by Age   */
%LET _CLIENTTASKLABEL='Filter by Age';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
%LET AgePrompt_min = 18;
%LET AgePrompt_max = 75;

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_ AS 
   SELECT t2.DUPERSID, 
          t2.NUMBER_ER_VISITS, 
          t2.CATEGORICAL_ER_VISITS, 
          t2.COUNT_of_DUPERSID, 
          t2.INFULLYR, 
          t2.DUPERSID1, 
          t2.AGE12X, 
          t2.SEX, 
          t2.REGION12, 
          t2.RACETHX, 
          t2.MARRY12X, 
          t2.EDUCYR, 
          t2.EDUYRDEG, 
          t2.EMPST31, 
          t2.EMPST42, 
          t2.EMPST53, 
          t2.SAQELIG, 
          t2.ADPRX42, 
          t2.EDRECODE, 
          t2.ADILCR42, 
          t2.EDRECODE_RECODED, 
          t2.ADILWW42, 
          t2.ADRTCR42, 
          t2.ADRTWW42, 
          t2.ADAPPT42, 
          t2.ADNDCR42, 
          t2.ADEGMC42, 
          t2.ADLIST42, 
          t2.ADEXPL42, 
          t2.ADRESP42, 
          t2.ADPRTM42, 
          t2.ADINST42, 
          t2.ADEZUN42, 
          t2.ADTLHW42, 
          t2.ADFFRM42, 
          t2.ADFHLP42, 
          t2.ADHECR42, 
          t2.ADSMOK42, 
          t2.ADNSMK42, 
          t2.ADDRBP42, 
          t2.ADSPEC42, 
          t2.ADSPRF42, 
          t2.ADGENH42, 
          t2.ADDAYA42, 
          t2.ADCLIM42, 
          t2.ADPALS42, 
          t2.ADPWLM42, 
          t2.ADMALS42, 
          t2.ADMWLM42, 
          t2.ADPAIN42, 
          t2.ADCAPE42, 
          t2.ADNRGY42, 
          t2.ADDOWN42, 
          t2.ADSOCA42, 
          t2.PCS42, 
          t2.MCS42, 
          t2.SFFLAG42, 
          t2.ADNERV42, 
          t2.ADHOPE42, 
          t2.ADREST42, 
          t2.ADSAD42, 
          t2.ADEFRT42, 
          t2.ADWRTH42, 
          t2.K6SUM42, 
          t2.ADINTR42, 
          t2.ADDPRS42, 
          t2.PHQ242, 
          t2.ADINSA42, 
          t2.ADINSB42, 
          t2.ADRISK42, 
          t2.ADOVER42, 
          t2.ADCMPM42, 
          t2.ADCMPD42, 
          t2.ADCMPY42, 
          t2.ADLANG42, 
          t2.SAQWT12F, 
          t2.FAMS1231, 
          t2.PROXY12, 
          t2.INTVLANG, 
          t2.ELGRND12, 
          t2.RTHLTH31, 
          t2.RTHLTH42, 
          t2.RTHLTH53, 
          t2.MNHLTH31, 
          t2.MNHLTH42, 
          t2.MNHLTH53, 
          t2.HIBPDX, 
          t2.CHDDX, 
          t2.ANGIDX, 
          t2.MIDX, 
          t2.OHRTDX, 
          t2.STRKDX, 
          t2.EMPHDX, 
          t2.CHOLDX, 
          t2.CANCERDX, 
          t2.DIABDX, 
          t2.ARTHDX, 
          t2.ASTHDX, 
          t2.PREGNT31, 
          t2.PREGNT42, 
          t2.PREGNT53, 
          t2.BMINDX53, 
          t2.LANGHM42, 
          t2.ENGCMF42, 
          t2.ENGSPK42, 
          t2.USBORN42, 
          t2.USLIVE42, 
          t2.HAVEUS42, 
          t2.YNOUSC42, 
          t2.PROVTY42, 
          t2.PLCTYP42, 
          t2.TYPEPE42, 
          t2.LANGPR42, 
          t2.MDUNAB42, 
          t2.DPOTSD12, 
          t2.TTLP12X, 
          t2.FAMINC12, 
          t2.POVCAT12, 
          t2.POVLEV12, 
          t2.UNINS12, 
          t2.INSCOV12, 
          t2.INSURC12, 
          t2.TRIST12X, 
          t2.TRIPR12X, 
          t2.TRIEX12X, 
          t2.TRILI12X, 
          t2.TRICH12X, 
          t2.MCRPD12, 
          t2.MCRPD12X, 
          t2.MCRPB12, 
          t2.MCRPHO12, 
          t2.MCDHMO12, 
          t2.MCDMC12, 
          t2.PRVHMO12, 
          t2.PRVMNC12, 
          t2.PRVDRL12, 
          t2.PHMONP12, 
          t2.PMNCNP12, 
          t2.PRDRNP12, 
          t2.TRICR12X, 
          t2.TRIAT12X, 
          t2.MCAID12, 
          t2.MCAID12X, 
          t2.MCARE12, 
          t2.MCARE12X, 
          t2.MCDAT12X, 
          t2.OTPAAT12, 
          t2.OTPBAT12, 
          t2.OTPUBA12, 
          t2.OTPUBB12, 
          t2.PRIDK12, 
          t2.PRIEU12, 
          t2.PRING12, 
          t2.PRIOG12, 
          t2.PRIS12, 
          t2.PRIV12, 
          t2.PRIVAT12, 
          t2.PROUT12, 
          t2.PUB12X, 
          t2.PUBAT12X, 
          t2.INS12X, 
          t2.INSAT12X, 
          t2.STAPR12, 
          t2.STPRAT12, 
          t2.DNTINS12, 
          t2.PMDINS12, 
          t2.PMEDUP31, 
          t2.PMEDUP42, 
          t2.PMEDUP53, 
          t2.PMEDPY31, 
          t2.PMEDPY42, 
          t2.PMEDPY53, 
          t2.PMEDPP31, 
          t2.PMEDPP42, 
          t2.PMEDPP53, 
          t2.TOTTCH12, 
          t2.TOTEXP12, 
          t2.TOTSLF12, 
          t2.TOTMCR12, 
          t2.TOTMCD12, 
          t2.TOTPRV12, 
          t2.TOTVA12, 
          t2.TOTTRI12, 
          t2.TOTOFD12, 
          t2.TOTSTL12, 
          t2.TOTWCP12, 
          t2.TOTOPR12, 
          t2.TOTOPU12, 
          t2.TOTOSR12, 
          t2.TOTPTR12, 
          t2.TOTOTH12, 
          t2.ERTOT12, 
          t2.IPZERO12, 
          t2.IPDIS12, 
          t2.IPNGTD12, 
          t2.RXTOT12, 
          t2.PERWT12F, 
          t2.HEALTH_GENERAL, 
          t2.HEALTH_LIMITS_ACTIVITY, 
          t2.HEALTH_LIMITS_STAIRS, 
          t2.ACCOMPLISH_PHYSICAL_PROBLEMS, 
          t2.WORK_LIMIT_PHYSICAL_PROBLEMS, 
          t2.ACCOMPLISH_MENTAL_PROBLEMS, 
          t2.WORK_LIMIT_MENTAL_PROBLEMS, 
          t2.PAIN_LIMIT_WORK, 
          t2.CALM, 
          t2.ENERGY, 
          t2.DEPRESSED, 
          t2.HEALTH_SOCIAL, 
          t2.EDUCYR_RECODED, 
          t2.EDUYRDEG_RECODED, 
          t2.MARITAL_STATUS, 
          t2.EMPLOY_STATUS_31, 
          t2.EMPLOY_STATUS_42, 
          t2.EMPLOY_STATUS_53, 
          t2.NOT_NEED_H_INSURANCE, 
          t2.HEALTH_WORTH_COST, 
          t2.LIKELY_RISK, 
          t2.ILLS_NO_HELP, 
          t2.SAQINTERVIEW_LANG, 
          t2.SPEC_REFERRAL, 
          t2.SEE_SPECIALIST, 
          t2.CHECK_BLOOD_PRESSURE, 
          t2.SMOKE, 
          t2.RATE_HEALTH_CARE, 
          t2.PERC_HEALTH_31, 
          t2.PERC_HEALTH_42, 
          t2.PERC_HEALTH_53, 
          t2.BMINDX_RECODED, 
          t2.ENGSPK_RECODED, 
          t2.PLCTYP_RECODED, 
          t2.TYPEPE_RECODED, 
          t2.PROVIDER_TYPE, 
          t2.HAVE_USC, 
          t2.WHY_NO_USC, 
          t2.HEALTH_GENERAL_REVERSED, 
          t2.PAIN_LIMIT_WORK_REVERSED, 
          t2.CALM_REVERSED, 
          t2.ENERGY_REVERSED, 
          t2.PERC_HEALTH_31_REVERSED, 
          t2.PERC_HEALTH_42_REVERSED, 
          t2.PERC_HEALTH_53_REVERSED, 
          t2.AGGREGATE_HEALTH_SCORE, 
          t2.AGGREGATE_PERCEIVED_HEALTH, 
          t2.CATEGORICAL_AGGREGATE_HEALTH, 
          t2.CATEGORICAL_EDUCATION, 
          t2.CATEGORICAL_PERCEIVED_HEALTH, 
          t2.INER, 
          t2.DUID, 
          t2.PID, 
          t2.DUPERSID11, 
          t2.EVNTIDX, 
          t2.EVENTRN, 
          t2.ERHEVIDX, 
          t2.FFEEIDX, 
          t2.PANEL, 
          t2.MPCDATA, 
          t2.ERDATEYR, 
          t2.ERDATEMM, 
          t2.ERDATEDD, 
          t2.SEEDOC, 
          t2.VSTCTGRY, 
          t2.VSTRELCN, 
          t2.LABTEST, 
          t2.SONOGRAM, 
          t2.XRAYS, 
          t2.MAMMOG, 
          t2.MRI, 
          t2.EKG, 
          t2.EEG, 
          t2.RCVVAC, 
          t2.ANESTH, 
          t2.THRTSWAB, 
          t2.OTHSVCE, 
          t2.SURGPROC, 
          t2.MEDPRESC, 
          t2.ERICD1X, 
          t2.ERICD2X, 
          t2.ERICD3X, 
          t2.ERPRO1X, 
          t2.ERCCC1X, 
          t2.ERCCC2X, 
          t2.ERCCC3X, 
          t2.FFERTYPE, 
          t2.FFBEF12, 
          t2.ERXP12X, 
          t2.ERTC12X, 
          t2.ERFSF12X, 
          t2.ERFMR12X, 
          t2.ERFMD12X, 
          t2.ERFPV12X, 
          t2.ERFVA12X, 
          t2.ERFTR12X, 
          t2.ERFOF12X, 
          t2.ERFSL12X, 
          t2.ERFWC12X, 
          t2.ERFOR12X, 
          t2.ERFOU12X, 
          t2.ERFOT12X, 
          t2.ERFXP12X, 
          t2.ERFTC12X, 
          t2.ERDSF12X, 
          t2.ERDMR12X, 
          t2.ERDMD12X, 
          t2.ERDPV12X, 
          t2.ERDVA12X, 
          t2.ERDTR12X, 
          t2.ERDOF12X, 
          t2.ERDSL12X, 
          t2.ERDWC12X, 
          t2.ERDOR12X, 
          t2.ERDOU12X, 
          t2.ERDOT12X, 
          t2.ERDXP12X, 
          t2.ERDTC12X, 
          t2.IMPFLAG, 
          t2.PERWT12F1, 
          t2.VARSTR, 
          t2.VARPSU, 
          t2.XRAY_RECODED, 
          t2.MRI_RECODED, 
          t2.CATEGORICAL_MARITAL_STATUS
      FROM SEAN.MEPS_FULLYR_ER_MERGED_FINAL_2 t2
      WHERE %_eg_WhereParam( t2.AGE12X, AgePrompt, BETWEEN, TYPE=N, IS_EXPLICIT=0 );
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL AgePrompt_min;
%SYMDEL AgePrompt_max;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:46 PM
   By task: One-Way Frequencies

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.AGE12X, T.SEX, T.MRI_RECODED, T.XRAY_RECODED
	FROM WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_ as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES MRI_RECODED / MISSPRINT  SCORES=TABLE;
	TABLES XRAY_RECODED / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Generate categorical age variable   */
%LET _CLIENTTASKLABEL='Generate categorical age variable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000 AS 
   SELECT t1.DUPERSID, 
          t1.NUMBER_ER_VISITS, 
          t1.CATEGORICAL_ER_VISITS, 
          t1.COUNT_of_DUPERSID, 
          t1.INFULLYR, 
          t1.DUPERSID1, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.SAQELIG, 
          t1.ADPRX42, 
          t1.EDRECODE, 
          t1.ADILCR42, 
          t1.EDRECODE_RECODED, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.SAQWT12F, 
          t1.FAMS1231, 
          t1.PROXY12, 
          t1.INTVLANG, 
          t1.ELGRND12, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.MNHLTH31, 
          t1.MNHLTH42, 
          t1.MNHLTH53, 
          t1.HIBPDX, 
          t1.CHDDX, 
          t1.ANGIDX, 
          t1.MIDX, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CHOLDX, 
          t1.CANCERDX, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.PREGNT31, 
          t1.PREGNT42, 
          t1.PREGNT53, 
          t1.BMINDX53, 
          t1.LANGHM42, 
          t1.ENGCMF42, 
          t1.ENGSPK42, 
          t1.USBORN42, 
          t1.USLIVE42, 
          t1.HAVEUS42, 
          t1.YNOUSC42, 
          t1.PROVTY42, 
          t1.PLCTYP42, 
          t1.TYPEPE42, 
          t1.LANGPR42, 
          t1.MDUNAB42, 
          t1.DPOTSD12, 
          t1.TTLP12X, 
          t1.FAMINC12, 
          t1.POVCAT12, 
          t1.POVLEV12, 
          t1.UNINS12, 
          t1.INSCOV12, 
          t1.INSURC12, 
          t1.TRIST12X, 
          t1.TRIPR12X, 
          t1.TRIEX12X, 
          t1.TRILI12X, 
          t1.TRICH12X, 
          t1.MCRPD12, 
          t1.MCRPD12X, 
          t1.MCRPB12, 
          t1.MCRPHO12, 
          t1.MCDHMO12, 
          t1.MCDMC12, 
          t1.PRVHMO12, 
          t1.PRVMNC12, 
          t1.PRVDRL12, 
          t1.PHMONP12, 
          t1.PMNCNP12, 
          t1.PRDRNP12, 
          t1.TRICR12X, 
          t1.TRIAT12X, 
          t1.MCAID12, 
          t1.MCAID12X, 
          t1.MCARE12, 
          t1.MCARE12X, 
          t1.MCDAT12X, 
          t1.OTPAAT12, 
          t1.OTPBAT12, 
          t1.OTPUBA12, 
          t1.OTPUBB12, 
          t1.PRIDK12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PRIVAT12, 
          t1.PROUT12, 
          t1.PUB12X, 
          t1.PUBAT12X, 
          t1.INS12X, 
          t1.INSAT12X, 
          t1.STAPR12, 
          t1.STPRAT12, 
          t1.DNTINS12, 
          t1.PMDINS12, 
          t1.PMEDUP31, 
          t1.PMEDUP42, 
          t1.PMEDUP53, 
          t1.PMEDPY31, 
          t1.PMEDPY42, 
          t1.PMEDPY53, 
          t1.PMEDPP31, 
          t1.PMEDPP42, 
          t1.PMEDPP53, 
          t1.TOTTCH12, 
          t1.TOTEXP12, 
          t1.TOTSLF12, 
          t1.TOTMCR12, 
          t1.TOTMCD12, 
          t1.TOTPRV12, 
          t1.TOTVA12, 
          t1.TOTTRI12, 
          t1.TOTOFD12, 
          t1.TOTSTL12, 
          t1.TOTWCP12, 
          t1.TOTOPR12, 
          t1.TOTOPU12, 
          t1.TOTOSR12, 
          t1.TOTPTR12, 
          t1.TOTOTH12, 
          t1.ERTOT12, 
          t1.IPZERO12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.RXTOT12, 
          t1.PERWT12F, 
          t1.HEALTH_GENERAL, 
          t1.HEALTH_LIMITS_ACTIVITY, 
          t1.HEALTH_LIMITS_STAIRS, 
          t1.ACCOMPLISH_PHYSICAL_PROBLEMS, 
          t1.WORK_LIMIT_PHYSICAL_PROBLEMS, 
          t1.ACCOMPLISH_MENTAL_PROBLEMS, 
          t1.WORK_LIMIT_MENTAL_PROBLEMS, 
          t1.PAIN_LIMIT_WORK, 
          t1.CALM, 
          t1.ENERGY, 
          t1.DEPRESSED, 
          t1.HEALTH_SOCIAL, 
          t1.EDUCYR_RECODED, 
          t1.EDUYRDEG_RECODED, 
          t1.MARITAL_STATUS, 
          t1.EMPLOY_STATUS_31, 
          t1.EMPLOY_STATUS_42, 
          t1.EMPLOY_STATUS_53, 
          t1.NOT_NEED_H_INSURANCE, 
          t1.HEALTH_WORTH_COST, 
          t1.LIKELY_RISK, 
          t1.ILLS_NO_HELP, 
          t1.SAQINTERVIEW_LANG, 
          t1.SPEC_REFERRAL, 
          t1.SEE_SPECIALIST, 
          t1.CHECK_BLOOD_PRESSURE, 
          t1.SMOKE, 
          t1.RATE_HEALTH_CARE, 
          t1.PERC_HEALTH_31, 
          t1.PERC_HEALTH_42, 
          t1.PERC_HEALTH_53, 
          t1.BMINDX_RECODED, 
          t1.ENGSPK_RECODED, 
          t1.PLCTYP_RECODED, 
          t1.TYPEPE_RECODED, 
          t1.PROVIDER_TYPE, 
          t1.HAVE_USC, 
          t1.WHY_NO_USC, 
          t1.HEALTH_GENERAL_REVERSED, 
          t1.PAIN_LIMIT_WORK_REVERSED, 
          t1.CALM_REVERSED, 
          t1.ENERGY_REVERSED, 
          t1.PERC_HEALTH_31_REVERSED, 
          t1.PERC_HEALTH_42_REVERSED, 
          t1.PERC_HEALTH_53_REVERSED, 
          t1.AGGREGATE_HEALTH_SCORE, 
          t1.AGGREGATE_PERCEIVED_HEALTH, 
          t1.CATEGORICAL_AGGREGATE_HEALTH, 
          t1.CATEGORICAL_EDUCATION, 
          t1.CATEGORICAL_PERCEIVED_HEALTH, 
          t1.INER, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID11, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F1, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.XRAY_RECODED, 
          t1.MRI_RECODED, 
          t1.CATEGORICAL_MARITAL_STATUS, 
          /* CATEGORICAL_AGE */
            (CASE  
               WHEN AGE12X >= 18 and AGE12X <=30
               THEN 1
            WHEN AGE12X >= 31 and AGE12X <=40
               THEN 2
            WHEN AGE12X >= 41 and AGE12X <=55
               THEN 3
            WHEN AGE12X >= 56 and AGE12X <=70
               THEN 4
            WHEN AGE12X > 70
               THEN 5
               ELSE .
            END) LABEL="Categorizes AGE12X" AS CATEGORICAL_AGE
      FROM WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_ t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies9   */
%LET _CLIENTTASKLABEL='One-Way Frequencies9';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:47 PM
   By task: One-Way Frequencies9

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.CATEGORICAL_AGGREGATE_HEALTH
	FROM WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies for CATEGORICAL_AGGREGATE_HEALTH";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES CATEGORICAL_AGGREGATE_HEALTH / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:47 PM
   By task: One-Way Frequencies1

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000(KEEP=MRI_RECODED XRAY_RECODED "&GroupingPrompt"n)
	OUT=WORK.SORT
	;
	BY "&GroupingPrompt"n;
RUN;

TITLE;
TITLE1 "One-Way Frequencies for MRI and XRAY grouped by &GroupingPrompt";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MRI_RECODED / MISSPRINT  SCORES=TABLE;
	TABLES XRAY_RECODED / MISSPRINT  SCORES=TABLE;
	BY "&GroupingPrompt"n;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: One-Way Frequencies2   */
%LET _CLIENTTASKLABEL='One-Way Frequencies2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:47 PM
   By task: One-Way Frequencies2

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000(KEEP=MRI_RECODED XRAY_RECODED "&GroupingPrompt"n)
	OUT=WORK.SORT
	;
	BY "&GroupingPrompt"n;
RUN;

TITLE;
TITLE1 "One-Way Frequencies for MRI and XRAY grouped by &GroupingPrompt";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MRI_RECODED / MISSPRINT  SCORES=TABLE;
	TABLES XRAY_RECODED / MISSPRINT  SCORES=TABLE;
	BY "&GroupingPrompt"n;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: One-Way Frequencies3   */
%LET _CLIENTTASKLABEL='One-Way Frequencies3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:48 PM
   By task: One-Way Frequencies3

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000(KEEP=MRI_RECODED XRAY_RECODED "&GroupingPrompt"n)
	OUT=WORK.SORT
	;
	BY "&GroupingPrompt"n;
RUN;

TITLE;
TITLE1 "One-Way Frequencies for MRI and XRAY grouped by &GroupingPrompt";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MRI_RECODED / MISSPRINT  SCORES=TABLE;
	TABLES XRAY_RECODED / MISSPRINT  SCORES=TABLE;
	BY "&GroupingPrompt"n;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: One-Way Frequencies4   */
%LET _CLIENTTASKLABEL='One-Way Frequencies4';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:48 PM
   By task: One-Way Frequencies4

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000(KEEP=MRI_RECODED XRAY_RECODED "&GroupingPrompt"n)
	OUT=WORK.SORT
	;
	BY "&GroupingPrompt"n;
RUN;

TITLE;
TITLE1 "One-Way Frequencies for MRI and XRAY grouped by &GroupingPrompt";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MRI_RECODED / MISSPRINT  SCORES=TABLE;
	TABLES XRAY_RECODED / MISSPRINT  SCORES=TABLE;
	BY "&GroupingPrompt"n;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: One-Way Frequencies5   */
%LET _CLIENTTASKLABEL='One-Way Frequencies5';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:49 PM
   By task: One-Way Frequencies5

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000(KEEP=MRI_RECODED XRAY_RECODED "&GroupingPrompt"n)
	OUT=WORK.SORT
	;
	BY "&GroupingPrompt"n;
RUN;

TITLE;
TITLE1 "One-Way Frequencies for MRI and XRAY grouped by &GroupingPrompt";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MRI_RECODED / MISSPRINT  SCORES=TABLE;
	TABLES XRAY_RECODED / MISSPRINT  SCORES=TABLE;
	BY "&GroupingPrompt"n;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: One-Way Frequencies6   */
%LET _CLIENTTASKLABEL='One-Way Frequencies6';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:49 PM
   By task: One-Way Frequencies6

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000(KEEP=MRI_RECODED XRAY_RECODED "&GroupingPrompt"n)
	OUT=WORK.SORT
	;
	BY "&GroupingPrompt"n;
RUN;

TITLE;
TITLE1 "One-Way Frequencies for MRI and XRAY grouped by &GroupingPrompt";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MRI_RECODED / MISSPRINT  SCORES=TABLE;
	TABLES XRAY_RECODED / MISSPRINT  SCORES=TABLE;
	BY "&GroupingPrompt"n;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: One-Way Frequencies7   */
%LET _CLIENTTASKLABEL='One-Way Frequencies7';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:49 PM
   By task: One-Way Frequencies7

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000(KEEP=MRI_RECODED XRAY_RECODED "&GroupingPrompt"n)
	OUT=WORK.SORT
	;
	BY "&GroupingPrompt"n;
RUN;

TITLE;
TITLE1 "One-Way Frequencies for MRI and XRAY grouped by &GroupingPrompt";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MRI_RECODED / MISSPRINT  SCORES=TABLE;
	TABLES XRAY_RECODED / MISSPRINT  SCORES=TABLE;
	BY "&GroupingPrompt"n;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: Summary Statistics   */
%LET _CLIENTTASKLABEL='Summary Statistics';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:50 PM
   By task: Summary Statistics

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000(FIRSTOBS=1  KEEP=AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS "&GroupingPrompt"n)
	OUT=WORK.SORTTempTableSorted
	;
	BY "&GroupingPrompt"n;
RUN;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results by &GroupingPrompt";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	NOLABELS
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N 
		NMISS	
		Q1 
		MEDIAN 
		Q3	;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS;
	BY "&GroupingPrompt"n;

RUN;
ODS GRAPHICS ON;
TITLE;
/*-----------------------------------------------------
 * Use PROC UNIVARIATE to generate the histograms.
 */

TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Histograms by &GroupingPrompt";
PROC UNIVARIATE DATA=WORK.SORTTempTableSorted	NOPRINT	;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS;

		BY "&GroupingPrompt"n;
	HISTOGRAM ;

RUN; QUIT;
ODS GRAPHICS OFF;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: Summary Statistics   */
%LET _CLIENTTASKLABEL='Summary Statistics';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:50 PM
   By task: Summary Statistics

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000(FIRSTOBS=1  KEEP=AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS "&GroupingPrompt"n)
	OUT=WORK.SORTTempTableSorted
	;
	BY "&GroupingPrompt"n;
RUN;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results by &GroupingPrompt";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	NOLABELS
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N 
		NMISS	
		Q1 
		MEDIAN 
		Q3	;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS;
	BY "&GroupingPrompt"n;

RUN;
ODS GRAPHICS ON;
TITLE;
/*-----------------------------------------------------
 * Use PROC UNIVARIATE to generate the histograms.
 */

TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Histograms by &GroupingPrompt";
PROC UNIVARIATE DATA=WORK.SORTTempTableSorted	NOPRINT	;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS;

		BY "&GroupingPrompt"n;
	HISTOGRAM ;

RUN; QUIT;
ODS GRAPHICS OFF;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: Summary Statistics   */
%LET _CLIENTTASKLABEL='Summary Statistics';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:50 PM
   By task: Summary Statistics

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000(FIRSTOBS=1  KEEP=AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS "&GroupingPrompt"n)
	OUT=WORK.SORTTempTableSorted
	;
	BY "&GroupingPrompt"n;
RUN;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results by &GroupingPrompt";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	NOLABELS
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N 
		NMISS	
		Q1 
		MEDIAN 
		Q3	;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS;
	BY "&GroupingPrompt"n;

RUN;
ODS GRAPHICS ON;
TITLE;
/*-----------------------------------------------------
 * Use PROC UNIVARIATE to generate the histograms.
 */

TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Histograms by &GroupingPrompt";
PROC UNIVARIATE DATA=WORK.SORTTempTableSorted	NOPRINT	;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS;

		BY "&GroupingPrompt"n;
	HISTOGRAM ;

RUN; QUIT;
ODS GRAPHICS OFF;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: Summary Statistics   */
%LET _CLIENTTASKLABEL='Summary Statistics';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:51 PM
   By task: Summary Statistics

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000(FIRSTOBS=1  KEEP=AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS "&GroupingPrompt"n)
	OUT=WORK.SORTTempTableSorted
	;
	BY "&GroupingPrompt"n;
RUN;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results by &GroupingPrompt";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	NOLABELS
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N 
		NMISS	
		Q1 
		MEDIAN 
		Q3	;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS;
	BY "&GroupingPrompt"n;

RUN;
ODS GRAPHICS ON;
TITLE;
/*-----------------------------------------------------
 * Use PROC UNIVARIATE to generate the histograms.
 */

TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Histograms by &GroupingPrompt";
PROC UNIVARIATE DATA=WORK.SORTTempTableSorted	NOPRINT	;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS;

		BY "&GroupingPrompt"n;
	HISTOGRAM ;

RUN; QUIT;
ODS GRAPHICS OFF;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: Summary Statistics   */
%LET _CLIENTTASKLABEL='Summary Statistics';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:51 PM
   By task: Summary Statistics

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000(FIRSTOBS=1  KEEP=AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS "&GroupingPrompt"n)
	OUT=WORK.SORTTempTableSorted
	;
	BY "&GroupingPrompt"n;
RUN;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results by &GroupingPrompt";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	NOLABELS
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N 
		NMISS	
		Q1 
		MEDIAN 
		Q3	;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS;
	BY "&GroupingPrompt"n;

RUN;
ODS GRAPHICS ON;
TITLE;
/*-----------------------------------------------------
 * Use PROC UNIVARIATE to generate the histograms.
 */

TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Histograms by &GroupingPrompt";
PROC UNIVARIATE DATA=WORK.SORTTempTableSorted	NOPRINT	;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS;

		BY "&GroupingPrompt"n;
	HISTOGRAM ;

RUN; QUIT;
ODS GRAPHICS OFF;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: Summary Statistics   */
%LET _CLIENTTASKLABEL='Summary Statistics';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:51 PM
   By task: Summary Statistics

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000(FIRSTOBS=1  KEEP=AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS "&GroupingPrompt"n)
	OUT=WORK.SORTTempTableSorted
	;
	BY "&GroupingPrompt"n;
RUN;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results by &GroupingPrompt";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	NOLABELS
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N 
		NMISS	
		Q1 
		MEDIAN 
		Q3	;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS;
	BY "&GroupingPrompt"n;

RUN;
ODS GRAPHICS ON;
TITLE;
/*-----------------------------------------------------
 * Use PROC UNIVARIATE to generate the histograms.
 */

TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Histograms by &GroupingPrompt";
PROC UNIVARIATE DATA=WORK.SORTTempTableSorted	NOPRINT	;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS;

		BY "&GroupingPrompt"n;
	HISTOGRAM ;

RUN; QUIT;
ODS GRAPHICS OFF;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: Summary Statistics   */
%LET _CLIENTTASKLABEL='Summary Statistics';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:52 PM
   By task: Summary Statistics

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000(FIRSTOBS=1  KEEP=AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS "&GroupingPrompt"n)
	OUT=WORK.SORTTempTableSorted
	;
	BY "&GroupingPrompt"n;
RUN;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results by &GroupingPrompt";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	NOLABELS
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N 
		NMISS	
		Q1 
		MEDIAN 
		Q3	;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS;
	BY "&GroupingPrompt"n;

RUN;
ODS GRAPHICS ON;
TITLE;
/*-----------------------------------------------------
 * Use PROC UNIVARIATE to generate the histograms.
 */

TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Histograms by &GroupingPrompt";
PROC UNIVARIATE DATA=WORK.SORTTempTableSorted	NOPRINT	;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH NUMBER_ER_VISITS CATEGORICAL_ER_VISITS;

		BY "&GroupingPrompt"n;
	HISTOGRAM ;

RUN; QUIT;
ODS GRAPHICS OFF;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: Filter by Health   */
%LET _CLIENTTASKLABEL='Filter by Health';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
%LET CategoricalHealthPrompt_min = 1;
%LET CategoricalHealthPrompt_max = 5;

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_ER_ME);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_ER_ME AS 
   SELECT t1.DUPERSID, 
          t1.NUMBER_ER_VISITS, 
          t1.CATEGORICAL_ER_VISITS, 
          t1.COUNT_of_DUPERSID, 
          t1.INFULLYR, 
          t1.DUPERSID1, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.SAQELIG, 
          t1.ADPRX42, 
          t1.EDRECODE, 
          t1.ADILCR42, 
          t1.EDRECODE_RECODED, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.SAQWT12F, 
          t1.FAMS1231, 
          t1.PROXY12, 
          t1.INTVLANG, 
          t1.ELGRND12, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.MNHLTH31, 
          t1.MNHLTH42, 
          t1.MNHLTH53, 
          t1.HIBPDX, 
          t1.CHDDX, 
          t1.ANGIDX, 
          t1.MIDX, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CHOLDX, 
          t1.CANCERDX, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.PREGNT31, 
          t1.PREGNT42, 
          t1.PREGNT53, 
          t1.BMINDX53, 
          t1.LANGHM42, 
          t1.ENGCMF42, 
          t1.ENGSPK42, 
          t1.USBORN42, 
          t1.USLIVE42, 
          t1.HAVEUS42, 
          t1.YNOUSC42, 
          t1.PROVTY42, 
          t1.PLCTYP42, 
          t1.TYPEPE42, 
          t1.LANGPR42, 
          t1.MDUNAB42, 
          t1.DPOTSD12, 
          t1.TTLP12X, 
          t1.FAMINC12, 
          t1.POVCAT12, 
          t1.POVLEV12, 
          t1.UNINS12, 
          t1.INSCOV12, 
          t1.INSURC12, 
          t1.TRIST12X, 
          t1.TRIPR12X, 
          t1.TRIEX12X, 
          t1.TRILI12X, 
          t1.TRICH12X, 
          t1.MCRPD12, 
          t1.MCRPD12X, 
          t1.MCRPB12, 
          t1.MCRPHO12, 
          t1.MCDHMO12, 
          t1.MCDMC12, 
          t1.PRVHMO12, 
          t1.PRVMNC12, 
          t1.PRVDRL12, 
          t1.PHMONP12, 
          t1.PMNCNP12, 
          t1.PRDRNP12, 
          t1.TRICR12X, 
          t1.TRIAT12X, 
          t1.MCAID12, 
          t1.MCAID12X, 
          t1.MCARE12, 
          t1.MCARE12X, 
          t1.MCDAT12X, 
          t1.OTPAAT12, 
          t1.OTPBAT12, 
          t1.OTPUBA12, 
          t1.OTPUBB12, 
          t1.PRIDK12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PRIVAT12, 
          t1.PROUT12, 
          t1.PUB12X, 
          t1.PUBAT12X, 
          t1.INS12X, 
          t1.INSAT12X, 
          t1.STAPR12, 
          t1.STPRAT12, 
          t1.DNTINS12, 
          t1.PMDINS12, 
          t1.PMEDUP31, 
          t1.PMEDUP42, 
          t1.PMEDUP53, 
          t1.PMEDPY31, 
          t1.PMEDPY42, 
          t1.PMEDPY53, 
          t1.PMEDPP31, 
          t1.PMEDPP42, 
          t1.PMEDPP53, 
          t1.TOTTCH12, 
          t1.TOTEXP12, 
          t1.TOTSLF12, 
          t1.TOTMCR12, 
          t1.TOTMCD12, 
          t1.TOTPRV12, 
          t1.TOTVA12, 
          t1.TOTTRI12, 
          t1.TOTOFD12, 
          t1.TOTSTL12, 
          t1.TOTWCP12, 
          t1.TOTOPR12, 
          t1.TOTOPU12, 
          t1.TOTOSR12, 
          t1.TOTPTR12, 
          t1.TOTOTH12, 
          t1.ERTOT12, 
          t1.IPZERO12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.RXTOT12, 
          t1.PERWT12F, 
          t1.HEALTH_GENERAL, 
          t1.HEALTH_LIMITS_ACTIVITY, 
          t1.HEALTH_LIMITS_STAIRS, 
          t1.ACCOMPLISH_PHYSICAL_PROBLEMS, 
          t1.WORK_LIMIT_PHYSICAL_PROBLEMS, 
          t1.ACCOMPLISH_MENTAL_PROBLEMS, 
          t1.WORK_LIMIT_MENTAL_PROBLEMS, 
          t1.PAIN_LIMIT_WORK, 
          t1.CALM, 
          t1.ENERGY, 
          t1.DEPRESSED, 
          t1.HEALTH_SOCIAL, 
          t1.EDUCYR_RECODED, 
          t1.EDUYRDEG_RECODED, 
          t1.MARITAL_STATUS, 
          t1.EMPLOY_STATUS_31, 
          t1.EMPLOY_STATUS_42, 
          t1.EMPLOY_STATUS_53, 
          t1.NOT_NEED_H_INSURANCE, 
          t1.HEALTH_WORTH_COST, 
          t1.LIKELY_RISK, 
          t1.ILLS_NO_HELP, 
          t1.SAQINTERVIEW_LANG, 
          t1.SPEC_REFERRAL, 
          t1.SEE_SPECIALIST, 
          t1.CHECK_BLOOD_PRESSURE, 
          t1.SMOKE, 
          t1.RATE_HEALTH_CARE, 
          t1.PERC_HEALTH_31, 
          t1.PERC_HEALTH_42, 
          t1.PERC_HEALTH_53, 
          t1.BMINDX_RECODED, 
          t1.ENGSPK_RECODED, 
          t1.PLCTYP_RECODED, 
          t1.TYPEPE_RECODED, 
          t1.PROVIDER_TYPE, 
          t1.HAVE_USC, 
          t1.WHY_NO_USC, 
          t1.HEALTH_GENERAL_REVERSED, 
          t1.PAIN_LIMIT_WORK_REVERSED, 
          t1.CALM_REVERSED, 
          t1.ENERGY_REVERSED, 
          t1.PERC_HEALTH_31_REVERSED, 
          t1.PERC_HEALTH_42_REVERSED, 
          t1.PERC_HEALTH_53_REVERSED, 
          t1.AGGREGATE_HEALTH_SCORE, 
          t1.AGGREGATE_PERCEIVED_HEALTH, 
          t1.CATEGORICAL_AGGREGATE_HEALTH, 
          t1.CATEGORICAL_EDUCATION, 
          t1.CATEGORICAL_PERCEIVED_HEALTH, 
          t1.INER, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID11, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F1, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.XRAY_RECODED, 
          t1.MRI_RECODED, 
          t1.CATEGORICAL_MARITAL_STATUS, 
          t1.CATEGORICAL_AGE
      FROM WORK.QUERY_FOR_MEPS_FULLYR_ER_ME_0000 t1
      WHERE %_eg_WhereParam( t1.CATEGORICAL_AGGREGATE_HEALTH, CategoricalHealthPrompt, BETWEEN, TYPE=N, IS_EXPLICIT=0 );
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL CategoricalHealthPrompt_max;
%SYMDEL CategoricalHealthPrompt_min;


/*   START OF NODE: One-Way Frequencies8   */
%LET _CLIENTTASKLABEL='One-Way Frequencies8';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 2:16:52 PM
   By task: One-Way Frequencies8

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_ME
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.CATEGORICAL_AGGREGATE_HEALTH
	FROM WORK.QUERY_FOR_MEPS_FULLYR_ER_ME as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies for CATEGORICAL_AGGREGATE_HEALTH";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES CATEGORICAL_AGGREGATE_HEALTH / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Program   */
%LET _CLIENTTASKLABEL='Program';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011515.egp';
%LET _SASPROGRAMFILE=;
%LET AgePrompt_min = 18;
%LET AgePrompt_max = 75;
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'SEX');
RUN;

GOPTIONS ACCESSIBLE;
*Program written by Sean Mihaljevich 01/15/15
*The purpose of this program is to create one-way-frequency tables for MRI and X-ray variables that are made flexible
by incorporating age and grouping prompts;

*Assignt project library;
LIBNAME SEAN BASE "P:\QAC\qac200\students\smihaljevich\Assignments";

*Create a working data set containing all of the variables in work.query_for_meps_fullyr_er_me_0000;
*Data is called in from the working data set work.query_for_meps_fullyr_er_me_0000.  The initial branch
of this project (the one stemming from Assignt Project Library) must be run for this data set to exist
and for the program to work properly.  This data set may also have already been fliterd for a certain age range, as it
follows the Filter by Age query;
data subset_program_using_prompts;
set work.query_for_meps_fullyr_er_me_0000;

keep  DUPERSID 
          NUMBER_ER_VISITS 
          CATEGORICAL_ER_VISITS 
          COUNT_of_DUPERSID 
          INFULLYR 
          DUPERSID1 
          AGE12X 
          SEX 
          REGION12 
          RACETHX 
          MARRY12X 
          EDUCYR 
          EDUYRDEG 
          EMPST31 
          EMPST42 
          EMPST53 
          SAQELIG 
          ADPRX42 
          EDRECODE 
          ADILCR42 
          EDRECODE_RECODED 
          ADILWW42 
          ADRTCR42 
          ADRTWW42 
          ADAPPT42 
          ADNDCR42 
          ADEGMC42 
          ADLIST42 
          ADEXPL42 
          ADRESP42 
          ADPRTM42 
          ADINST42 
          ADEZUN42 
          ADTLHW42 
          ADFFRM42 
          ADFHLP42 
          ADHECR42 
          ADSMOK42 
          ADNSMK42 
          ADDRBP42 
          ADSPEC42 
          ADSPRF42 
          ADGENH42 
          ADDAYA42 
          ADCLIM42 
          ADPALS42 
          ADPWLM42 
          ADMALS42 
          ADMWLM42 
          ADPAIN42 
          ADCAPE42 
          ADNRGY42 
          ADDOWN42 
          ADSOCA42 
          PCS42 
          MCS42 
          SFFLAG42 
          ADNERV42 
          ADHOPE42 
          ADREST42 
          ADSAD42 
          ADEFRT42 
          ADWRTH42 
          K6SUM42 
          ADINTR42 
          ADDPRS42 
          PHQ242 
          ADINSA42 
          ADINSB42 
          ADRISK42 
          ADOVER42 
          ADCMPM42 
          ADCMPD42 
          ADCMPY42 
          ADLANG42 
          SAQWT12F 
          FAMS1231 
          PROXY12 
          INTVLANG 
          ELGRND12 
          RTHLTH31 
          RTHLTH42 
          RTHLTH53 
          MNHLTH31 
          MNHLTH42 
          MNHLTH53 
          HIBPDX 
          CHDDX 
          ANGIDX 
          MIDX 
          OHRTDX 
          STRKDX 
          EMPHDX 
          CHOLDX 
          CANCERDX 
          DIABDX 
          ARTHDX 
          ASTHDX 
          PREGNT31 
          PREGNT42 
          PREGNT53 
          BMINDX53 
          LANGHM42 
          ENGCMF42 
          ENGSPK42 
          USBORN42 
          USLIVE42 
          HAVEUS42 
          YNOUSC42 
          PROVTY42 
          PLCTYP42 
          TYPEPE42 
          LANGPR42 
          MDUNAB42 
          DPOTSD12 
          TTLP12X 
          FAMINC12 
          POVCAT12 
          POVLEV12 
          UNINS12 
          INSCOV12 
          INSURC12 
          TRIST12X 
          TRIPR12X 
          TRIEX12X 
          TRILI12X 
          TRICH12X 
          MCRPD12 
          MCRPD12X 
          MCRPB12 
          MCRPHO12 
          MCDHMO12 
          MCDMC12 
          PRVHMO12 
          PRVMNC12 
          PRVDRL12 
          PHMONP12 
          PMNCNP12 
          PRDRNP12 
          TRICR12X 
          TRIAT12X 
          MCAID12 
          MCAID12X 
          MCARE12 
          MCARE12X 
          MCDAT12X 
          OTPAAT12 
          OTPBAT12 
          OTPUBA12 
          OTPUBB12 
          PRIDK12 
          PRIEU12 
          PRING12 
          PRIOG12 
          PRIS12 
          PRIV12 
          PRIVAT12 
          PROUT12 
          PUB12X 
          PUBAT12X 
          INS12X 
          INSAT12X 
          STAPR12 
          STPRAT12 
          DNTINS12 
          PMDINS12 
          PMEDUP31 
          PMEDUP42 
          PMEDUP53 
          PMEDPY31 
          PMEDPY42 
          PMEDPY53 
          PMEDPP31 
          PMEDPP42 
          PMEDPP53 
          TOTTCH12 
          TOTEXP12 
          TOTSLF12 
          TOTMCR12 
          TOTMCD12 
          TOTPRV12 
          TOTVA12 
          TOTTRI12 
          TOTOFD12 
          TOTSTL12 
          TOTWCP12 
          TOTOPR12 
          TOTOPU12 
          TOTOSR12 
          TOTPTR12 
          TOTOTH12 
          ERTOT12 
          IPZERO12 
          IPDIS12 
          IPNGTD12 
          RXTOT12 
          PERWT12F 
          HEALTH_GENERAL 
          HEALTH_LIMITS_ACTIVITY 
          HEALTH_LIMITS_STAIRS 
          ACCOMPLISH_PHYSICAL_PROBLEMS 
          WORK_LIMIT_PHYSICAL_PROBLEMS 
          ACCOMPLISH_MENTAL_PROBLEMS 
          WORK_LIMIT_MENTAL_PROBLEMS 
          PAIN_LIMIT_WORK 
          CALM 
          ENERGY 
          DEPRESSED 
          HEALTH_SOCIAL 
          EDUCYR_RECODED 
          EDUYRDEG_RECODED 
          MARITAL_STATUS 
          EMPLOY_STATUS_31 
          EMPLOY_STATUS_42 
          EMPLOY_STATUS_53 
          NOT_NEED_H_INSURANCE 
          HEALTH_WORTH_COST 
          LIKELY_RISK 
          ILLS_NO_HELP 
          SAQINTERVIEW_LANG 
          SPEC_REFERRAL 
          SEE_SPECIALIST 
          CHECK_BLOOD_PRESSURE 
          SMOKE 
          RATE_HEALTH_CARE 
          PERC_HEALTH_31 
          PERC_HEALTH_42 
          PERC_HEALTH_53 
          BMINDX_RECODED 
          ENGSPK_RECODED 
          PLCTYP_RECODED 
          TYPEPE_RECODED 
          PROVIDER_TYPE 
          HAVE_USC 
          WHY_NO_USC 
          HEALTH_GENERAL_REVERSED 
          PAIN_LIMIT_WORK_REVERSED 
          CALM_REVERSED 
          ENERGY_REVERSED 
          PERC_HEALTH_31_REVERSED 
          PERC_HEALTH_42_REVERSED 
          PERC_HEALTH_53_REVERSED 
          AGGREGATE_HEALTH_SCORE 
          AGGREGATE_PERCEIVED_HEALTH 
          CATEGORICAL_AGGREGATE_HEALTH 
          CATEGORICAL_MARITAL_STATUS 
          CATEGORICAL_EDUCATION 
          CATEGORICAL_PERCEIVED_HEALTH 
          INER 
          DUID 
          PID 
          DUPERSID11 
          EVNTIDX 
          EVENTRN 
          ERHEVIDX 
          FFEEIDX 
          PANEL 
          MPCDATA 
          ERDATEYR 
          ERDATEMM 
          ERDATEDD 
          SEEDOC 
          VSTCTGRY 
          VSTRELCN 
          LABTEST 
          SONOGRAM 
          XRAYS 
          MAMMOG 
          MRI 
          EKG 
          EEG 
          RCVVAC 
          ANESTH 
          THRTSWAB 
          OTHSVCE 
          SURGPROC 
          MEDPRESC 
          ERICD1X 
          ERICD2X 
          ERICD3X 
          ERPRO1X 
          ERCCC1X 
          ERCCC2X 
          ERCCC3X 
          FFERTYPE 
          FFBEF12 
          ERXP12X 
          ERTC12X 
          ERFSF12X 
          ERFMR12X 
          ERFMD12X 
          ERFPV12X 
          ERFVA12X 
          ERFTR12X 
          ERFOF12X 
          ERFSL12X 
          ERFWC12X 
          ERFOR12X 
          ERFOU12X 
          ERFOT12X 
          ERFXP12X 
          ERFTC12X 
          ERDSF12X 
          ERDMR12X 
          ERDMD12X 
          ERDPV12X 
          ERDVA12X 
          ERDTR12X 
          ERDOF12X 
          ERDSL12X 
          ERDWC12X 
          ERDOR12X 
          ERDOU12X 
          ERDOT12X 
          ERDXP12X 
          ERDTC12X 
          IMPFLAG 
          PERWT12F1 
          VARSTR 
          VARPSU 
          XRAY_RECODED 
          MRI_RECODED
		CATEGORICAL_AGE;

*Sort by the prompt GroupingPrompt.  This is necessary to create grouped frequency tables;
PROC SORT
	DATA=subset_program_using_prompts;
	BY &GroupingPrompt;
RUN;

*Create one-way-frequency tables for XRAY_RECODED and MRI_RECODED.  The frequency tables are filtered by age using
 the AgePrompt, which allows one to select a range of ages between 18 and 75.  They are also grouped using GroupingPrompt, 
which allows the user to group observations by the variable CATEGORICAL_AGE, AGE12X, SEX, REGION12, RACETHX, 
CATEGORICAL_MARITAL_STATUS, or CATEGORICAL_EDUCATION.  When the program runs the user is asked to specify the 
desired age range and grouping variable.;
proc freq
data=subset_program_using_prompts
ORDER =internal;
WHERE AGE12X >= &AgePrompt_min and age12x<=&AgePrompt_max;
TITLE "One-Way-Frequencies for X-rays and MRIs for ages &AgePrompt_min to &AgePrompt_max grouped by &GroupingPrompt";
Table XRAY_RECODED /
	MISSPRINT
	Scores=TABLE;
Table MRI_RECODED /
	MISSPRINT
	Scores=TABLE;
By &GroupingPrompt;
run;

quit;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;
%SYMDEL AgePrompt_min;
%SYMDEL AgePrompt_max;
%SYMDEL GroupingPrompt;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
