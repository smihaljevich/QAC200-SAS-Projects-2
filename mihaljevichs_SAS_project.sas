/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Monday, January 12, 2015     TIME: 9:41:00 AM
PROJECT: mihaljevichs_SAS_project_010915
PROJECT PATH: P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp
---------------------------------------- */

/* Library assignment for Local.SEAN */
Libname SEAN V9 'P:\QAC\qac200\students\smihaljevich\Assignments' ;
/* Library assignment for Local.SEAN */
Libname SEAN V9 'P:\QAC\qac200\students\smihaljevich\Assignments' ;


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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
LIBNAME SEAN  "P:\QAC\qac200\students\smihaljevich\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Subset of Relevant Variables Age 18 and older   */
%LET _CLIENTTASKLABEL='Subset of Relevant Variables Age 18 and older';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(SEAN.MEPS_FULLYR_2012_SUBSET);

PROC SQL;
   CREATE TABLE SEAN.MEPS_FULLYR_2012_SUBSET(label="MEPS_FULLYR_2012_SUBSET") AS 
   SELECT t1.DUPERSID, 
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
          t1.ADILCR42, 
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
          t1.PERWT12F
      FROM EC100002.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SAS program code 1   */
%LET SYSLAST=SEAN.MEPS_FULLYR_2012_SUBSET;
%LET _CLIENTTASKLABEL='SAS program code 1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';
%LET _SASPROGRAMFILE='P:\QAC\qac200\students\smihaljevich\SAS code\SAS program code 1.sas';

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 10:03:52 AM
   By task: Data Set Attributes1

   Input Data: Local:SEAN.MEPS_FULLYR_2012_SUBSET
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForMEPS_FULLYR_2012_);
TITLE "Data Set Attributes for Subset of Relevant Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";


PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=SEAN.MEPS_FULLYR_2012_SUBSET ;

RUN;



GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies for all variables in subset of relevant variables   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for all variables in subset of relevant variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:14 AM
   By task: One-Way Frequencies for all variables in subset of relevant variables

   Input Data: Local:SEAN.MEPS_FULLYR_2012_SUBSET
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:SEAN.MEPS_FULLYR_2012_SUBSET
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.AGE12X, T.SEX, T.REGION12, T.RACETHX, T.MARRY12X, T.EDUCYR, T.EDUYRDEG, T.EMPST31, T.EMPST42, T.EMPST53, T.SAQELIG, T.ADPRX42, T.ADILCR42, T.ADILWW42, T.ADRTCR42, T.ADRTWW42, T.ADAPPT42, T.ADNDCR42, T.ADEGMC42, T.ADLIST42
		     , T.ADEXPL42, T.ADRESP42, T.ADPRTM42, T.ADINST42, T.ADEZUN42, T.ADTLHW42, T.ADFFRM42, T.ADFHLP42, T.ADHECR42, T.ADSMOK42, T.ADNSMK42, T.ADDRBP42, T.ADSPEC42, T.ADSPRF42, T.ADGENH42, T.ADDAYA42, T.ADCLIM42, T.ADPALS42, T.ADPWLM42
		     , T.ADMALS42, T.ADMWLM42, T.ADPAIN42, T.ADCAPE42, T.ADNRGY42, T.ADDOWN42, T.ADSOCA42, T.PCS42, T.MCS42, T.SFFLAG42, T.ADNERV42, T.ADHOPE42, T.ADREST42, T.ADSAD42, T.ADEFRT42, T.ADWRTH42, T.K6SUM42, T.ADINTR42, T.ADDPRS42
		     , T.PHQ242, T.ADINSA42, T.ADINSB42, T.ADRISK42, T.ADOVER42, T.ADCMPM42, T.ADCMPD42, T.ADCMPY42, T.ADLANG42, T.SAQWT12F, T.FAMS1231, T.PROXY12, T.INTVLANG, T.ELGRND12, T.RTHLTH31, T.RTHLTH42, T.RTHLTH53, T.MNHLTH31, T.MNHLTH42
		     , T.MNHLTH53, T.HIBPDX, T.CHDDX, T.ANGIDX, T.MIDX, T.OHRTDX, T.STRKDX, T.EMPHDX, T.CHOLDX, T.CANCERDX, T.DIABDX, T.ARTHDX, T.ASTHDX, T.PREGNT31, T.PREGNT42, T.PREGNT53, T.BMINDX53, T.LANGHM42, T.ENGCMF42, T.ENGSPK42, T.USBORN42
		     , T.USLIVE42, T.HAVEUS42, T.YNOUSC42, T.PROVTY42, T.PLCTYP42, T.TYPEPE42, T.LANGPR42, T.MDUNAB42, T.DPOTSD12, T.TTLP12X, T.FAMINC12, T.POVCAT12, T.POVLEV12, T.UNINS12, T.INSCOV12, T.INSURC12, T.TRIST12X, T.TRIPR12X
		     , T.TRIEX12X, T.TRILI12X, T.TRICH12X, T.MCRPD12, T.MCRPD12X, T.MCRPB12, T.MCRPHO12, T.MCDHMO12, T.MCDMC12, T.PRVHMO12, T.PRVMNC12, T.PRVDRL12, T.PHMONP12, T.PMNCNP12, T.PRDRNP12, T.TRICR12X, T.TRIAT12X, T.MCAID12, T.MCAID12X
		     , T.MCARE12, T.MCARE12X, T.MCDAT12X, T.OTPAAT12, T.OTPBAT12, T.OTPUBA12, T.OTPUBB12, T.PRIDK12, T.PRIEU12, T.PRING12, T.PRIOG12, T.PRIS12, T.PRIV12, T.PRIVAT12, T.PROUT12, T.PUB12X, T.PUBAT12X, T.INS12X, T.INSAT12X, T.STAPR12
		     , T.STPRAT12, T.DNTINS12, T.PMDINS12, T.PMEDUP31, T.PMEDUP42, T.PMEDUP53, T.PMEDPY31, T.PMEDPY42, T.PMEDPY53, T.PMEDPP31, T.PMEDPP42, T.PMEDPP53, T.TOTTCH12, T.TOTEXP12, T.TOTSLF12, T.TOTMCR12, T.TOTMCD12, T.TOTPRV12, T.TOTVA12
		     , T.TOTTRI12, T.TOTOFD12, T.TOTSTL12, T.TOTWCP12, T.TOTOPR12, T.TOTOPU12, T.TOTOSR12, T.TOTPTR12, T.TOTOTH12, T.ERTOT12, T.IPZERO12, T.IPDIS12, T.IPNGTD12, T.RXTOT12, T.PERWT12F
	FROM SEAN.MEPS_FULLYR_2012_SUBSET(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for all variables in subset of relevant variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Sean Mihaljevich";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE;
	TABLES MARRY12X / MISSPRINT  SCORES=TABLE;
	TABLES EDUCYR / MISSPRINT  SCORES=TABLE;
	TABLES EDUYRDEG / MISSPRINT  SCORES=TABLE;
	TABLES EMPST31 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST42 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST53 / MISSPRINT  SCORES=TABLE;
	TABLES SAQELIG / MISSPRINT  SCORES=TABLE;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE;
	TABLES PCS42 / MISSPRINT  SCORES=TABLE;
	TABLES MCS42 / MISSPRINT  SCORES=TABLE;
	TABLES SFFLAG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE;
	TABLES K6SUM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE;
	TABLES PHQ242 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE;
	TABLES SAQWT12F / MISSPRINT  SCORES=TABLE;
	TABLES FAMS1231 / MISSPRINT  SCORES=TABLE;
	TABLES PROXY12 / MISSPRINT  SCORES=TABLE;
	TABLES INTVLANG / MISSPRINT  SCORES=TABLE;
	TABLES ELGRND12 / MISSPRINT  SCORES=TABLE;
	TABLES RTHLTH31 / MISSPRINT  SCORES=TABLE;
	TABLES RTHLTH42 / MISSPRINT  SCORES=TABLE;
	TABLES RTHLTH53 / MISSPRINT  SCORES=TABLE;
	TABLES MNHLTH31 / MISSPRINT  SCORES=TABLE;
	TABLES MNHLTH42 / MISSPRINT  SCORES=TABLE;
	TABLES MNHLTH53 / MISSPRINT  SCORES=TABLE;
	TABLES HIBPDX / MISSPRINT  SCORES=TABLE;
	TABLES CHDDX / MISSPRINT  SCORES=TABLE;
	TABLES ANGIDX / MISSPRINT  SCORES=TABLE;
	TABLES MIDX / MISSPRINT  SCORES=TABLE;
	TABLES OHRTDX / MISSPRINT  SCORES=TABLE;
	TABLES STRKDX / MISSPRINT  SCORES=TABLE;
	TABLES EMPHDX / MISSPRINT  SCORES=TABLE;
	TABLES CHOLDX / MISSPRINT  SCORES=TABLE;
	TABLES CANCERDX / MISSPRINT  SCORES=TABLE;
	TABLES DIABDX / MISSPRINT  SCORES=TABLE;
	TABLES ARTHDX / MISSPRINT  SCORES=TABLE;
	TABLES ASTHDX / MISSPRINT  SCORES=TABLE;
	TABLES PREGNT31 / MISSPRINT  SCORES=TABLE;
	TABLES PREGNT42 / MISSPRINT  SCORES=TABLE;
	TABLES PREGNT53 / MISSPRINT  SCORES=TABLE;
	TABLES BMINDX53 / MISSPRINT  SCORES=TABLE;
	TABLES LANGHM42 / MISSPRINT  SCORES=TABLE;
	TABLES ENGCMF42 / MISSPRINT  SCORES=TABLE;
	TABLES ENGSPK42 / MISSPRINT  SCORES=TABLE;
	TABLES USBORN42 / MISSPRINT  SCORES=TABLE;
	TABLES USLIVE42 / MISSPRINT  SCORES=TABLE;
	TABLES HAVEUS42 / MISSPRINT  SCORES=TABLE;
	TABLES YNOUSC42 / MISSPRINT  SCORES=TABLE;
	TABLES PROVTY42 / MISSPRINT  SCORES=TABLE;
	TABLES PLCTYP42 / MISSPRINT  SCORES=TABLE;
	TABLES TYPEPE42 / MISSPRINT  SCORES=TABLE;
	TABLES LANGPR42 / MISSPRINT  SCORES=TABLE;
	TABLES MDUNAB42 / MISSPRINT  SCORES=TABLE;
	TABLES DPOTSD12 / MISSPRINT  SCORES=TABLE;
	TABLES TTLP12X / MISSPRINT  SCORES=TABLE;
	TABLES FAMINC12 / MISSPRINT  SCORES=TABLE;
	TABLES POVCAT12 / MISSPRINT  SCORES=TABLE;
	TABLES POVLEV12 / MISSPRINT  SCORES=TABLE;
	TABLES UNINS12 / MISSPRINT  SCORES=TABLE;
	TABLES INSCOV12 / MISSPRINT  SCORES=TABLE;
	TABLES INSURC12 / MISSPRINT  SCORES=TABLE;
	TABLES TRIST12X / MISSPRINT  SCORES=TABLE;
	TABLES TRIPR12X / MISSPRINT  SCORES=TABLE;
	TABLES TRIEX12X / MISSPRINT  SCORES=TABLE;
	TABLES TRILI12X / MISSPRINT  SCORES=TABLE;
	TABLES TRICH12X / MISSPRINT  SCORES=TABLE;
	TABLES MCRPD12 / MISSPRINT  SCORES=TABLE;
	TABLES MCRPD12X / MISSPRINT  SCORES=TABLE;
	TABLES MCRPB12 / MISSPRINT  SCORES=TABLE;
	TABLES MCRPHO12 / MISSPRINT  SCORES=TABLE;
	TABLES MCDHMO12 / MISSPRINT  SCORES=TABLE;
	TABLES MCDMC12 / MISSPRINT  SCORES=TABLE;
	TABLES PRVHMO12 / MISSPRINT  SCORES=TABLE;
	TABLES PRVMNC12 / MISSPRINT  SCORES=TABLE;
	TABLES PRVDRL12 / MISSPRINT  SCORES=TABLE;
	TABLES PHMONP12 / MISSPRINT  SCORES=TABLE;
	TABLES PMNCNP12 / MISSPRINT  SCORES=TABLE;
	TABLES PRDRNP12 / MISSPRINT  SCORES=TABLE;
	TABLES TRICR12X / MISSPRINT  SCORES=TABLE;
	TABLES TRIAT12X / MISSPRINT  SCORES=TABLE;
	TABLES MCAID12 / MISSPRINT  SCORES=TABLE;
	TABLES MCAID12X / MISSPRINT  SCORES=TABLE;
	TABLES MCARE12 / MISSPRINT  SCORES=TABLE;
	TABLES MCARE12X / MISSPRINT  SCORES=TABLE;
	TABLES MCDAT12X / MISSPRINT  SCORES=TABLE;
	TABLES OTPAAT12 / MISSPRINT  SCORES=TABLE;
	TABLES OTPBAT12 / MISSPRINT  SCORES=TABLE;
	TABLES OTPUBA12 / MISSPRINT  SCORES=TABLE;
	TABLES OTPUBB12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIDK12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIEU12 / MISSPRINT  SCORES=TABLE;
	TABLES PRING12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIOG12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIS12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIV12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIVAT12 / MISSPRINT  SCORES=TABLE;
	TABLES PROUT12 / MISSPRINT  SCORES=TABLE;
	TABLES PUB12X / MISSPRINT  SCORES=TABLE;
	TABLES PUBAT12X / MISSPRINT  SCORES=TABLE;
	TABLES INS12X / MISSPRINT  SCORES=TABLE;
	TABLES INSAT12X / MISSPRINT  SCORES=TABLE;
	TABLES STAPR12 / MISSPRINT  SCORES=TABLE;
	TABLES STPRAT12 / MISSPRINT  SCORES=TABLE;
	TABLES DNTINS12 / MISSPRINT  SCORES=TABLE;
	TABLES PMDINS12 / MISSPRINT  SCORES=TABLE;
	TABLES PMEDUP31 / MISSPRINT  SCORES=TABLE;
	TABLES PMEDUP42 / MISSPRINT  SCORES=TABLE;
	TABLES PMEDUP53 / MISSPRINT  SCORES=TABLE;
	TABLES PMEDPY31 / MISSPRINT  SCORES=TABLE;
	TABLES PMEDPY42 / MISSPRINT  SCORES=TABLE;
	TABLES PMEDPY53 / MISSPRINT  SCORES=TABLE;
	TABLES PMEDPP31 / MISSPRINT  SCORES=TABLE;
	TABLES PMEDPP42 / MISSPRINT  SCORES=TABLE;
	TABLES PMEDPP53 / MISSPRINT  SCORES=TABLE;
	TABLES TOTTCH12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTSLF12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTMCR12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTMCD12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTPRV12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTVA12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTTRI12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTOFD12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTSTL12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTWCP12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTOPR12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTOPU12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTOSR12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTPTR12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTOTH12 / MISSPRINT  SCORES=TABLE;
	TABLES ERTOT12 / MISSPRINT  SCORES=TABLE;
	TABLES IPZERO12 / MISSPRINT  SCORES=TABLE;
	TABLES IPDIS12 / MISSPRINT  SCORES=TABLE;
	TABLES IPNGTD12 / MISSPRINT  SCORES=TABLE;
	TABLES RXTOT12 / MISSPRINT  SCORES=TABLE;
	TABLES PERWT12F / MISSPRINT  SCORES=TABLE;
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


/*   START OF NODE: Recode of variables for missing data codes   */
%LET _CLIENTTASKLABEL='Recode of variables for missing data codes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_MEPS_FULLYR_2012_MANAGED);

PROC SQL;
   CREATE TABLE WORK."QUERY_MEPS_FULLYR_2012_MANAGED"n AS 
   SELECT t1.DUPERSID, 
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
          t1.ADILCR42, 
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
          /* HEALTH_GENERAL */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="Health in general (recoded missing)" AS HEALTH_GENERAL, 
          /* HEALTH_LIMITS_ACTIVITY */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="Health limits moderate activity (recoded missing)" AS HEALTH_LIMITS_ACTIVITY, 
          /* HEALTH_LIMITS_STAIRS */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="Health limits climbing stairs (recoded missing)" AS HEALTH_LIMITS_STAIRS, 
          /* ACCOMPLISH_PHYSICAL_PROBLEMS */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="Accomplished less due to physical problems over 4 week span" AS ACCOMPLISH_PHYSICAL_PROBLEMS, 
          /* WORK_LIMIT_PHYSICAL_PROBLEMS */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="Work limit due to physical problems over 4 week span (recoded missing)" AS 
            WORK_LIMIT_PHYSICAL_PROBLEMS, 
          /* ACCOMPLISH_MENTAL_PROBLEMS */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="Accomplished less due to mental problems over 4 week span" AS ACCOMPLISH_MENTAL_PROBLEMS, 
          /* WORK_LIMIT_MENTAL_PROBLEMS */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="Work limit due to mental problems over 4 week span (recoded missing)" AS 
            WORK_LIMIT_MENTAL_PROBLEMS, 
          /* PAIN_LIMIT_WORK */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="Pain limits normal work over 4 week span (recoded missing)" AS PAIN_LIMIT_WORK, 
          /* CALM */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="Felt calm/peaceful over 4 week span" AS CALM, 
          /* ENERGY */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="Had lots of energy over 4 week span (recoded missing)" AS ENERGY, 
          /* DEPRESSED */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="Felt depressed over 4 week span (recoded missing)" AS DEPRESSED, 
          /* HEALTH_SOCIAL */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="Health stopped social activity over 4 week span (recoded missing)" AS HEALTH_SOCIAL, 
          /* EDUCYR_RECODED */
            (CASE 
               WHEN -1 = t1.EDUCYR THEN .
               WHEN -7 = t1.EDUCYR THEN .
               WHEN -8 = t1.EDUCYR THEN .
               WHEN -9 = t1.EDUCYR THEN .
               ELSE t1.EDUCYR
            END) LABEL="Years of education when first entered MEPS (recoded missing)" AS EDUCYR_RECODED, 
          /* EDUYRDEG_RECODED */
            (CASE 
               WHEN -1 = t1.EDUYRDEG THEN .
               WHEN -7 = t1.EDUYRDEG THEN .
               WHEN -8 = t1.EDUYRDEG THEN .
               WHEN -9 = t1.EDUYRDEG THEN .
               ELSE t1.EDUYRDEG
            END) LABEL="Years of education or highest degree  (recoded missing)" AS EDUYRDEG_RECODED, 
          /* MARITAL_STATUS */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Marital status (recoded missing)" AS MARITAL_STATUS, 
          /* EMPLOY_STATUS_31 */
            (CASE 
               WHEN -1 = t1.EMPST31 THEN .
               WHEN -7 = t1.EMPST31 THEN .
               WHEN -8 = t1.EMPST31 THEN .
               WHEN -9 = t1.EMPST31 THEN .
               ELSE t1.EMPST31
            END) LABEL="Employment status RD 3/1 (recoded missing)" AS EMPLOY_STATUS_31, 
          /* EMPLOY_STATUS_42 */
            (CASE 
               WHEN -1 = t1.EMPST42 THEN .
               WHEN -7 = t1.EMPST42 THEN .
               WHEN -8 = t1.EMPST42 THEN .
               WHEN -9 = t1.EMPST42 THEN .
               ELSE t1.EMPST42
            END) LABEL="Employment status RD 4/2 (recoded missing)" AS EMPLOY_STATUS_42, 
          /* EMPLOY_STATUS_53 */
            (CASE 
               WHEN -7 = t1.EMPST53 THEN .
               WHEN -8 = t1.EMPST53 THEN .
               WHEN -9 = t1.EMPST53 THEN .
               ELSE t1.EMPST53
            END) LABEL="Employment status RD 5/3 (recoded missing)" AS EMPLOY_STATUS_53, 
          /* NOT_NEED_H_INSURANCE */
            (CASE 
               WHEN -1 = t1.ADINSA42 THEN .
               WHEN -7 = t1.ADINSA42 THEN .
               WHEN -8 = t1.ADINSA42 THEN .
               WHEN -9 = t1.ADINSA42 THEN .
               ELSE t1.ADINSA42
            END) LABEL="Do not need health insurance (recoded missing)" AS NOT_NEED_H_INSURANCE, 
          /* HEALTH_WORTH_COST */
            (CASE 
               WHEN -1 = t1.ADINSB42 THEN .
               WHEN -7 = t1.ADINSB42 THEN .
               WHEN -8 = t1.ADINSB42 THEN .
               WHEN -9 = t1.ADINSB42 THEN .
               ELSE t1.ADINSB42
            END) LABEL="Health insurance not worth cost (recoded missing)" AS HEALTH_WORTH_COST, 
          /* LIKELY_RISK */
            (CASE 
               WHEN -1 = t1.ADRISK42 THEN .
               WHEN -7 = t1.ADRISK42 THEN .
               WHEN -8 = t1.ADRISK42 THEN .
               WHEN -9 = t1.ADRISK42 THEN .
               ELSE t1.ADRISK42
            END) LABEL="More likely to take risks (recoded missing)" AS LIKELY_RISK, 
          /* ILLS_NO_HELP */
            (CASE 
               WHEN -1 = t1.ADOVER42 THEN .
               WHEN -7 = t1.ADOVER42 THEN .
               WHEN -8 = t1.ADOVER42 THEN .
               WHEN -9 = t1.ADOVER42 THEN .
               ELSE t1.ADOVER42
            END) LABEL="Can overcome ills w/o med help (recoded missing)" AS ILLS_NO_HELP, 
          /* SAQINTERVIEW_LANG */
            (CASE 
               WHEN -1 = t1.ADLANG42 THEN .
               ELSE t1.ADLANG42
            END) LABEL="Language of SAQ interview (recoded missing)" AS SAQINTERVIEW_LANG, 
          /* SPEC_REFERRAL */
            (CASE 
               WHEN -1 = t1.ADSPRF42 THEN .
               WHEN -9 = t1.ADSPRF42 THEN .
               ELSE t1.ADSPRF42
            END) LABEL="How easy getting special referral (recoded missing)" AS SPEC_REFERRAL, 
          /* SEE_SPECIALIST */
            (CASE 
               WHEN -1 = t1.ADSPEC42 THEN .
               WHEN -9 = t1.ADSPEC42 THEN .
               ELSE t1.ADSPEC42
            END) LABEL="Need to see specialist (recoded missing)" AS SEE_SPECIALIST, 
          /* CHECK_BLOOD_PRESSURE */
            (CASE 
               WHEN -1 = t1.ADDRBP42 THEN .
               WHEN -8 = t1.ADDRBP42 THEN .
               WHEN -9 = t1.ADDRBP42 THEN .
               ELSE t1.ADDRBP42
            END) LABEL="Dr. checked blood pressure (recoded missing)" AS CHECK_BLOOD_PRESSURE, 
          /* SMOKE */
            (CASE 
               WHEN -1 = t1.ADSMOK42 THEN .
               WHEN -9 = t1.ADSMOK42 THEN .
               ELSE t1.ADSMOK42
            END) LABEL="Currently smoke (recoded missing)" AS SMOKE, 
          /* RATE_HEALTH_CARE */
            (CASE 
               WHEN -1 = t1.ADHECR42 THEN .
               WHEN -9 = t1.ADHECR42 THEN .
               ELSE t1.ADHECR42
            END) LABEL="Rating of health care (recoded missing)" AS RATE_HEALTH_CARE, 
          /* PERC_HEALTH_31 */
            (CASE 
               WHEN -1 = t1.RTHLTH31 THEN .
               WHEN -7 = t1.RTHLTH31 THEN .
               WHEN -8 = t1.RTHLTH31 THEN .
               WHEN -9 = t1.RTHLTH31 THEN .
               ELSE t1.RTHLTH31
            END) LABEL="Perceived health status RD 3/1 (recoded missing)" AS PERC_HEALTH_31, 
          /* PERC_HEALTH_42 */
            (CASE 
               WHEN -1 = t1.RTHLTH42 THEN .
               WHEN -7 = t1.RTHLTH42 THEN .
               WHEN -8 = t1.RTHLTH42 THEN .
               WHEN -9 = t1.RTHLTH42 THEN .
               ELSE t1.RTHLTH42
            END) LABEL="Perceived health status RD 4/2 (recoded missing)" AS PERC_HEALTH_42, 
          /* PERC_HEALTH_53 */
            (CASE 
               WHEN -1 = t1.RTHLTH53 THEN .
               WHEN -7 = t1.RTHLTH53 THEN .
               WHEN -8 = t1.RTHLTH53 THEN .
               WHEN -9 = t1.RTHLTH53 THEN .
               ELSE t1.RTHLTH53
            END) LABEL="Perceived health status RD 5/3 (recoded missing)" AS PERC_HEALTH_53, 
          /* BMINDX_RECODED */
            (CASE 
               WHEN -1 = t1.BMINDX53 THEN .
               WHEN -9 = t1.BMINDX53 THEN .
               ELSE t1.BMINDX53
            END) LABEL="Adult body mass index RD 5/3 (recoded missing)" AS BMINDX_RECODED, 
          /* ENGSPK_RECODED */
            (CASE 
               WHEN -1 = t1.ENGSPK42 THEN .
               WHEN -9 = t1.ENGSPK42 THEN .
               ELSE t1.ENGSPK42
            END) LABEL="Not confortable speaking english (recoded missing)" AS ENGSPK_RECODED, 
          /* PLCTYP_RECODED */
            (CASE 
               WHEN -1 = t1.PLCTYP42 THEN .
               WHEN -7 = t1.PLCTYP42 THEN .
               WHEN -8 = t1.PLCTYP42 THEN .
               WHEN -9 = t1.PLCTYP42 THEN .
               ELSE t1.PLCTYP42
            END) LABEL="USC type of place (recoded missing)" AS PLCTYP_RECODED, 
          /* TYPEPE_RECODED */
            (CASE 
               WHEN -1 = t1.TYPEPE42 THEN .
               ELSE t1.TYPEPE42
            END) LABEL="USC type of provider (recoded missing)" AS TYPEPE_RECODED, 
          /* PROVIDER_TYPE */
            (CASE 
               WHEN -1 = t1.PROVTY42 THEN .
               ELSE t1.PROVTY42
            END) LABEL="Provider type (recoded missing)" AS PROVIDER_TYPE, 
          /* HAVE_USC */
            (CASE 
               WHEN -1 = t1.HAVEUS42 THEN .
               WHEN -7 = t1.HAVEUS42 THEN .
               WHEN -8 = t1.HAVEUS42 THEN .
               WHEN -9 = t1.HAVEUS42 THEN .
               ELSE t1.HAVEUS42
            END) LABEL="Does person have USC provider (recoded missing)" AS HAVE_USC, 
          /* WHY_NO_USC */
            (CASE 
               WHEN -1 = t1.YNOUSC42 THEN .
               WHEN -8 = t1.YNOUSC42 THEN .
               WHEN -9 = t1.YNOUSC42 THEN .
               ELSE t1.YNOUSC42
            END) LABEL="Main reason person does not have USC (recoded missing)" AS WHY_NO_USC
      FROM SEAN.MEPS_FULLYR_2012_SUBSET t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:15 AM
   By task: Table Analysis

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HEALTH_GENERAL, T.ADGENH42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADGENH42 * HEALTH_GENERAL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis1   */
%LET _CLIENTTASKLABEL='Table Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:15 AM
   By task: Table Analysis1

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HEALTH_LIMITS_ACTIVITY, T.ADDAYA42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADDAYA42 * HEALTH_LIMITS_ACTIVITY /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis2   */
%LET _CLIENTTASKLABEL='Table Analysis2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:15 AM
   By task: Table Analysis2

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HEALTH_LIMITS_STAIRS, T.ADCLIM42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADCLIM42 * HEALTH_LIMITS_STAIRS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis3   */
%LET _CLIENTTASKLABEL='Table Analysis3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:15 AM
   By task: Table Analysis3

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ACCOMPLISH_PHYSICAL_PROBLEMS, T.ADPALS42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPALS42 * ACCOMPLISH_PHYSICAL_PROBLEMS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis4   */
%LET _CLIENTTASKLABEL='Table Analysis4';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:15 AM
   By task: Table Analysis4

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.WORK_LIMIT_PHYSICAL_PROBLEMS, T.ADPWLM42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPWLM42 * WORK_LIMIT_PHYSICAL_PROBLEMS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis5   */
%LET _CLIENTTASKLABEL='Table Analysis5';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:15 AM
   By task: Table Analysis5

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ACCOMPLISH_MENTAL_PROBLEMS, T.ADMALS42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADMALS42 * ACCOMPLISH_MENTAL_PROBLEMS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis6   */
%LET _CLIENTTASKLABEL='Table Analysis6';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:16 AM
   By task: Table Analysis6

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.WORK_LIMIT_MENTAL_PROBLEMS, T.ADMWLM42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADMWLM42 * WORK_LIMIT_MENTAL_PROBLEMS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis7   */
%LET _CLIENTTASKLABEL='Table Analysis7';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:16 AM
   By task: Table Analysis7

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PAIN_LIMIT_WORK, T.ADPAIN42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPAIN42 * PAIN_LIMIT_WORK /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis8   */
%LET _CLIENTTASKLABEL='Table Analysis8';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:16 AM
   By task: Table Analysis8

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CALM, T.ADCAPE42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADCAPE42 * CALM /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis9   */
%LET _CLIENTTASKLABEL='Table Analysis9';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:16 AM
   By task: Table Analysis9

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ENERGY, T.ADNRGY42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADNRGY42 * ENERGY /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis10   */
%LET _CLIENTTASKLABEL='Table Analysis10';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:16 AM
   By task: Table Analysis10

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DEPRESSED, T.ADDOWN42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADDOWN42 * DEPRESSED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis11   */
%LET _CLIENTTASKLABEL='Table Analysis11';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:16 AM
   By task: Table Analysis11

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HEALTH_SOCIAL, T.ADSOCA42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADSOCA42 * HEALTH_SOCIAL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis12   */
%LET _CLIENTTASKLABEL='Table Analysis12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:17 AM
   By task: Table Analysis12

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARITAL_STATUS, T.MARRY12X
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MARRY12X * MARITAL_STATUS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis13   */
%LET _CLIENTTASKLABEL='Table Analysis13';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:17 AM
   By task: Table Analysis13

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDUCYR_RECODED, T.EDUCYR
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EDUCYR * EDUCYR_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis14   */
%LET _CLIENTTASKLABEL='Table Analysis14';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:17 AM
   By task: Table Analysis14

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDUYRDEG_RECODED, T.EDUYRDEG
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EDUYRDEG * EDUYRDEG_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis15   */
%LET _CLIENTTASKLABEL='Table Analysis15';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:17 AM
   By task: Table Analysis15

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EMPLOY_STATUS_31, T.EMPST31
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EMPST31 * EMPLOY_STATUS_31 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis16   */
%LET _CLIENTTASKLABEL='Table Analysis16';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:17 AM
   By task: Table Analysis16

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EMPLOY_STATUS_42, T.EMPST42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EMPST42 * EMPLOY_STATUS_42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis17   */
%LET _CLIENTTASKLABEL='Table Analysis17';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:17 AM
   By task: Table Analysis17

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EMPLOY_STATUS_53, T.EMPST53
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EMPST53 * EMPLOY_STATUS_53 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis18   */
%LET _CLIENTTASKLABEL='Table Analysis18';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:17 AM
   By task: Table Analysis18

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.NOT_NEED_H_INSURANCE, T.ADINSA42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADINSA42 * NOT_NEED_H_INSURANCE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis19   */
%LET _CLIENTTASKLABEL='Table Analysis19';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:18 AM
   By task: Table Analysis19

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HEALTH_WORTH_COST, T.ADINSB42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADINSB42 * HEALTH_WORTH_COST /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis20   */
%LET _CLIENTTASKLABEL='Table Analysis20';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:18 AM
   By task: Table Analysis20

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.LIKELY_RISK, T.ADRISK42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADRISK42 * LIKELY_RISK /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis21   */
%LET _CLIENTTASKLABEL='Table Analysis21';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:18 AM
   By task: Table Analysis21

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ILLS_NO_HELP, T.ADOVER42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADOVER42 * ILLS_NO_HELP /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis22   */
%LET _CLIENTTASKLABEL='Table Analysis22';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:18 AM
   By task: Table Analysis22

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SAQINTERVIEW_LANG, T.ADLANG42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADLANG42 * SAQINTERVIEW_LANG /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis23   */
%LET _CLIENTTASKLABEL='Table Analysis23';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:18 AM
   By task: Table Analysis23

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SPEC_REFERRAL, T.ADSPRF42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADSPRF42 * SPEC_REFERRAL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis24   */
%LET _CLIENTTASKLABEL='Table Analysis24';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:18 AM
   By task: Table Analysis24

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SEE_SPECIALIST, T.ADSPEC42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADSPEC42 * SEE_SPECIALIST /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis25   */
%LET _CLIENTTASKLABEL='Table Analysis25';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:19 AM
   By task: Table Analysis25

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CHECK_BLOOD_PRESSURE, T.ADDRBP42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADDRBP42 * CHECK_BLOOD_PRESSURE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis26   */
%LET _CLIENTTASKLABEL='Table Analysis26';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:19 AM
   By task: Table Analysis26

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SMOKE, T.ADSMOK42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADSMOK42 * SMOKE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis27   */
%LET _CLIENTTASKLABEL='Table Analysis27';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:19 AM
   By task: Table Analysis27

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.RATE_HEALTH_CARE, T.ADHECR42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADHECR42 * RATE_HEALTH_CARE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis28   */
%LET _CLIENTTASKLABEL='Table Analysis28';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:19 AM
   By task: Table Analysis28

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PERC_HEALTH_31, T.RTHLTH31
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES RTHLTH31 * PERC_HEALTH_31 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis29   */
%LET _CLIENTTASKLABEL='Table Analysis29';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:19 AM
   By task: Table Analysis29

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PERC_HEALTH_42, T.RTHLTH42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES RTHLTH42 * PERC_HEALTH_42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis30   */
%LET _CLIENTTASKLABEL='Table Analysis30';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:19 AM
   By task: Table Analysis30

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PERC_HEALTH_53, T.RTHLTH53
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES RTHLTH53 * PERC_HEALTH_53 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis31   */
%LET _CLIENTTASKLABEL='Table Analysis31';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:20 AM
   By task: Table Analysis31

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.BMINDX_RECODED, T.BMINDX53
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES BMINDX53 * BMINDX_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis32   */
%LET _CLIENTTASKLABEL='Table Analysis32';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:20 AM
   By task: Table Analysis32

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ENGSPK_RECODED, T.ENGSPK42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ENGSPK42 * ENGSPK_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis33   */
%LET _CLIENTTASKLABEL='Table Analysis33';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:20 AM
   By task: Table Analysis33

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PLCTYP_RECODED, T.PLCTYP42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES PLCTYP42 * PLCTYP_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis34   */
%LET _CLIENTTASKLABEL='Table Analysis34';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:20 AM
   By task: Table Analysis34

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.TYPEPE_RECODED, T.TYPEPE42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES TYPEPE42 * TYPEPE_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis35   */
%LET _CLIENTTASKLABEL='Table Analysis35';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:20 AM
   By task: Table Analysis35

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PROVIDER_TYPE, T.PROVTY42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES PROVTY42 * PROVIDER_TYPE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis36   */
%LET _CLIENTTASKLABEL='Table Analysis36';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:20 AM
   By task: Table Analysis36

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HAVE_USC, T.HAVEUS42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES HAVEUS42 * HAVE_USC /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Table Analysis37   */
%LET _CLIENTTASKLABEL='Table Analysis37';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:21 AM
   By task: Table Analysis37

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.WHY_NO_USC, T.YNOUSC42
	FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES YNOUSC42 * WHY_NO_USC /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
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


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 9:40:22 AM
   By task: Data Set Attributes

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFormeps_fullyr_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFormeps_fullyr_2012(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFormeps_fullyr_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFormeps_fullyr_2012 OUT=WORK.CONTContentsFormeps_fullyr_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFormeps_fullyr_2012
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
