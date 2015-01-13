/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, January 13, 2015     TIME: 10:22:42 AM
PROJECT: mihaljevichs_SAS_project_011315
PROJECT PATH: P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
LIBNAME SEAN  "P:\QAC\qac200\students\smihaljevich\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Subset of Relevant Variables Age 18 and older   */
%LET _CLIENTTASKLABEL='Subset of Relevant Variables Age 18 and older';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

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
          t1.PERWT12F, 
          t1.EDRECODE
      FROM EC100007.meps_fullyr_2012 t1
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:09 AM
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
		     , T.TOTTRI12, T.TOTOFD12, T.TOTSTL12, T.TOTWCP12, T.TOTOPR12, T.TOTOPU12, T.TOTOSR12, T.TOTPTR12, T.TOTOTH12, T.ERTOT12, T.IPZERO12, T.IPDIS12, T.IPNGTD12, T.RXTOT12, T.PERWT12F, T.EDRECODE
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
	TABLES EDRECODE / MISSPRINT  SCORES=TABLE;
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_MEPS_FULLYR_2012_MANAGED);

PROC SQL;
   CREATE TABLE WORK.QUERY_MEPS_FULLYR_2012_MANAGED(label="QUERY_MEPS_FULLYR_2012_MANAGED") AS 
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
          t1.EDRECODE, 
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
            END) LABEL="Main reason person does not have USC (recoded missing)" AS WHY_NO_USC, 
          /* EDRECODE_RECODED */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="Recoded missing values for EDRECODE" AS EDRECODE_RECODED
      FROM SEAN.MEPS_FULLYR_2012_SUBSET t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis39   */
%LET _CLIENTTASKLABEL='Table Analysis39';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:09 AM
   By task: Table Analysis39

   Input Data: Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_MEPS_FULLYR_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGE12X, T.SEX, T.REGION12, T.RACETHX, T.MARRY12X, T.EDUCYR, T.EDUYRDEG, T.EMPST31, T.EMPST42, T.EMPST53, T.SAQELIG, T.ADPRX42, T.ADILCR42, T.ADILWW42, T.ADRTCR42, T.ADRTWW42, T.ADAPPT42, T.ADNDCR42, T.ADEGMC42, T.ADLIST42
		     , T.ADEXPL42, T.ADRESP42, T.ADPRTM42, T.ADINST42, T.ADEZUN42, T.ADTLHW42, T.ADFFRM42, T.ADFHLP42, T.ADHECR42, T.ADSMOK42, T.ADNSMK42, T.ADDRBP42, T.ADSPEC42, T.ADSPRF42, T.ADGENH42, T.ADDAYA42, T.ADCLIM42, T.ADPALS42, T.ADPWLM42
		     , T.ADMALS42, T.ADMWLM42, T.ADPAIN42, T.ADCAPE42, T.ADNRGY42, T.ADDOWN42, T.ADSOCA42, T.PCS42, T.MCS42, T.SFFLAG42, T.ADNERV42, T.ADHOPE42, T.ADREST42, T.ADSAD42, T.ADEFRT42, T.ADWRTH42, T.K6SUM42, T.ADINTR42, T.ADDPRS42
		     , T.PHQ242, T.ADINSA42, T.ADINSB42, T.ADRISK42, T.ADOVER42, T.ADCMPM42, T.ADCMPD42, T.ADCMPY42, T.ADLANG42, T.SAQWT12F, T.FAMS1231, T.PROXY12, T.INTVLANG, T.ELGRND12, T.RTHLTH31, T.RTHLTH42, T.RTHLTH53, T.MNHLTH31, T.MNHLTH42
		     , T.MNHLTH53, T.HIBPDX, T.CHDDX, T.ANGIDX, T.MIDX, T.OHRTDX, T.STRKDX, T.EMPHDX, T.CHOLDX, T.CANCERDX, T.DIABDX, T.ARTHDX, T.ASTHDX, T.PREGNT31, T.PREGNT42, T.PREGNT53, T.BMINDX53, T.LANGHM42, T.ENGCMF42, T.ENGSPK42, T.USBORN42
		     , T.USLIVE42, T.HAVEUS42, T.YNOUSC42, T.PROVTY42, T.PLCTYP42, T.TYPEPE42, T.LANGPR42, T.MDUNAB42, T.DPOTSD12, T.TTLP12X, T.FAMINC12, T.POVCAT12, T.POVLEV12, T.UNINS12, T.INSCOV12, T.INSURC12, T.TRIST12X, T.TRIPR12X
		     , T.TRIEX12X, T.TRILI12X, T.TRICH12X, T.MCRPD12, T.MCRPD12X, T.MCRPB12, T.MCRPHO12, T.MCDHMO12, T.MCDMC12, T.PRVHMO12, T.PRVMNC12, T.PRVDRL12, T.PHMONP12, T.PMNCNP12, T.PRDRNP12, T.TRICR12X, T.TRIAT12X, T.MCAID12, T.MCAID12X
		     , T.MCARE12, T.MCARE12X, T.MCDAT12X, T.OTPAAT12, T.OTPBAT12, T.OTPUBA12, T.OTPUBB12, T.PRIDK12, T.PRIEU12, T.PRING12, T.PRIOG12, T.PRIS12, T.PRIV12, T.PRIVAT12, T.PROUT12, T.PUB12X, T.PUBAT12X, T.INS12X, T.INSAT12X, T.STAPR12
		     , T.STPRAT12, T.DNTINS12, T.PMDINS12, T.PMEDUP31, T.PMEDUP42, T.PMEDUP53, T.PMEDPY31, T.PMEDPY42, T.PMEDPY53, T.PMEDPP31, T.PMEDPP42, T.PMEDPP53, T.TOTTCH12, T.TOTEXP12, T.TOTSLF12, T.TOTMCR12, T.TOTMCD12, T.TOTPRV12, T.TOTVA12
		     , T.TOTTRI12, T.TOTOFD12, T.TOTSTL12, T.TOTWCP12, T.TOTOPR12, T.TOTOPU12, T.TOTOSR12, T.TOTPTR12, T.TOTOTH12, T.ERTOT12, T.IPZERO12, T.IPDIS12, T.IPNGTD12, T.RXTOT12, T.PERWT12F, T.HEALTH_GENERAL, T.HEALTH_LIMITS_ACTIVITY
		     , T.HEALTH_LIMITS_STAIRS, T.ACCOMPLISH_PHYSICAL_PROBLEMS, T.WORK_LIMIT_PHYSICAL_PROBLEMS, T.ACCOMPLISH_MENTAL_PROBLEMS, T.WORK_LIMIT_MENTAL_PROBLEMS, T.PAIN_LIMIT_WORK, T.CALM, T.ENERGY, T.DEPRESSED, T.HEALTH_SOCIAL
		     , T.EDUCYR_RECODED, T.EDUYRDEG_RECODED, T.MARITAL_STATUS, T.EMPLOY_STATUS_31, T.EMPLOY_STATUS_42, T.EMPLOY_STATUS_53, T.NOT_NEED_H_INSURANCE, T.HEALTH_WORTH_COST, T.LIKELY_RISK, T.ILLS_NO_HELP, T.SAQINTERVIEW_LANG, T.SPEC_REFERRAL
		     , T.SEE_SPECIALIST, T.CHECK_BLOOD_PRESSURE, T.SMOKE, T.RATE_HEALTH_CARE, T.PERC_HEALTH_31, T.PERC_HEALTH_42, T.PERC_HEALTH_53, T.BMINDX_RECODED, T.ENGSPK_RECODED, T.PLCTYP_RECODED, T.TYPEPE_RECODED, T.PROVIDER_TYPE, T.HAVE_USC
		     , T.WHY_NO_USC, T.EDRECODE_RECODED, T.EDRECODE
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
	TABLES ADDAYA42 * HEALTH_LIMITS_ACTIVITY /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADCLIM42 * HEALTH_LIMITS_STAIRS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADMALS42 * ACCOMPLISH_MENTAL_PROBLEMS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPWLM42 * WORK_LIMIT_PHYSICAL_PROBLEMS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADMWLM42 * WORK_LIMIT_MENTAL_PROBLEMS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPAIN42 * PAIN_LIMIT_WORK /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADCAPE42 * CALM /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADNRGY42 * ENERGY /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADDOWN42 * DEPRESSED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSOCA42 * HEALTH_SOCIAL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPALS42 * ACCOMPLISH_PHYSICAL_PROBLEMS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES MARRY12X * MARITAL_STATUS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EDUCYR * EDUCYR_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EDUYRDEG * EDUYRDEG_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EMPST31 * EMPLOY_STATUS_31 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EMPST42 * EMPLOY_STATUS_42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EMPST53 * EMPLOY_STATUS_53 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADINSA42 * NOT_NEED_H_INSURANCE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADINSB42 * HEALTH_WORTH_COST /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADRISK42 * LIKELY_RISK /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADOVER42 * ILLS_NO_HELP /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADLANG42 * SAQINTERVIEW_LANG /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSPRF42 * SPEC_REFERRAL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSPEC42 * SEE_SPECIALIST /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADDRBP42 * CHECK_BLOOD_PRESSURE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSMOK42 * SMOKE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADHECR42 * RATE_HEALTH_CARE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES RTHLTH31 * PERC_HEALTH_31 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES RTHLTH42 * PERC_HEALTH_42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES RTHLTH53 * PERC_HEALTH_53 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES BMINDX53 * BMINDX_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ENGSPK42 * ENGSPK_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES PLCTYP42 * PLCTYP_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES TYPEPE42 * TYPEPE_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES PROVTY42 * PROVIDER_TYPE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES HAVEUS42 * HAVE_USC /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES YNOUSC42 * WHY_NO_USC /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EDRECODE * EDRECODE_RECODED /
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


/*   START OF NODE: Reverse code certain variables   */
%LET _CLIENTTASKLABEL='Reverse code certain variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.SUBSET_RECODED_REVERSED);

PROC SQL;
   CREATE TABLE WORK.SUBSET_RECODED_REVERSED(label="SUBSET_RECODED_REVERSED") AS 
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
          /* HEALTH_GENERAL_REVERSED */
            (6 - t1.HEALTH_GENERAL) LABEL=
            "Values of ""HEALTH_GENERAL"" are reversed so that higher value = better health" AS HEALTH_GENERAL_REVERSED, 
          /* PAIN_LIMIT_WORK_REVERSED */
            (6 - t1.PAIN_LIMIT_WORK) LABEL=
            "Values of ""PAIN_LIMIT_WORK"" are reversed so that higher value = better health" AS 
            PAIN_LIMIT_WORK_REVERSED, 
          /* CALM_REVERSED */
            (6 - t1.CALM) LABEL="Values of ""CALM"" are reversed so that higher value = better health" AS CALM_REVERSED, 
          /* ENERGY_REVERSED */
            (6 - t1.ENERGY) LABEL="Values of ""ENERGY"" are reversed so that higher value = better health" AS 
            ENERGY_REVERSED, 
          t1.EDRECODE, 
          t1.EDRECODE_RECODED, 
          /* PERC_HEALTH_31_REVERSED */
            (6 - t1.PERC_HEALTH_31) LABEL="Reverse coded PERC_HEALTH_31 so higher values = better health" AS 
            PERC_HEALTH_31_REVERSED, 
          /* PERC_HEALTH_42_REVERSED */
            (6 - t1.PERC_HEALTH_42) LABEL="Reverse coded PERC_HEALTH_42 so higher values = better health" AS 
            PERC_HEALTH_42_REVERSED, 
          /* PERC_HEALTH_53_REVERSED */
            (6 - t1.PERC_HEALTH_53) LABEL="Reversed coded PERC_HEALTH_53 so higher values = better health" AS 
            PERC_HEALTH_53_REVERSED
      FROM WORK.QUERY_MEPS_FULLYR_2012_MANAGED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis38   */
%LET _CLIENTTASKLABEL='Table Analysis38';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:09 AM
   By task: Table Analysis38

   Input Data: Local:WORK.SUBSET_RECODED_REVERSED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_RECODED_REVERSED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HEALTH_GENERAL, T.PAIN_LIMIT_WORK, T.CALM, T.ENERGY, T.HEALTH_GENERAL_REVERSED, T.PAIN_LIMIT_WORK_REVERSED, T.CALM_REVERSED, T.ENERGY_REVERSED, T.PERC_HEALTH_31, T.PERC_HEALTH_42, T.PERC_HEALTH_53, T.PERC_HEALTH_31_REVERSED
		     , T.PERC_HEALTH_42_REVERSED, T.PERC_HEALTH_53_REVERSED
	FROM WORK.SUBSET_RECODED_REVERSED as T
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
	TABLES HEALTH_GENERAL * HEALTH_GENERAL_REVERSED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES PAIN_LIMIT_WORK * PAIN_LIMIT_WORK_REVERSED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES CALM * CALM_REVERSED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ENERGY * ENERGY_REVERSED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES PERC_HEALTH_31 * PERC_HEALTH_31_REVERSED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES PERC_HEALTH_42 * PERC_HEALTH_42_REVERSED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES PERC_HEALTH_53 * PERC_HEALTH_53_REVERSED /
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


/*   START OF NODE: Generate Aggregate Health Variables   */
%LET _CLIENTTASKLABEL='Generate Aggregate Health Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_AGGREGATE_HEALTH);

PROC SQL;
   CREATE TABLE WORK."QUERY_FOR_AGGREGATE_HEALTH"n(label="QUERY_FOR_AGGREGATE_HEALTH") AS 
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
          t1.EDRECODE, 
          t1.EDRECODE_RECODED, 
          t1.PERC_HEALTH_31_REVERSED, 
          t1.PERC_HEALTH_42_REVERSED, 
          t1.PERC_HEALTH_53_REVERSED, 
          /* AGGREGATE_HEALTH_SCORE */
            ((t1.HEALTH_GENERAL_REVERSED +  t1.PAIN_LIMIT_WORK_REVERSED +  t1.CALM_REVERSED +  t1.ENERGY_REVERSED +  
            t1.HEALTH_LIMITS_ACTIVITY +  t1.HEALTH_LIMITS_STAIRS +  t1.ACCOMPLISH_PHYSICAL_PROBLEMS +  
            t1.WORK_LIMIT_PHYSICAL_PROBLEMS +  t1.ACCOMPLISH_MENTAL_PROBLEMS +  t1.WORK_LIMIT_MENTAL_PROBLEMS +  
            t1.DEPRESSED +  t1.HEALTH_SOCIAL)
            ) LABEL="Aggregate Health Score" AS AGGREGATE_HEALTH_SCORE, 
          /* AGGREGATE_PERCEIVED_HEALTH */
            ((t1.PERC_HEALTH_31_REVERSED + t1.PERC_HEALTH_42_REVERSED + t1.PERC_HEALTH_53_REVERSED)) LABEL=
            "Perceived health status for entire year" AS AGGREGATE_PERCEIVED_HEALTH
      FROM WORK.SUBSET_RECODED_REVERSED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:10 AM
   By task: List Data

   Input Data: Local:WORK.QUERY_FOR_AGGREGATE_HEALTH
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_AGGREGATE_HEALTH
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HEALTH_GENERAL_REVERSED, T.PAIN_LIMIT_WORK_REVERSED, T.CALM_REVERSED, T.ENERGY_REVERSED, T.HEALTH_LIMITS_ACTIVITY, T.HEALTH_LIMITS_STAIRS, T.ACCOMPLISH_PHYSICAL_PROBLEMS, T.WORK_LIMIT_PHYSICAL_PROBLEMS
		     , T.ACCOMPLISH_MENTAL_PROBLEMS, T.WORK_LIMIT_MENTAL_PROBLEMS, T.DEPRESSED, T.HEALTH_SOCIAL, T.AGGREGATE_HEALTH_SCORE
	FROM WORK.QUERY_FOR_AGGREGATE_HEALTH as T
;
QUIT;
TITLE;
TITLE1 "SF-12 Variables and their Aggregate";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=229)
	OBS="Row number"
	;
	VAR HEALTH_GENERAL_REVERSED PAIN_LIMIT_WORK_REVERSED CALM_REVERSED ENERGY_REVERSED HEALTH_LIMITS_ACTIVITY HEALTH_LIMITS_STAIRS ACCOMPLISH_PHYSICAL_PROBLEMS WORK_LIMIT_PHYSICAL_PROBLEMS ACCOMPLISH_MENTAL_PROBLEMS WORK_LIMIT_MENTAL_PROBLEMS DEPRESSED
	  HEALTH_SOCIAL AGGREGATE_HEALTH_SCORE;
RUN;
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


/*   START OF NODE: List Data1   */
%LET _CLIENTTASKLABEL='List Data1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:10 AM
   By task: List Data1

   Input Data: Local:WORK.QUERY_FOR_AGGREGATE_HEALTH
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_AGGREGATE_HEALTH
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PERC_HEALTH_31_REVERSED, T.PERC_HEALTH_42_REVERSED, T.PERC_HEALTH_53_REVERSED, T.AGGREGATE_PERCEIVED_HEALTH
	FROM WORK.QUERY_FOR_AGGREGATE_HEALTH as T
;
QUIT;
TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	;
	VAR PERC_HEALTH_31_REVERSED PERC_HEALTH_42_REVERSED PERC_HEALTH_53_REVERSED AGGREGATE_PERCEIVED_HEALTH;
RUN;
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


/*   START OF NODE: Summary Statistics   */
%LET _CLIENTTASKLABEL='Summary Statistics';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:10 AM
   By task: Summary Statistics

   Input Data: Local:WORK.QUERY_FOR_AGGREGATE_HEALTH
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_AGGREGATE_HEALTH
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGGREGATE_HEALTH_SCORE, T.AGGREGATE_PERCEIVED_HEALTH, T.EDRECODE_RECODED, T.MARITAL_STATUS
	FROM WORK.QUERY_FOR_AGGREGATE_HEALTH(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Sean Mihaljevich";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
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
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH EDRECODE_RECODED MARITAL_STATUS;

RUN;
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


/*   START OF NODE: Distribution Analysis for Aggregate Health Score   */
%LET _CLIENTTASKLABEL='Distribution Analysis for Aggregate Health Score';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:10 AM
   By task: Distribution Analysis for Aggregate Health Score

   Input Data: Local:WORK.QUERY_FOR_AGGREGATE_HEALTH
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_AGGREGATE_HEALTH
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGGREGATE_HEALTH_SCORE, T.AGGREGATE_PERCEIVED_HEALTH, T.EDRECODE_RECODED, T.MARITAL_STATUS
	FROM WORK.QUERY_FOR_AGGREGATE_HEALTH(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: AGGREGATE_HEALTH_SCORE, AGGREGATE_PERCEIVED_HEALTH, EDRECODE_RECODED, MARITAL_STATUS";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Sean Mihaljevich";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH EDRECODE_RECODED MARITAL_STATUS;
	HISTOGRAM   AGGREGATE_HEALTH_SCORE AGGREGATE_PERCEIVED_HEALTH EDRECODE_RECODED MARITAL_STATUS / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Generate Categorical Variables   */
%LET _CLIENTTASKLABEL='Generate Categorical Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_AGGREGATE_HEALTH_0000);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_AGGREGATE_HEALTH_0000 AS 
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
          /* CATEGORICAL_AGGREGATE_HEALTH */
            (CASE  
               WHEN t1.AGGREGATE_HEALTH_SCORE >=54
               THEN 5
             WHEN t1.AGGREGATE_HEALTH_SCORE >= 50 and t1.AGGREGATE_HEALTH_SCORE < 54
               THEN 4
            WHEN t1.AGGREGATE_HEALTH_SCORE >=40 and t1.AGGREGATE_HEALTH_SCORE < 50
               THEN 3
            WHEN t1.AGGREGATE_HEALTH_SCORE >=26 and t1.AGGREGATE_HEALTH_SCORE <40
               THEN 2
            WHEN t1.AGGREGATE_HEALTH_SCORE >= 0 and t1.AGGREGATE_HEALTH_SCORE < 26
               THEN 1
               ELSE .
            END) LABEL="Classfiies ""AGGREGATE_HEALTH_SCORE"" into categories" AS CATEGORICAL_AGGREGATE_HEALTH, 
          /* CATEGORICAL_MARITAL STATUS */
            (CASE  
               WHEN t1.MARITAL_STATUS = 1 or t1.MARITAL_STATUS = 4
               THEN 1
                WHEN t1.MARITAL_STATUS = 2  or  t1.MARITAL_STATUS = 3 or  t1.MARITAL_STATUS = 5
               THEN 0
             ELSE .
            END) LABEL="Categorized marital status in married (1) or not married (0)" AS 'CATEGORICAL_MARITAL STATUS'n, 
          /* CATEGORICAL_EDUCATION */
            (CASE  
               WHEN t1.EDRECODE_RECODED = 0
               THEN 0
              WHEN t1.EDRECODE_RECODED <=5 and t1.EDRECODE_RECODED >= 1
               THEN 1
            WHEN t1.EDRECODE_RECODED <=8 and t1.EDRECODE_RECODED >= 6
               THEN 2
            WHEN t1.EDRECODE_RECODED <=12 and t1.EDRECODE_RECODED >= 9
               THEN 3
            WHEN t1.EDRECODE_RECODED = 13
               THEN 4
            WHEN t1.EDRECODE_RECODED = 14
               THEN 5
            WHEN t1.EDRECODE_RECODED = 15
               THEN 6
            WHEN t1.EDRECODE_RECODED = 16
               THEN 7
               ELSE .
            END) LABEL="Categorized EDRECODE_RECODED" AS CATEGORICAL_EDUCATION, 
          /* CATEGORICAL_PERCEIVED_HEALTH */
            (CASE  
               WHEN t1.AGGREGATE_PERCEIVED_HEALTH <= 15 and t1.AGGREGATE_PERCEIVED_HEALTH > 11
               THEN 3
            WHEN t1.AGGREGATE_PERCEIVED_HEALTH <= 11 and t1.AGGREGATE_PERCEIVED_HEALTH >= 7
               THEN 2
            WHEN t1.AGGREGATE_PERCEIVED_HEALTH <= 6 and t1.AGGREGATE_PERCEIVED_HEALTH >= 0
               THEN 1
               ELSE .
            END) LABEL="Categorized AGGREGATE_PERCIEVED_HEALTH" AS CATEGORICAL_PERCEIVED_HEALTH
      FROM WORK.QUERY_FOR_AGGREGATE_HEALTH t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:11 AM
   By task: Table Analysis

   Input Data: Local:WORK.QUERY_FOR_AGGREGATE_HEALTH_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_AGGREGATE_HEALTH_0000
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CATEGORICAL_AGGREGATE_HEALTH, T."CATEGORICAL_MARITAL STATUS"n, T.CATEGORICAL_EDUCATION, T.MARITAL_STATUS, T.EDRECODE_RECODED, T.AGGREGATE_HEALTH_SCORE, T.CATEGORICAL_PERCEIVED_HEALTH, T.AGGREGATE_PERCEIVED_HEALTH
	FROM WORK.QUERY_FOR_AGGREGATE_HEALTH_0000 as T
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
	TABLES AGGREGATE_HEALTH_SCORE * CATEGORICAL_AGGREGATE_HEALTH /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES MARITAL_STATUS * "CATEGORICAL_MARITAL STATUS"n /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EDRECODE_RECODED * CATEGORICAL_EDUCATION /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES AGGREGATE_PERCEIVED_HEALTH * CATEGORICAL_PERCEIVED_HEALTH /
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


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:11 AM
   By task: One-Way Frequencies

   Input Data: Local:WORK.QUERY_FOR_AGGREGATE_HEALTH_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_AGGREGATE_HEALTH_0000
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T."CATEGORICAL_MARITAL STATUS"n, T.CATEGORICAL_EDUCATION
	FROM WORK.QUERY_FOR_AGGREGATE_HEALTH_0000 as T
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
	TABLES "CATEGORICAL_MARITAL STATUS"n / MISSPRINT  SCORES=TABLE;
	TABLES CATEGORICAL_EDUCATION / MISSPRINT  SCORES=TABLE;
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


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:12 AM
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


/*   START OF NODE: Assign Project Library (SEAN)   */
%LET _CLIENTTASKLABEL='Assign Project Library (SEAN)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
LIBNAME SEAN BASE "P:\QAC\qac200\students\smihaljevich\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Flag full year observations   */
LIBNAME EC100018 "P:\QAC\qac200\students\smihaljevich\Assignments";


%LET _CLIENTTASKLABEL='Flag full year observations';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_2012_SUBSET_FINAL);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_2012_SUBSET_FINAL AS 
   SELECT /* INFULLYR */
            (1) AS INFULLYR, 
          t1.DUPERSID, 
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
          t1.'CATEGORICAL_MARITAL STATUS'n, 
          t1.CATEGORICAL_EDUCATION, 
          t1.CATEGORICAL_PERCEIVED_HEALTH
      FROM EC100018.meps_2012_subset_final t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Flag ER observations   */
LIBNAME EC100020 "P:\QAC\qac200\Data\MEPS";


%LET _CLIENTTASKLABEL='Flag ER observations';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT AS 
   SELECT /* INER */
            (1) AS INER, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
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
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU
      FROM EC100020.meps_er_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Merge subset and ER   */
%LET _CLIENTTASKLABEL='Merge subset and ER';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(SEAN.MEPS_FULLYR_ER_MERGED);

PROC SQL;
   CREATE TABLE SEAN.MEPS_FULLYR_ER_MERGED(label="MEPS_FULLYR_ER_MERGED") AS 
   SELECT t1.INFULLYR, 
          t1.DUPERSID, 
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
          t1.'CATEGORICAL_MARITAL STATUS'n, 
          t1.CATEGORICAL_EDUCATION, 
          t1.CATEGORICAL_PERCEIVED_HEALTH, 
          t2.INER, 
          t2.DUID, 
          t2.PID, 
          t2.DUPERSID AS DUPERSID1, 
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
          t2.PERWT12F AS PERWT12F1, 
          t2.VARSTR, 
          t2.VARPSU
      FROM WORK.QUERY_FOR_MEPS_2012_SUBSET_FINAL t1
           FULL JOIN WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT t2 ON (t1.DUPERSID = t2.DUPERSID)
      WHERE t2.INER = 1 AND t1.INFULLYR = 1;
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies11   */
%LET _CLIENTTASKLABEL='One-Way Frequencies11';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:13 AM
   By task: One-Way Frequencies11

   Input Data: Local:SEAN.MEPS_FULLYR_ER_MERGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:SEAN.MEPS_FULLYR_ER_MERGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.INFULLYR, T.INER
	FROM SEAN.MEPS_FULLYR_ER_MERGED as T
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
	TABLES INFULLYR /  SCORES=TABLE;
	TABLES INER /  SCORES=TABLE;
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


/*   START OF NODE: Table Analysis11   */
%LET _CLIENTTASKLABEL='Table Analysis11';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:14 AM
   By task: Table Analysis11

   Input Data: Local:SEAN.MEPS_FULLYR_ER_MERGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:SEAN.MEPS_FULLYR_ER_MERGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.INER, T.INFULLYR
	FROM SEAN.MEPS_FULLYR_ER_MERGED as T
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
	TABLES INFULLYR * INER /
		NOROW
		NOPERCENT
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


/*   START OF NODE: List Data31   */
%LET _CLIENTTASKLABEL='List Data31';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:14 AM
   By task: List Data31

   Input Data: Local:SEAN.MEPS_FULLYR_ER_MERGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:SEAN.MEPS_FULLYR_ER_MERGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DUPERSID, T.DUPERSID1, T.INER
	FROM SEAN.MEPS_FULLYR_ER_MERGED as T
;
QUIT;
TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	OBS="Row number"
	;
	VAR DUPERSID DUPERSID1 INER;
RUN;
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


/*   START OF NODE: Data Set Attributes11   */
%LET _CLIENTTASKLABEL='Data Set Attributes11';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:14 AM
   By task: Data Set Attributes11

   Input Data: Local:SEAN.MEPS_FULLYR_ER_MERGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForMEPS_FULLYR_ER_ME);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=SEAN.MEPS_FULLYR_ER_MERGED OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsForMEPS_FULLYR_ER_ME(LABEL="Contents Details for MEPS_FULLYR_ER_MERGED");
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
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsForMEPS_FULLYR_ER_ME
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_ER_MERGED';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsForMEPS_FULLYR_ER_ME OUT=WORK.CONTContentsForMEPS_FULLYR_ER_ME;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsForMEPS_FULLYR_ER_ME
		WHERE memname='MEPS_FULLYR_ER_MERGED';
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


/*   START OF NODE: Recode MRI, XRAY   */
%LET _CLIENTTASKLABEL='Recode MRI, XRAY';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED AS 
   SELECT t1.INFULLYR, 
          t1.DUPERSID, 
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
          t1.'CATEGORICAL_MARITAL STATUS'n, 
          t1.CATEGORICAL_EDUCATION, 
          t1.CATEGORICAL_PERCEIVED_HEALTH, 
          t1.INER, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
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
          /* XRAY_RECODED */
            (CASE 
               WHEN 2 = t1.XRAYS THEN 0
               WHEN -7 = t1.XRAYS THEN .
               WHEN -8 = t1.XRAYS THEN .
               WHEN -9 = t1.XRAYS THEN .
               WHEN 95 = t1.XRAYS THEN 0
               ELSE t1.XRAYS
            END) LABEL="Recoded missing values as ""."" and no values as ""0""" AS XRAY_RECODED, 
          /* MRI_RECODED */
            (CASE 
               WHEN 2 = t1.MRI THEN 0
               WHEN -7 = t1.MRI THEN .
               WHEN -8 = t1.MRI THEN .
               WHEN -9 = t1.MRI THEN .
               WHEN 95 = t1.MRI THEN 0
               ELSE t1.MRI
            END) LABEL="Recoding missing values as ""."" and no values as ""0""" AS MRI_RECODED
      FROM SEAN.MEPS_FULLYR_ER_MERGED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:14 AM
   By task: One-Way Frequencies1

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.XRAY_RECODED, T.MRI_RECODED
	FROM WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED as T
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
	TABLES XRAY_RECODED /  SCORES=TABLE;
	TABLES MRI_RECODED /  SCORES=TABLE;
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


/*   START OF NODE: Create count variable   */
%LET _CLIENTTASKLABEL='Create count variable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.COUNT);

PROC SQL;
   CREATE TABLE WORK."COUNT"n AS 
   SELECT t1.DUPERSID, 
          /* COUNT_of_DUPERSID */
            (COUNT(t1.DUPERSID)) AS COUNT_of_DUPERSID
      FROM WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED t1
      GROUP BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Merge Count, create # ER visits   */
%LET _CLIENTTASKLABEL='Merge Count, create # ER visits';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(SEAN.MEPS_FULLYR_ER_MERGED_COUNT);

PROC SQL;
   CREATE TABLE SEAN.MEPS_FULLYR_ER_MERGED_COUNT(label="MEPS_FULLYR_ER_MERGED_COUNT") AS 
   SELECT t1.DUPERSID, 
          /* NUMBER_ER_VISITS */
            (t1.COUNT_of_DUPERSID) LABEL="Number of ER visits per person" AS NUMBER_ER_VISITS, 
          t1.COUNT_of_DUPERSID, 
          t2.INFULLYR, 
          t2.DUPERSID AS DUPERSID1, 
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
          t2.'CATEGORICAL_MARITAL STATUS'n, 
          t2.CATEGORICAL_EDUCATION, 
          t2.CATEGORICAL_PERCEIVED_HEALTH, 
          t2.INER, 
          t2.DUID, 
          t2.PID, 
          t2.DUPERSID1 AS DUPERSID11, 
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
          t2.MRI_RECODED
      FROM WORK.COUNT t1
           INNER JOIN WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies2   */
%LET _CLIENTTASKLABEL='One-Way Frequencies2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:15 AM
   By task: One-Way Frequencies2

   Input Data: Local:SEAN.MEPS_FULLYR_ER_MERGED_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:SEAN.MEPS_FULLYR_ER_MERGED_COUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.NUMBER_ER_VISITS
	FROM SEAN.MEPS_FULLYR_ER_MERGED_COUNT as T
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
	TABLES NUMBER_ER_VISITS / MISSPRINT  SCORES=TABLE;
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


/*   START OF NODE: Distribution Analysis   */
%LET _CLIENTTASKLABEL='Distribution Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:15 AM
   By task: Distribution Analysis

   Input Data: Local:SEAN.MEPS_FULLYR_ER_MERGED_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:SEAN.MEPS_FULLYR_ER_MERGED_COUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.NUMBER_ER_VISITS
	FROM SEAN.MEPS_FULLYR_ER_MERGED_COUNT as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: NUMBER_ER_VISITS";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR NUMBER_ER_VISITS;
	HISTOGRAM   NUMBER_ER_VISITS / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Categorical ER visits   */
%LET _CLIENTTASKLABEL='Categorical ER visits';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_ AS 
   SELECT t1.DUPERSID, 
          t1.NUMBER_ER_VISITS, 
          /* CATEGORICAL_ER_VISITS */
            (CASE  
               WHEN t1.NUMBER_ER_VISITS = 1
               THEN 1
            WHEN t1.NUMBER_ER_VISITS = 2
               THEN 2
            WHEN t1.NUMBER_ER_VISITS > 2 and t1.NUMBER_ER_VISITS <= 6
               THEN 3
            WHEN t1.NUMBER_ER_VISITS >= 7
               THEN 4
               ELSE .
            END) LABEL="Categorize NUMBER_ER_VISITS into four groups" AS CATEGORICAL_ER_VISITS, 
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
          t1.'CATEGORICAL_MARITAL STATUS'n, 
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
          t1.MRI_RECODED
      FROM SEAN.MEPS_FULLYR_ER_MERGED_COUNT t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis1   */
%LET _CLIENTTASKLABEL='Table Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:15 AM
   By task: Table Analysis1

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CATEGORICAL_ER_VISITS, T.NUMBER_ER_VISITS
	FROM WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_ as T
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
	TABLES NUMBER_ER_VISITS * CATEGORICAL_ER_VISITS /
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


/*   START OF NODE: One-Way Frequencies3   */
%LET _CLIENTTASKLABEL='One-Way Frequencies3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 10:20:15 AM
   By task: One-Way Frequencies3

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.CATEGORICAL_ER_VISITS
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
	TABLES CATEGORICAL_ER_VISITS / MISSPRINT  SCORES=TABLE;
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

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
