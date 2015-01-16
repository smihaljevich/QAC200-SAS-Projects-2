/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Friday, January 16, 2015     TIME: 3:20:21 PM
PROJECT: mihaljevichs_SAS_project_011615
PROJECT PATH: P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp
---------------------------------------- */

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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME SEAN BASE "P:\QAC\qac200\students\smihaljevich\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Bar Chart Sex   */
%LET _CLIENTTASKLABEL='Bar Chart Sex';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:12 PM
   By task: Bar Chart Sex

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SEX
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "1 = Male, 2 = Female" )


;
TITLE;
TITLE1 "Bar Chart for SEX";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 SEX
 /
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: Bar Chart Region   */
%LET _CLIENTTASKLABEL='Bar Chart Region';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:12 PM
   By task: Bar Chart Region

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.REGION12
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "1 = Northeast, 2 = Midwest, 3 = South, 4 = West" )


;
TITLE;
TITLE1 "Bar Chart for REGION12";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 REGION12
 /
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: Bar Chart Race   */
%LET _CLIENTTASKLABEL='Bar Chart Race';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:13 PM
   By task: Bar Chart Race

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.RACETHX
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "1 = Hispanic, 2 = White , 3 = Black, 4 = Asian, 5 = Other Race/Multiple Race" )


;
TITLE;
TITLE1 "Bar Chart for RACETHX";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 RACETHX
 /
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: Bar Chart Marital Status   */
%LET _CLIENTTASKLABEL='Bar Chart Marital Status';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:14 PM
   By task: Bar Chart Marital Status

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CATEGORICAL_MARITAL_STATUS
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = Not Married, 1 = Married" )


;
TITLE;
TITLE1 "Bar Chart for CATEGORICAL_MARITAL_STATUS";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 CATEGORICAL_MARITAL_STATUS
 /
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: Bar Chart Education   */
%LET _CLIENTTASKLABEL='Bar Chart Education';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:15 PM
   By task: Bar Chart Education

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CATEGORICAL_EDUCATION
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0= <1st Grade,1=Elementary,2=Middle School,3=HS,4=HS Degree,5=College,6=Col. Degree,7=Advanced" )


;
TITLE;
TITLE1 "Bar Chart for CATEGORICAL_EDUCATION";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 CATEGORICAL_EDUCATION
 /
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: Bar Chart Age   */
%LET _CLIENTTASKLABEL='Bar Chart Age';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:16 PM
   By task: Bar Chart Age

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CATEGORICAL_AGE
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "Age Categories: 1 = 18-30, 2 = 31-40, 3 = 41-55, 4 =56-70, 5 = greater than 70" )


;
TITLE;
TITLE1 "Bar Chart for CATEGORICAL_AGE";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 CATEGORICAL_AGE
 /
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: Bar Chart MRI   */
%LET _CLIENTTASKLABEL='Bar Chart MRI';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:17 PM
   By task: Bar Chart MRI

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MRI_RECODED
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive MRI, 1 = received MRI" )


;
TITLE;
TITLE1 "Bar Chart for MRI_RECODED";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 MRI_RECODED
 /
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: Bar Chart X-ray   */
%LET _CLIENTTASKLABEL='Bar Chart X-ray';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:17 PM
   By task: Bar Chart X-ray

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.XRAY_RECODED
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive X-ray, 1 = received X-ray" )


;
TITLE;
TITLE1 "Bar Chart for XRAY_RECODED";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 XRAY_RECODED
 /
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: Histogram Age   */
%LET _CLIENTTASKLABEL='Histogram Age';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:18 PM
   By task: Histogram Age

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGE12X
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Frequency of Specific Age" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "Age" )


;
TITLE;
TITLE1 "Histogram for AGE12X";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 AGE12X
 /
	CLIPREF
FRAME	DISCRETE
	TYPE=FREQ
	OUTSIDE=FREQ
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: Histogram ER Visits   */
%LET _CLIENTTASKLABEL='Histogram ER Visits';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:19 PM
   By task: Histogram ER Visits

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.NUMBER_ER_VISITS
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Frequency of Specific Value" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "Number of ER visits per person" )


;
TITLE;
TITLE1 "Histogram for NUMBER_ER_VISITS";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 NUMBER_ER_VISITS
 /
	CLIPREF
FRAME	DISCRETE
	TYPE=FREQ
	OUTSIDE=FREQ
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: Histogram Aggregate Health   */
%LET _CLIENTTASKLABEL='Histogram Aggregate Health';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:20 PM
   By task: Histogram Aggregate Health

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGGREGATE_HEALTH_SCORE
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Frequency of Specific Score" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "Aggregate Health Score" )


;
TITLE;
TITLE1 "Histogram for AGGREGATE_HEALTH_SCORE";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 AGGREGATE_HEALTH_SCORE
 /
	CLIPREF
FRAME	DISCRETE
	TYPE=FREQ
	OUTSIDE=FREQ
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: Histogram Perceived Health   */
%LET _CLIENTTASKLABEL='Histogram Perceived Health';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:20 PM
   By task: Histogram Perceived Health

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGGREGATE_PERCEIVED_HEALTH
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Frequency of Specific Value" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "Aggregate Perceived Health Score" )


;
TITLE;
TITLE1 "Histogram for AGGREGATE_PERCEIVED_HEALTH";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 AGGREGATE_PERCEIVED_HEALTH
 /
	CLIPREF
FRAME	DISCRETE
	TYPE=FREQ
	OUTSIDE=FREQ
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: MRI grouped by Sex   */
%LET _CLIENTTASKLABEL='MRI grouped by Sex';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:21 PM
   By task: MRI grouped by Sex

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MRI_RECODED, T.SEX
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive MRI, 1 = received MRI" )


;
Axis3

	LABEL=( "1 = Male, 2 = Female" )



;
TITLE;
TITLE1 "Bar Chart for MRI_RECODED grouped by SEX";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 MRI_RECODED
 /
	GROUP=SEX
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: MRI grouped by Region   */
%LET _CLIENTTASKLABEL='MRI grouped by Region';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:22 PM
   By task: MRI grouped by Region

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MRI_RECODED, T.REGION12
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive MRI, 1 = received MRI" )


;
Axis3

	LABEL=( "1 = Northeast, 2 = Midwest, 3 = South, 4 = West" )



;
TITLE;
TITLE1 "Bar Chart for MRI_RECODED grouped by REGION12";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 MRI_RECODED
 /
	GROUP=REGION12
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: MRI grouped by Race   */
%LET _CLIENTTASKLABEL='MRI grouped by Race';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:22 PM
   By task: MRI grouped by Race

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MRI_RECODED, T.RACETHX
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive MRI, 1 = received MRI" )


;
Axis3

	LABEL=( "Race" )



;
TITLE;
TITLE1 "Bar Chart for MRI_RECODED grouped by RACETHX";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 MRI_RECODED
 /
	GROUP=RACETHX
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: MRI grouped by Maritul Status   */
%LET _CLIENTTASKLABEL='MRI grouped by Maritul Status';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:23 PM
   By task: MRI grouped by Maritul Status

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MRI_RECODED, T.CATEGORICAL_MARITAL_STATUS
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive MRI, 1 = received MRI" )


;
Axis3

	LABEL=( "0 = Not Married, 1 = Married" )



;
TITLE;
TITLE1 "Bar Chart for MRI_RECODED grouped by CATEGORICAL_MARITAL_STATUS";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 MRI_RECODED
 /
	GROUP=CATEGORICAL_MARITAL_STATUS
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: MRI grouped by education   */
%LET _CLIENTTASKLABEL='MRI grouped by education';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:24 PM
   By task: MRI grouped by education

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MRI_RECODED, T.CATEGORICAL_EDUCATION
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive MRI, 1 = received MRI" )


;
Axis3

	LABEL=( "Education Level" )



;
TITLE;
TITLE1 "Bar Chart for MRI_RECODED grouped by CATEGORICAL_EDUCATION";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 MRI_RECODED
 /
	GROUP=CATEGORICAL_EDUCATION
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: MRI grouped by Age   */
%LET _CLIENTTASKLABEL='MRI grouped by Age';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:25 PM
   By task: MRI grouped by Age

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MRI_RECODED, T.CATEGORICAL_AGE
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive MRI, 1 = received MRI" )


;
Axis3

	LABEL=( "Age Category" )



;
TITLE;
TITLE1 "Bar Chart for MRI_RECODED grouped by CATEGORICAL_AGE";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 MRI_RECODED
 /
	GROUP=CATEGORICAL_AGE
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: X-ray grouped by Sex   */
%LET _CLIENTTASKLABEL='X-ray grouped by Sex';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:26 PM
   By task: X-ray grouped by Sex

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.XRAY_RECODED, T.SEX
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive X-ray, 1 = received X-ray" )


;
Axis3

	LABEL=( "1 = Male, 2 = Female" )



;
TITLE;
TITLE1 "Bar Chart for XRAY_RECODED grouped by SEX";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 XRAY_RECODED
 /
	GROUP=SEX
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: X-ray grouped by Region   */
%LET _CLIENTTASKLABEL='X-ray grouped by Region';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:27 PM
   By task: X-ray grouped by Region

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.XRAY_RECODED, T.REGION12
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive X-ray, 1 = received X-ray" )


;
Axis3

	LABEL=( "1 = Northeast, 2 = Midwest, 3 = South, 4 = West" )



;
TITLE;
TITLE1 "Bar Chart for XRAY_RECODED grouped by REGION12";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 XRAY_RECODED
 /
	GROUP=REGION12
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: X-ray grouped by Race   */
%LET _CLIENTTASKLABEL='X-ray grouped by Race';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:28 PM
   By task: X-ray grouped by Race

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.XRAY_RECODED, T.RACETHX
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive X-ray, 1 = received X-ray" )


;
Axis3

	LABEL=( "Race" )



;
TITLE;
TITLE1 "Bar Chart for XRAY_RECODED grouped by RACETHX";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 XRAY_RECODED
 /
	GROUP=RACETHX
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: X-ray grouped by Maritul Status   */
%LET _CLIENTTASKLABEL='X-ray grouped by Maritul Status';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:29 PM
   By task: X-ray grouped by Maritul Status

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.XRAY_RECODED, T.CATEGORICAL_MARITAL_STATUS
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive X-ray, 1 = received X-ray" )


;
Axis3

	LABEL=( "0 = Not Married, 1 = Married" )



;
TITLE;
TITLE1 "Bar Chart for XRAY_RECODED grouped by CATGORICAL_MARITAL_STATUS";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 XRAY_RECODED
 /
	GROUP=CATEGORICAL_MARITAL_STATUS
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: X-ray grouped by education   */
%LET _CLIENTTASKLABEL='X-ray grouped by education';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:30 PM
   By task: X-ray grouped by education

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.XRAY_RECODED, T.CATEGORICAL_EDUCATION
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive X-ray, 1 = received X-ray" )


;
Axis3

	LABEL=( "Education Level" )



;
TITLE;
TITLE1 "Bar Chart for XRAY_RECODED grouped by CATEGORICAL_EDUCATION";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 XRAY_RECODED
 /
	GROUP=CATEGORICAL_EDUCATION
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: X-ray grouped by Age   */
%LET _CLIENTTASKLABEL='X-ray grouped by Age';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:30 PM
   By task: X-ray grouped by Age

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.XRAY_RECODED, T.CATEGORICAL_AGE
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive X-ray, 1 = received X-ray" )


;
Axis3

	LABEL=( "Age Category" )



;
TITLE;
TITLE1 "Bar Chart for XRAY_RECODED grouped by CATEGORICAL_AGE";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 XRAY_RECODED
 /
	GROUP=CATEGORICAL_AGE
	CLIPREF
FRAME	DISCRETE
	TYPE=PCT
	OUTSIDE=PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
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


/*   START OF NODE: Scatter Plot for AGE12X and BMINDX53   */
%LET _CLIENTTASKLABEL='Scatter Plot for AGE12X and BMINDX53';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:31 PM
   By task: Scatter Plot for AGE12X and BMINDX53

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGE12X, T.BMINDX53
	FROM ECLIB000.meps_fullyr_er_merged_final_3(FIRSTOBS=1 ) as T
;
QUIT;
	SYMBOL1
	INTERPOL=NONE
	HEIGHT=10pt
	VALUE=CIRCLE
	LINE=1
	WIDTH=2

	CV = _STYLE_
;
Axis1
	STYLE=1
	WIDTH=1
	MINOR=NONE
	LABEL=(   "Body Mass Index")


;
Axis2
	STYLE=1
	WIDTH=1
	MINOR=NONE
	LABEL=(   "Age")


;
TITLE;
TITLE1 "Scatter Plot for AGE12X and BMINDX53";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GPLOT DATA=WORK.SORTTempTableSorted
;
PLOT BMINDX53 * AGE12X / 
	VAXIS=AXIS1

	HAXIS=AXIS2

FRAME ;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
GOPTIONS RESET = SYMBOL;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies on BMINDX53   */
%LET _CLIENTTASKLABEL='One-Way Frequencies on BMINDX53';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:32 PM
   By task: One-Way Frequencies on BMINDX53

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.BMINDX53
	FROM ECLIB000.meps_fullyr_er_merged_final_3 as T
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
	TABLES BMINDX53 / MISSPRINT  SCORES=TABLE;
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


/*   START OF NODE: Filter BMINDX53   */
LIBNAME EC100093 "P:\QAC\qac200\students\smihaljevich\Assignments";


%LET _CLIENTTASKLABEL='Filter BMINDX53';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_ AS 
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
      FROM EC100093.meps_fullyr_er_merged_final_3 t1
      WHERE t1.BMINDX53 BETWEEN 0 AND 85;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Scatter Plot for AGE12X and BMINDX53 (Filtered)   */
%LET _CLIENTTASKLABEL='Scatter Plot for AGE12X and BMINDX53 (Filtered)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:33 PM
   By task: Scatter Plot for AGE12X and BMINDX53 (Filtered)

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGE12X, T.BMINDX53
	FROM WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_(FIRSTOBS=1 ) as T
;
QUIT;
	SYMBOL1
	INTERPOL=NONE
	HEIGHT=10pt
	VALUE=CIRCLE
	LINE=1
	WIDTH=2

	CV = _STYLE_
;
Axis1
	STYLE=1
	WIDTH=1
	MINOR=NONE
	LABEL=(   "Body Mass Index")


;
Axis2
	STYLE=1
	WIDTH=1
	MINOR=NONE
	LABEL=(   "Age")


;
TITLE;
TITLE1 "Scatter Plot for AGE12X and BMINDX53 (Filtered)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GPLOT DATA=WORK.SORTTempTableSorted
;
PLOT BMINDX53 * AGE12X / 
	VAXIS=AXIS1

	HAXIS=AXIS2

FRAME ;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
GOPTIONS RESET = SYMBOL;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Scatter Plot grouped by SEX   */
%LET _CLIENTTASKLABEL='Scatter Plot grouped by SEX';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'REGION12');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:33 PM
   By task: Scatter Plot grouped by SEX

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_(FIRSTOBS=1  KEEP=AGE12X BMINDX53 "&GroupingPrompt"n)
	OUT=WORK.SORTTempTableSorted
	;
	BY "&GroupingPrompt"n;
RUN;
	SYMBOL1
	INTERPOL=NONE
	HEIGHT=10pt
	VALUE=CIRCLE
	LINE=1
	WIDTH=2

	CV = _STYLE_
;
Axis1
	STYLE=1
	WIDTH=1
	MINOR=NONE
	LABEL=(   "Body Mass Index")


;
Axis2
	STYLE=1
	WIDTH=1
	MINOR=NONE
	LABEL=(   "Age")


;
TITLE;
TITLE1 "Scatter Plot for AGE12X and BMINDX53 (Filtered) grouped by SEX (1 = Male, 2 = Female)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GPLOT DATA=WORK.SORTTempTableSorted
 NOCACHE ;
PLOT BMINDX53 * AGE12X / 
	VAXIS=AXIS1

	HAXIS=AXIS2

FRAME ;
	BY "&GroupingPrompt"n;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
GOPTIONS RESET = SYMBOL;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: Scatter Plot grouped by REGION12   */
%LET _CLIENTTASKLABEL='Scatter Plot grouped by REGION12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';
DATA _NULL_;
CALL SYMPUT("GroupingPrompt",'REGION12');
RUN;

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:33 PM
   By task: Scatter Plot grouped by REGION12

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.QUERY_FOR_MEPS_FULLYR_ER_MERGED_(FIRSTOBS=1  KEEP=AGE12X BMINDX53 "&GroupingPrompt"n)
	OUT=WORK.SORTTempTableSorted
	;
	BY "&GroupingPrompt"n;
RUN;
	SYMBOL1
	INTERPOL=NONE
	HEIGHT=10pt
	VALUE=CIRCLE
	LINE=1
	WIDTH=2

	CV = _STYLE_
;
Axis1
	STYLE=1
	WIDTH=1
	MINOR=NONE
	LABEL=(   "Body Mass Index")


;
Axis2
	STYLE=1
	WIDTH=1
	MINOR=NONE
	LABEL=(   "Age")


;
TITLE;
TITLE1 "Scatter Plot for AGE12X and BMINDX53 (Filtered) grouped by REGION12 (1 = Northeast, 2 = Midwest, 3 = South, 4 = West)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GPLOT DATA=WORK.SORTTempTableSorted
 NOCACHE ;
PLOT BMINDX53 * AGE12X / 
	VAXIS=AXIS1

	HAXIS=AXIS2

FRAME ;
	BY "&GroupingPrompt"n;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
GOPTIONS RESET = SYMBOL;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%SYMDEL GroupingPrompt;


/*   START OF NODE: Multivariate Region, Sex, MRI   */
%LET _CLIENTTASKLABEL='Multivariate Region, Sex, MRI';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:33 PM
   By task: Multivariate Region, Sex, MRI

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set ECLIB000.meps_fullyr_er_merged_final_3
   ------------------------------------------------------------------- */
PROC SORT
	DATA=ECLIB000.meps_fullyr_er_merged_final_3(KEEP=MRI_RECODED REGION12 SEX)
	OUT=WORK.SORTTempTableSorted
	;
	BY SEX;
RUN;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive MRI, 1 = received MRI" )


;
Axis3

	LABEL=( "1 = Northeast, 2 = Midwest, 3 = South, 4 = West" )



;
TITLE;
TITLE1 "Bar Chart for MRI_RECODED grouped by REGION12 and SEX (1 = Male, 2 = Female)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 MRI_RECODED
 /
	GROUP=REGION12
	SHAPE=BLOCK
FRAME	DISCRETE
	TYPE=PCT
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
	BY SEX;
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


/*   START OF NODE: Multivariate Race, Sex, MRI   */
%LET _CLIENTTASKLABEL='Multivariate Race, Sex, MRI';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:34 PM
   By task: Multivariate Race, Sex, MRI

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set ECLIB000.meps_fullyr_er_merged_final_3
   ------------------------------------------------------------------- */
PROC SORT
	DATA=ECLIB000.meps_fullyr_er_merged_final_3(KEEP=MRI_RECODED RACETHX SEX)
	OUT=WORK.SORTTempTableSorted
	;
	BY SEX;
RUN;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive MRI, 1 = received MRI" )


;
Axis3

	LABEL=( "Race" )



;
TITLE;
TITLE1 "Bar Chart for MRI_RECODED grouped by RACETHX and SEX (1 = Male, 2 = Female)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 MRI_RECODED
 /
	GROUP=RACETHX
	SHAPE=BLOCK
FRAME	DISCRETE
	TYPE=PCT
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
	BY SEX;
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


/*   START OF NODE: Multivariate Marital, Sex, MRI   */
%LET _CLIENTTASKLABEL='Multivariate Marital, Sex, MRI';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:36 PM
   By task: Multivariate Marital, Sex, MRI

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set ECLIB000.meps_fullyr_er_merged_final_3
   ------------------------------------------------------------------- */
PROC SORT
	DATA=ECLIB000.meps_fullyr_er_merged_final_3(KEEP=MRI_RECODED CATEGORICAL_MARITAL_STATUS SEX)
	OUT=WORK.SORTTempTableSorted
	;
	BY SEX;
RUN;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive MRI, 1 = received MRI" )


;
Axis3

	LABEL=( "0 = not married, 1 = married" )



;
TITLE;
TITLE1 "Bar Chart for MRI_RECODED grouped by CATEGORICAL_MARITAL_STATUS and SEX (1 = Male, 2 = Female)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 MRI_RECODED
 /
	GROUP=CATEGORICAL_MARITAL_STATUS
	SHAPE=BLOCK
FRAME	DISCRETE
	TYPE=PCT
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
	BY SEX;
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


/*   START OF NODE: Multivariate Education, Sex, MRI   */
%LET _CLIENTTASKLABEL='Multivariate Education, Sex, MRI';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:37 PM
   By task: Multivariate Education, Sex, MRI

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set ECLIB000.meps_fullyr_er_merged_final_3
   ------------------------------------------------------------------- */
PROC SORT
	DATA=ECLIB000.meps_fullyr_er_merged_final_3(KEEP=MRI_RECODED CATEGORICAL_EDUCATION SEX)
	OUT=WORK.SORTTempTableSorted
	;
	BY SEX;
RUN;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive MRI, 1 = received MRI" )


;
Axis3

	LABEL=( "Education Level" )



;
TITLE;
TITLE1 "Bar Chart for MRI_RECODED grouped by CATEGORICAL_EDUCATION and SEX (1 = Male, 2 = Female)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 MRI_RECODED
 /
	GROUP=CATEGORICAL_EDUCATION
	SHAPE=BLOCK
FRAME	DISCRETE
	TYPE=PCT
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
	BY SEX;
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


/*   START OF NODE: Multivariate Age, Sex, MRI   */
%LET _CLIENTTASKLABEL='Multivariate Age, Sex, MRI';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:38 PM
   By task: Multivariate Age, Sex, MRI

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set ECLIB000.meps_fullyr_er_merged_final_3
   ------------------------------------------------------------------- */
PROC SORT
	DATA=ECLIB000.meps_fullyr_er_merged_final_3(KEEP=MRI_RECODED CATEGORICAL_AGE SEX)
	OUT=WORK.SORTTempTableSorted
	;
	BY SEX;
RUN;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive MRI, 1 = received MRI" )


;
Axis3

	LABEL=( "Age Category" )



;
TITLE;
TITLE1 "Bar Chart for MRI_RECODED grouped by CATEGORICAL_AGE and SEX (1 = Male, 2 = Female)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 MRI_RECODED
 /
	GROUP=CATEGORICAL_AGE
	SHAPE=BLOCK
FRAME	DISCRETE
	TYPE=PCT
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
	BY SEX;
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


/*   START OF NODE: Multivariate Region, Sex, X-ray   */
%LET _CLIENTTASKLABEL='Multivariate Region, Sex, X-ray';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:39 PM
   By task: Multivariate Region, Sex, X-ray

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set ECLIB000.meps_fullyr_er_merged_final_3
   ------------------------------------------------------------------- */
PROC SORT
	DATA=ECLIB000.meps_fullyr_er_merged_final_3(KEEP=XRAY_RECODED REGION12 SEX)
	OUT=WORK.SORTTempTableSorted
	;
	BY SEX;
RUN;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive X-ray, 1 = received X-ray" )


;
Axis3

	LABEL=( "1 = Northeast, 2 = Midwest, 3 = South, 4 = West" )



;
TITLE;
TITLE1 "Bar Chart for XRAY_RECODED grouped by REGION12 and SEX (1 = Male, 2 = Female)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 XRAY_RECODED
 /
	GROUP=REGION12
	SHAPE=BLOCK
FRAME	DISCRETE
	TYPE=PCT
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
	BY SEX;
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


/*   START OF NODE: Multivariate Race, Sex, X-ray   */
%LET _CLIENTTASKLABEL='Multivariate Race, Sex, X-ray';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:40 PM
   By task: Multivariate Race, Sex, X-ray

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set ECLIB000.meps_fullyr_er_merged_final_3
   ------------------------------------------------------------------- */
PROC SORT
	DATA=ECLIB000.meps_fullyr_er_merged_final_3(KEEP=XRAY_RECODED RACETHX SEX)
	OUT=WORK.SORTTempTableSorted
	;
	BY SEX;
RUN;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive X-ray, 1 = received X-ray" )


;
Axis3

	LABEL=( "Race" )



;
TITLE;
TITLE1 "Bar Chart for XRAY_RECODED grouped by RACETHX and SEX (1 = Male, 2 = Female)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 XRAY_RECODED
 /
	GROUP=RACETHX
	SHAPE=BLOCK
FRAME	DISCRETE
	TYPE=PCT
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
	BY SEX;
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


/*   START OF NODE: Multivariate Marital, Sex, X-ray   */
%LET _CLIENTTASKLABEL='Multivariate Marital, Sex, X-ray';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:41 PM
   By task: Multivariate Marital, Sex, X-ray

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set ECLIB000.meps_fullyr_er_merged_final_3
   ------------------------------------------------------------------- */
PROC SORT
	DATA=ECLIB000.meps_fullyr_er_merged_final_3(KEEP=XRAY_RECODED CATEGORICAL_MARITAL_STATUS SEX)
	OUT=WORK.SORTTempTableSorted
	;
	BY SEX;
RUN;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive X-ray, 1 = received X-ray" )


;
Axis3

	LABEL=( "0 = not married, 1 = married" )



;
TITLE;
TITLE1 "Bar Chart for XRAY_RECODED grouped by CATEGORICAL_MARITAL_STATUS and SEX (1 = Male, 2 = Female)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 XRAY_RECODED
 /
	GROUP=CATEGORICAL_MARITAL_STATUS
	SHAPE=BLOCK
FRAME	DISCRETE
	TYPE=PCT
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
	BY SEX;
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


/*   START OF NODE: Multivariate Education, Sex, X-ray   */
%LET _CLIENTTASKLABEL='Multivariate Education, Sex, X-ray';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:42 PM
   By task: Multivariate Education, Sex, X-ray

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set ECLIB000.meps_fullyr_er_merged_final_3
   ------------------------------------------------------------------- */
PROC SORT
	DATA=ECLIB000.meps_fullyr_er_merged_final_3(KEEP=XRAY_RECODED CATEGORICAL_EDUCATION SEX)
	OUT=WORK.SORTTempTableSorted
	;
	BY SEX;
RUN;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive X-ray, 1 = received X-ray" )


;
Axis3

	LABEL=( "Education Level" )



;
TITLE;
TITLE1 "Bar Chart for XRAY_RECODED grouped by CATEGORICAL_EDUCATION and SEX (1 = Male, 2 = Female)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 XRAY_RECODED
 /
	GROUP=CATEGORICAL_EDUCATION
	SHAPE=BLOCK
FRAME	DISCRETE
	TYPE=PCT
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
	BY SEX;
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


/*   START OF NODE: Multivariate Age, Sex, X-ray   */
%LET _CLIENTTASKLABEL='Multivariate Age, Sex, X-ray';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\smihaljevich\Assignments\mihaljevichs_SAS_project_011615.egp';
%LET _CLIENTPROJECTNAME='mihaljevichs_SAS_project_011615.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\smihaljevich\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 16, 2015 at 3:16:44 PM
   By task: Multivariate Age, Sex, X-ray

   Input Data: P:\QAC\qac200\students\smihaljevich\Assignments\meps_fullyr_er_merged_final_3.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set ECLIB000.meps_fullyr_er_merged_final_3
   ------------------------------------------------------------------- */
PROC SORT
	DATA=ECLIB000.meps_fullyr_er_merged_final_3(KEEP=XRAY_RECODED CATEGORICAL_AGE SEX)
	OUT=WORK.SORTTempTableSorted
	;
	BY SEX;
RUN;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Percentage Share of Observations" )


;
Axis2
	STYLE=1
	WIDTH=1
	LABEL=( "0 = did not receive X-ray, 1 = received X-ray" )


;
Axis3

	LABEL=( "Age Category" )



;
TITLE;
TITLE1 "Bar Chart for XRAY_RECODED grouped by CATEGORICAL_AGE and SEX (1 = Male, 2 = Female)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 XRAY_RECODED
 /
	GROUP=CATEGORICAL_AGE
	SHAPE=BLOCK
FRAME	DISCRETE
	TYPE=PCT
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	GAXIS=AXIS3
PATTERNID=MIDPOINT
;
	BY SEX;
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

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
