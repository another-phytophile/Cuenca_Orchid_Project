%let avis = "C:\Users\yujer\Documents\Cuenca_Orchid_Project\Data\finaltest.csv";

/*import cleaned csv*/
proc import datafile=&avis
    out=orchidinit
    dbms=csv
    replace;
    getnames=yes;
run;

ods graphics on;
/*Perform Overall ANOVA*/
proc glm data=orchidinit plots=diagnostics; 
	class es med l;
	model rnum = es|med|l / SS3;
    lsmeans es med l;
run;

/*Perform Species Specific ANOVAs*/
proc sql;
create table cmax as select *
from orchidinit where es = "11" ;


proc glm data=cmax plots=diagnostics; 
	class med l;
	model nr = med|l / SS3;
    lsmeans med l;
run;

proc glm data=cmax plots=diagnostics; 
	class med l;
	model rnum = med|l / SS3;
run;

proc sql;
create table cym as select *
from orchidinit where es = "10" ;


proc glm data=cym plots=diagnostics; 
	class med l;
	model nr = med|l / SS3;
    lsmeans med l;
run;


/*WTF Poisson Stuff*/
proc genmod data=orchidinit;
	class es med l;
    model nr = es|med|l /dist=poisson
                         link=log;
run;

proc genmod data=orchidinit;
	class es med l;
    model nr = es|med|l /dist=negbin
                         link=log;
run;

proc genmod data=orchidinit;
	class es med l;
    model nr = es|med|l /dist=poisson
                         link=log;
    zeromodel es|med|l / link=logit;
run;

proc genmod data=orchidinit;
	class es med l;
    model nr = es|med|l /dist=negbin
                         link=log;
    zeromodel es|med|l / link=logit;
run;

quit;