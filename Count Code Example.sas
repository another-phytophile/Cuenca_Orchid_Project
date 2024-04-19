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