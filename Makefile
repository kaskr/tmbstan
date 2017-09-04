install:
	R CMD INSTALL --preclean tmbstan

update-stan-model:
	R --slave < autogen.R
