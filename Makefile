pdf: ./NAMESPACE
	R ${R_OPTS} -e 'library(roxygen2);roxygenize("~/scenic/")'
	R CMD Rd2pdf --no-preview --force ~/scenic/

html: ./man/*
	R ${R_OPTS} -e 'library(roxygen2);roxygenize("~/scenic/")'
	R CMD INSTALL --html --no-inst ~/scenic/
# 	cp -v ~/R/x86_64-redhat-linux-gnu-library/3.0/scenic/html/* /ua/kendzior/public_html/CKGROUP/YOUNKIN/scenic-html/
# 	cp -v ~/R/x86_64-redhat-linux-gnu-library/3.0/scenic/DESCRIPTION /ua/kendzior/public_html/CKGROUP/YOUNKIN/
# cd /ua/kendzior/public_html/CKGROUP/YOUNKIN/scenic-html/; touch ./*.html
