# pivot

[![Build Status](https://travis-ci.org/haplo/pivot.svg?branch=master)](https://travis-ci.org/tidyverse/pivot)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/pivot)](http://cran.r-project.org/package=pivot)

## License Disclaimer

This pacakge was developed through funding and resources by the 
[United States Veterans Health Administration](https://www.va.gov/health/) and the 
[VA Informatic and Computing Infrastructure (VINCI)](https://www.hsrd.research.va.gov/for_researchers/vinci/)
by govornment employees.  As such according to 17 U.S. Code §105 is not 
covered by copyright and is released to the public domain with no 
license restrictions.  This code is released without any guarantee of
suitability for any purpose.

## Overview
The `pivot` R pacakge extends the capabilities of [dbplyr](http://cran.r-project.org/package=dbplyr) 
to have pivot and unpivot capabilities.  It currently works only for Microsoft SQL Server.
For convenience it also provides extentions the [tidyr](http://cran.r-project.org/package=tidyr)
functions [spread](http://tidyr.tidyverse.org/reference/spread.html) 
and [gather](http://tidyr.tidyverse.org/reference/gather.html)
to act on remote Microsoft SQL Tables without needing to collect the data first. 


