# pivot

[![Build Status](https://api.travis-ci.org/halpo/pivot.svg?branch=master)](https://travis-ci.org/halpo/pivot)
<!--[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/pivot)](https://cran.r-project.org/package=pivot)-->
[![codecov.io](https://codecov.io/github/halpo/pivot/coverage.svg?branch=master)](https://codecov.io/github/halpo/pivot?branch=master)

## License Disclaimer

This pacakge was developed through funding and resources by the 
[United States Veterans Health Administration](https://www.va.gov/health/) and the 
[VA Informatic and Computing Infrastructure (VINCI)](https://www.hsrd.research.va.gov/for_researchers/vinci/)
by govornment employees.  As such according to 17 U.S. Code §105 is not 
covered by copyright and is released to the public domain with no 
license restrictions.  This code is released without any guarantee of
suitability for any purpose.

## Overview
The `pivot` R pacakge extends the capabilities of [dbplyr](https://cran.r-project.org/package=dbplyr) 
to have [pivot and unpivot](https://technet.microsoft.com/en-us/library/ms177410(v=sql.105).aspx) capabilities.
It currently works only for Microsoft SQL Server.
For convenience it also provides extentions to the [tidyr](https://cran.r-project.org/package=tidyr)
functions [spread](https://tidyr.tidyverse.org/reference/spread.html) 
and [gather](https://tidyr.tidyverse.org/reference/gather.html)
to act on remote Microsoft SQL Tables without needing to collect the data first. 


