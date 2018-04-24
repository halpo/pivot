## Test environments
* local linux install, R 3.4.3
* ubuntu 14.04 (on travis-ci), R 3.4.4
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

---
Update 2018-04-24

Per the request of Swetlana Herbrandt, 

> Please add '()' behind all function names (e.g. unpivot()) in your description.

I have updated the Description field to reflect package names in single
quotes and function names followed by parentheses.  I was however 
unclear about handling 'sql_tbl' as it is a class name and neither a 
package nor a function name, so I left that as is.

In response to the request:

> Please write package names and software names in single quotes 
> (e.g. 'MASS') in title and description. 

The title reads "SQL PIVOT and UNPIVOT".  The title of the package is 
`pivot` and the uppercase `PIVOT` refers to the SQL keyword and not the 
package name, and so I repectfully submit that the PIVOT should not be
in quotes.

The request was also made for adding testable examples.  I have added 
a testing suite to test those potions that are able to be tested, 
however the core functionality cannot be tested as it would require an 
established connection to a Microsoft SQL Database.  I have gone to 
the extent of simulating the database connection and remote tables, and 
have checked that the SQL rendered is what would be expected.  The 
examples and tests should run now.
---

* win_builder gives 1 note due to the unrecognized word 'unpivot'
* travis-ci gives 1 note due to package 'odbc' being unavailable on that platform.

This is an update of the previous submission Made on April 17.
Uwe Ligges Reqponded requesting a change to the Description field
 as well as justification for the package:

> But then, is the functionality not included in the MS SQL server? 
> Please explain the functionality we do not receive anyway by 
> submitting SQL requests to the SQL server i.e. via RODBC? 

RODBC, or DBI, odbc, and dbplyr which pivot is built on and extends, 
provides the functionality to seamlessly `pivot` (ie. `summarise+spread`) 
and `unpivot` (ie. `gather`) tables on remote `sql_tbl` tables residing on 
a Microsoft SQL Server.  The DESCRIPTION has been updated to reflect 
this.
