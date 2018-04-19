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
