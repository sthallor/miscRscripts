:: ErrorSearch.BAT
:: DOS batch file to recursively search directories with ASCII text files
::      for keywords and list each row into a single file.
:: FHS, Apr 20, 2016
@ECHO OFF
Setlocal EnableDelayedExpansion

:: dirname: Directory to start with, will look in all subdirectories
:: zipfile: name of zip file to look for in each directory
:: csvfile: name of extracted zip file 
:: Keywords: for lines to collect from output text file
:: outfile: filename for text file containing output results
set dirname="E:\ace\WellData"
set "filefilter=\*_log.txt"
set filefilter1="file_clean_log.txt"
set keywords="ERROR"
set outfile="ERROR.txt"

if exist %outfile% del %outfile%
for /f "tokens=*" %%G in ('dir /b /s /a:d %dirname%') do if exist "%%G%filefilter%" (
   find %keywords% "%%G%filefilter%" | find /C %keywords% > "count.txt"
   set /p count=<"count.txt"
   if !count!==0 (
      echo No Errors Found in "%%G%filefilter%"
   ) ELSE (      
      echo Found !count! errors in "%%G%filefilter%"
      echo Found !count! errors in "%%G%filefilter%" >> %outfile%
      dir /OD "%%G%filefilter%" | find %filefilter1% >> %outfile%
      find %keywords% "%%G%filefilter%" | find %keywords%  >> %outfile%
      echo.>> %outfile%
   )
)
if exist "count.txt" del "count.txt"

SET /P testing="Enter anything to end batch script: "

