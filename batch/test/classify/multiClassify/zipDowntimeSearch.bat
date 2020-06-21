:: search.BAT
:: DOS batch file to recursively search directories with zipped ASCII files
::      for keywords and list each row into a single file.
:: unzips file, searches file with R program downtimeSearch.R, and deletes unzipped file
:: FHS, May 16, 2016
@ECHO OFF

:: dirname: Directory to start with, will look in all subdirectories
:: zipfile: name of zip file to look for in each directory
:: csvfile: name of extracted zip file
 
set dirname="C:/Users/Fred.Seymour/Rcode/TestData"

:: set dirname="E:/ace/WellData"


set "zipfile=/input_EDR_file_clean_output.zip"
set csvfile="input_EDR_file_clean_output.csv"

for /f "tokens=*" %%G in ('dir /b /s /a:d %dirname%') do if exist "%%G%zipfile%" (
   echo processing "%%G%zipfile%"
   "C:/Users/Fred.Seymour/Rcode/7-Zip/7z.exe" e "%%G%zipfile%"
   echo scanning %csvfile%
   "C:\Program Files\R\R-3.2.2\bin\x64\Rscript.exe" /Users/Fred.Seymour/Rcode/master/mhealth/downtimeSearch.R %csvfile%
   del %csvfile%
)

SET /P testing="Enter anything to finish batch script: "



