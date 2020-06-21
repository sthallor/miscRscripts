:: overallRFVotes.BAT
:: DOS batch file to place Classifier Output Overall RF Confidence Vote for each Well
::      into a single file, this will help find outliers
:: FHS, Mar 27, 2019
@ECHO OFF

:: Directory to start with, will look in all subdirectories
:: File filter restricts file to output text files only
:: Keywords for lines to collect from output text file
:: outfile, filename for text file containing output results
set dirname="C:\Users\Fred.Seymour\EDR_Data\190310_BatchTest"
set "filefilter=\*clean_clean_classify_log.txt"
set keywords="OverallRFVote"
set outfile="RFVotes_500trees_Model2C.txt"


:: Documentation of the long crytic DOS BATCH command
:: dir /b -- only list the files
:: dir /s -- recursively list directories and files in subfolders
:: dir /a:d %dirname% -- directory listing starting with 'dirname'
:: find %keywords% "filename" -- like grep, finds lines in text file that contain %keywords%
::     the quotes "" on filename allows blank spaces in filenames
:: for /f "tokens=*" %%G in ('command1') do 'command2' -- iteratively takes each line output from command1
::     and executes command2
:: if exist "filename" find %keywords% "filename" -- makes sure that file exists before
::     executing find command, this avoids missing file error message in find command
:: | find %keywords% -- pipe through filter again to removes filename lines that are part of find output
:: > %outfile% -- redirect output to text file for further processing  

if exist %outfile% del %outfile%

:: for /f "tokens=*" %%G in ('dir /b /a:d %dirname%') do if exist "%%G%filefilter%" find %keywords% "%%G%filefilter%" | find %keywords% >> %outfile%

:: with the /s option that recursively lists directories and files in subfolders
for /f "tokens=*" %%G in ('dir /b /s /a:d %dirname%') do if exist "%%G%filefilter%" find %keywords% "%%G%filefilter%" | find %keywords% >> %outfile%

SET /P testing="Enter something to end script: "





