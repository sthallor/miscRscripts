:: Batch file to run data cleaner
:: FHS, Nov 28, 2017

echo 'Running the data cleaner program'
"C:\Program Files\R\R-3.2.2\bin\x64\Rscript.exe"^
	 E:/Analytics/Rcode/190401_master/source/util/mainDclean.R^
	 E:/Analytics/Rcode/190401_master/parms/test/util/dcleanEDR.prm 1>output.txt 2>errors.txt
