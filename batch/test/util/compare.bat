:: Batch file to run join program
:: FHS, Feb 18, 2019

echo 'Running the compare program'
"C:\Program Files\R\R-3.2.2\bin\x64\Rscript.exe"^
	 E:/Analytics/Rcode/190401_master/source/util/mainCompare.R^
	 E:/Analytics/Rcode/190401_master/parms/test/util/compare.prm 1>output.txt 2>errors.txt
