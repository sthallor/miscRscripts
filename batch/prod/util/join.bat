:: Batch file to run join program
:: FHS, Mar 10, 2019

echo 'Running the join program'
"C:\Program Files\R\R-3.2.2\bin\x64\Rscript.exe"^
	 E:/Analytics/Rcode/190401_master/source/util/mainJoin.R^
	 E:/Analytics/Rcode/190401_master/parms/prod/util/join.prm 1>output.txt 2>errors.txt
