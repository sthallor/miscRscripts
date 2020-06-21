:: Batch file to run offset program
:: FHS, Nov 30, 2017

echo 'Running the stripchart program'
"C:\Program Files\R\R-3.2.2\bin\x64\Rscript.exe"^
	 E:/Analytics/Rcode/190401_master/source/graphics/mainStripchart.R^
	 E:/Analytics/Rcode/190401_master/parms/test/util/stripchart.prm 1>output.txt 2>errors.txt
