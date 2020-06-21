:: Batch file to run offset program
:: FHS, Mar 18, 2019

echo 'Running the stripchart program'
"C:\Program Files\R\R-3.4.2\bin\x64\Rscript.exe"^
	 C:/Users/Fred.Seymour/Rcode/master/source/graphics/mainStripchart.R^
	 C:/Users/Fred.Seymour/Rcode/master/batch/test/classify/multiClassify/stripchart.prm 1>output.txt 2>errors.txt