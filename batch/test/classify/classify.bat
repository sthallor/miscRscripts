:: Batch file to run rig state classifier
:: FHS, Nov 28, 2017

echo 'Running the rig state classifier program'
"C:\Program Files\R\R-3.2.2\bin\x64\Rscript.exe"^
	 E:/Analytics/Rcode/190401_master/source/classify/mainClassify.R^
	 E:/Analytics/Rcode/190401_master/parms/test/classify/classify.prm 1>output.txt 2>errors.txt
