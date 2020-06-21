:: Batch file to run real time rig state classifier
:: FHS, Mar 10, 2019

echo 'Running the real time rig state classifier program'
"C:\Program Files\R\R-3.2.2\bin\x64\Rscript.exe"^
	 E:/Analytics/Rcode/190401_master/source/classify/mainRTclassify.R^
	 E:/Analytics/Rcode/190401_master/parms/prod/classify/RTclassify.prm 1>output.txt 2>errors.txt
