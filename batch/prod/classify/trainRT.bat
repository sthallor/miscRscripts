:: Batch file to run rig state trainer
:: FHS, Mar 10, 2019

echo 'Running the classifier training program'
"C:\Program Files\R\R-3.2.2\bin\x64\Rscript.exe"^
	 E:/Analytics/Rcode/190401_master/source/classify/mainTrain.R^
	 E:/Analytics/Rcode/190401_master/parms/prod/classify/trainRT.prm 1>output.txt 2>errors.txt
