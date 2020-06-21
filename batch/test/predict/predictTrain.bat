:: Batch file to run predict trainer
:: FHS, Nov 28, 2017

echo 'Running the predict training program'
"C:\Program Files\R\R-3.2.2\bin\x64\Rscript.exe"^
	 E:/Analytics/Rcode/190401_master/source/predict/mainPredictTrain.R^
	 E:/Analytics/Rcode/190401_master/parms/test/predict/predictTrain.prm 1>output.txt 2>errors.txt
