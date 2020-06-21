:: TestAll.bat
:: Test all of the batch scripts
:: FHS, March 10, 2019

@ECHO OFF

ECHO Started: %date% %time%

ECHO testing util/dcleanEDR.bat
CALL "util\dcleanEDR.bat"
ECHO dcleanEDR.bat errors.txt file contents
TYPE errors.txt
@ECHO: 
@ECHO:

ECHO testing util/dcleanHistorian.bat
CALL "util\dcleanHistorian.bat"
ECHO dcleanHistorian.bat errors.txt file contents
TYPE errors.txt
@ECHO: 
@ECHO:

ECHO testing util/join.bat
CALL "util\join.bat"
ECHO join.bat errors.txt file contents
TYPE errors.txt
@ECHO: 
@ECHO:

ECHO testing util/offset.bat
CALL "util\offset.bat"
ECHO offset.bat errors.txt file contents
TYPE errors.txt
@ECHO: 
@ECHO:

ECHO testing util/stripchart.bat
CALL "util\stripchart.bat"
ECHO stripchart.bat errors.txt file contents
TYPE errors.txt
@ECHO: 
@ECHO:


ECHO testing util/compare.bat
CALL "util\compare.bat"
ECHO compare.bat errors.txt file contents
TYPE errors.txt
@ECHO: 
@ECHO:


ECHO testing classify/train.bat
CALL "classify\train.bat"
ECHO train.bat errors.txt file contents
TYPE errors.txt
@ECHO: 
@ECHO:

ECHO testing classify/classify.bat
CALL "classify\classify.bat"
ECHO classify.bat errors.txt file contents
TYPE errors.txt
@ECHO: 
@ECHO: 

ECHO testing classify/trainRT.bat
CALL "classify\trainRT.bat"
ECHO trainRT.bat errors.txt file contents
TYPE errors.txt
@ECHO: 
@ECHO:

ECHO testing classify/RTclassify.bat
CALL "classify\RTclassify.bat"
ECHO RTclassify.bat errors.txt file contents
TYPE errors.txt
@ECHO: 
@ECHO: 

:: ECHO testing predict/predictTrain.bat
:: CALL "predict\predictTrain.bat"
:: ECHO predictTrain.bat errors.txt file contents
:: TYPE errors.txt
:: @ECHO: 
:: @ECHO: 

:: ECHO testing predict/predict.bat
:: CALL "predict\predict.bat"
:: ECHO predict.bat errors.txt file contents
:: TYPE errors.txt
:: @ECHO: 
:: @ECHO: 

:: ECHO testing predict/predictStripchart.bat
:: CALL "predict\predictStripchart.bat"
:: ECHO predictStripchart.bat errors.txt file contents
:: TYPE errors.txt
:: @ECHO: 
:: @ECHO: 

:: ECHO testing predict/RTpredict.bat
:: CALL "predict\RTpredict.bat"
:: ECHO RTpredict.bat errors.txt file contents
:: TYPE errors.txt
:: @ECHO: 
:: @ECHO: 

:: ECHO testing predict/RTanalytics.bat
:: CALL "predict\RTanalytics.bat"
:: ECHO RTanalytics.bat errors.txt file contents
:: TYPE errors.txt
:: @ECHO: 
:: @ECHO: 

ECHO Finished: %date% %time%

@ECHO:
ECHO 'Done.'
DEL errors.txt
DEL output.txt


SET /P testing="Enter anything to finish batch script: "