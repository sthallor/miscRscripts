:: fixPath.bat
:: Setup pathnames for installation in new location
:: FHS, Apr 1, 2019

@ECHO OFF

:: List of Rcode, parameters, and batch filenames on which to change directory trees
set filenames1="test\classify\classify.bat"^
	       "test\classify\RTclassify.bat"^
               "test\classify\train.bat"^
               "test\classify\trainRT.bat"^
               "test\util\dcleanEDR.bat"^
               "test\util\dcleanHistorian.bat"^
	       "test\util\join.bat"^
               "test\util\offset.bat"^
               "test\util\stripchart.bat"^
               "test\util\compare.bat"^
               "test\predict\predictTrain.bat"^
               "test\predict\predict.bat"^
               "test\predict\predictStripchart.bat"^
               "test\predict\RTpredict.bat"^
               "test\predict\RTanalytics.bat"^
               "prod\classify\classify.bat"^
	       "prod\classify\RTclassify.bat"^
               "prod\classify\train.bat"^
               "prod\classify\trainRT.bat"^
               "prod\util\dcleanEDR.bat"^
               "prod\util\dcleanHistorian.bat"^
	       "prod\util\join.bat"^
               "prod\util\offset.bat"^
               "prod\util\stripchart.bat"^
               "prod\util\compare.bat"^
               "prod\predict\predictTrain.bat"^
               "prod\predict\predict.bat"^
               "prod\predict\predictStripchart.bat"^
               "prod\predict\RTpredict.bat"^
               "prod\predict\RTanalytics.bat"^
	       "..\parms\test\classify\classify.prm"^
	       "..\parms\test\classify\dcleanRT.prm"^
               "..\parms\test\classify\RTclassify.prm"^
               "..\parms\test\classify\train.prm"^
               "..\parms\test\classify\trainRT.prm"^
               "..\parms\test\util\dcleanEDR.prm"^
               "..\parms\test\util\dcleanHistorian.prm"^
               "..\parms\test\util\join.prm"^
               "..\parms\test\util\offset.prm"^
               "..\parms\test\util\stripchart.prm"^
               "..\parms\test\util\compare.prm"^
               "..\parms\test\predict\predictTrain.prm"^
               "..\parms\test\predict\predict.prm"^
               "..\parms\test\predict\predictStripchart.prm"^
               "..\parms\test\predict\RTpredict.prm"^
               "..\parms\test\predict\RTclassify.prm"^
	       "..\parms\prod\classify\classify.prm"^
	       "..\parms\prod\classify\dcleanRT.prm"^
               "..\parms\prod\classify\RTclassify.prm"^
               "..\parms\prod\classify\train.prm"^
               "..\parms\prod\classify\trainRT.prm"^
               "..\parms\prod\util\dcleanEDR.prm"^
               "..\parms\prod\util\dcleanHistorian.prm"^
               "..\parms\prod\util\join.prm"^
               "..\parms\prod\util\offset.prm"^
               "..\parms\prod\util\stripchart.prm"^
               "..\parms\prod\util\compare.prm"^
               "..\parms\prod\predict\predictTrain.prm"^
               "..\parms\prod\predict\predict.prm"^
               "..\parms\prod\predict\predictStripchart.prm"^
               "..\parms\prod\predict\RTpredict.prm"^
               "..\parms\prod\predict\RTclassify.prm"^
               "..\source\classify\mainTrain.R"^
               "..\source\classify\mainClassify.R"^
               "..\source\classify\mainRTClassify.R"^
               "..\source\util\mainDclean.R"^
               "..\source\util\mainJoin.R"^
               "..\source\util\mainOffset.R"^
               "..\source\util\mainCompare.R"^
               "..\source\predict\mainPredictTrain.R"^
               "..\source\predict\mainPredict.R"^
               "..\source\predict\mainPredictStripchart.R"^
               "..\source\predict\mainRTPredict.R"^
               "..\source\predict\mainRTanalytics.R"^
               "..\source\graphics\mainStripchart.R"^
               "..\source\graphics\stripchart.Rmd"


set str1old="Rcode/master"
set str1new="Rcode/190401_master"
set str2old="C:/Users/Fred.Seymour/"
set str2new="E:/Analytics/"

:: Loops through filenames1 and performs string substitutions
for %%f in (%filenames1%) do (
	echo/
	echo file:
	echo %%f
	echo Replacing string %str1old% with %str1new% into temp.txt
	CALL "prod\util\BatchSubstitute.bat" %str1old% %str1new% %%f > temp.txt
	echo Replacing string %str2old% with %str2new% into temp1.txt
	CALL "prod\util\BatchSubstitute.bat" %str2old% %str2new% temp.txt > temp1.txt
	MOVE temp1.txt %%f
)

del temp

echo 'Done.'


SET /P testing="Enter anything to finish batch script: "