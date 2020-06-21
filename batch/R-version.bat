:: R-version.bat
:: Setup R-version for batch files in installation in new location
:: FHS, Feb 18, 2019

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
               "prod\predict\RTanalytics.bat"	       


set str1old="R-3.4.2"
set str1new="R-3.2.2"

:: Loops through filenames1 and performs string substitutions
for %%f in (%filenames1%) do (
	echo/
	echo file:
	echo %%f
	echo Replacing string %str1old% with %str1new% into temp.txt
	CALL "prod\util\BatchSubstitute.bat" %str1old% %str1new% %%f > temp.txt
	MOVE temp.txt %%f
)


echo 'Done.'


SET /P testing="Enter anything to finish batch script: "