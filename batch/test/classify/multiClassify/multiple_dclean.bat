:: classifyMultipleYYYYY.bat
:: dclean list of drill rig files in a single run
:: FHS, Mar 26, 2019

@ECHO OFF

:: List of Drill Rig filenames on which to run classifier program

:: set filenames="664367" "666801" "667284"

:: set filenames="664003"  

set filenames="381576" "381578" "381911" "382099" "382921"^
              "383082" "383105" "383226" "383706" "383980"^
              "384565" "384569" "384576" "384579" "384581"^
              "661952" "663851" "664003" "664009" "664013"^
              "664367" "664513" "666660" "666801" "667284"^
              "667461" "667741" "667747" "668018" "668019"

:: Loops through the DR files and performs classifications one at a time
for %%f in (%filenames%) do (
	echo/
	echo/
	echo/
	echo Preparing data cleaning parameters for file:
	echo %%f
	CALL "C:\Users\Fred.Seymour\Rcode\master\batch\test\classify\multiClassify\BatchSubstitute.bat" YYYYY %%f dcleanEDRYYYYY.prm > dclean.prm
	echo 'Running the data clean program'
	CALL "C:\Program Files\R\R-3.4.2\bin\x64\Rscript.exe"^
	 /Users/Fred.Seymour/Rcode/master/source/util/mainDclean.R^
	 /Users/Fred.Seymour/Rcode/master/batch/test/classify/multiClassify/dclean.prm
)

echo 'Done.'
SET /P testing="Enter something to exit: "
