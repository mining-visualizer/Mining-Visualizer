
REM the expression %~dp0 returns the drive and folder in which this batch file is located
 
cd %~dp0
pushd ..

REM Compile the program 

D:\Programs\Lazarus-1.6\lazbuild --build-all --build-mode=Release "MiningVisualizer.lpi"
IF ERRORLEVEL 1 GOTO ERROR

REM delete the contents of the Release folder

del Release\*.* /S /Q
FOR /D %%p IN ("Release\*.*") DO rmdir "%%p" /s /q

copy MiningVisualizer.exe Release
IF ERRORLEVEL 1 GOTO ERROR
xcopy /i /s Rainmeter Release\Rainmeter
IF ERRORLEVEL 1 GOTO ERROR
xcopy /i /s WebApp Release\WebApp
IF ERRORLEVEL 1 GOTO ERROR
xcopy /i /s SSL-Certs Release\SSL-Certs
IF ERRORLEVEL 1 GOTO ERROR

del MiningVisualizer.zip
powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::CreateFromDirectory('.\Release', 'MiningVisualizer.zip'); }"
move MiningVisualizer.zip Release

popd
@ECHO OFF
echo.
echo.
echo =============================================
echo.
echo.
echo All Done!
echo.
echo.
echo.
echo.
echo.
goto OUT

:ERROR

popd
@ECHO OFF
echo.
echo.
echo =============================================
echo.
echo.
echo ERRORS WERE ENCOUNTERED !!!
echo.
echo.
echo.
echo.
echo.

:OUT
pause