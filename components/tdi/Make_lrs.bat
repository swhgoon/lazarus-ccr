@ECHO OFF
@ECHO OFF

if "%1" == "" goto param_default

if NOT EXIST %1\lazarus.exe goto error_param

if NOT EXIST %1\tools\lazres.exe GOTO error_lazres

del tdi.lrs
%1\tools\lazres.exe tdi.lrs ttdinotebook.png

GOTO end

:param_default
Make_lrs.bat c:\lazarus
goto :end

:error_param
ECHO "Please Inform Lazarus folder. For instance:"
echo.
ECHO "Make_lrs.bat c:\lazarus"
goto :end

:error_lazres
ECHO "Arquivo:"
echo %1\tools\lazres.exe
ECHO "was not found... Please compile the project \lazarus\tools\lazres.lpi"
goto end

:end
echo "Press ENTER to leave" .
pause