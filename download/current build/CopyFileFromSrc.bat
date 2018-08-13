@echo off
xcopy "..\..\src\Win32\Debug\DocumentDropper.exe" "Win32" /Y
xcopy "..\..\src\Win64\Debug\DocumentDropper.exe" "Win64" /Y
:: xcopy "..\..\src\OSX32\Debug\DocumentDropper.app" "macOS\DocumentDropper.app" /O /X /E /H /K