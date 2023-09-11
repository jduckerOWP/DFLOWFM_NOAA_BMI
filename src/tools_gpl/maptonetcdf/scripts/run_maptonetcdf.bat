@ echo off
title run_maptonetcdf
    rem
    rem This script runs maptonetcdf on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

    rem
    rem Set the input arguments
    rem
set mapfile= 
if [%1] EQU [] (
    goto usage
) else (
    if [%1] EQU [--help] (
        goto usage
    ) else (
        set mapfile=%1
    )
)
set ncfile= 
if [%2] EQU [] (
    goto usage
) else (
    if [%2] EQU [--help] (
        goto usage
    ) else (
        set ncfile=%2
    )
)
set numLayers= 
if [%3] EQU [] (
    goto usage
) else (
    if [%3] EQU [--help] (
        goto usage
    ) else (
        set numLayers=%3
    )
)


set workdir=%CD%
set mapfile=%workdir%\%mapfile%
set ncfile=%workdir%\%ncfile%

echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem
set D3D_HOME=%~dp0..\..\..

rem Remove "\dwaq\scripts\..\..\.." from D3D_HOME
set D3DT=%D3D_HOME:~0,-22%

rem last directory will be the architecture directory
for %%f in ("%D3DT%") do set ARCH=%%~nxf

set waqdir=%D3D_HOME%\%ARCH%\dwaq\bin

set sharedir=%D3D_HOME%\%ARCH%\share\bin
set PATH=%waqdir%;%sharedir%

    rem
    rem No adaptions needed below
    rem

    rem Run
set PATH=%waqdir%;%sharedir%;%~dp0

    rem go to directory, run maptonetcdf, and return
For %%A in ("%mapfile%") do (
    set mapName=%%~nxA
    set mapPath=%%~dpA
)
cd /d "%mapPath%"
echo executing in this window: "%waqdir%\maptonetcdf.exe" "%mapfile%" "%ncfile%" "%numLayers%"
"%waqdir%\maptonetcdf.exe" "%mapfile%" "%ncfile%" "%numLayers%"
cd /d "%workdir%"



goto end

:usage
echo Usage:
echo run_maptonetcdf.bat [--help] mapFile.map ncFile.nc numLayers
echo     --help             : (Optional) show this usage
echo     mapFile.map        : (Mandatory) maptonetcdf .map input file.
echo     ncFile.nc          : (Mandatory) maptonetcdf .nc output file.
echo     numLayers          : (Mandatory) number of layers.
:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
