@echo off

rem Usage: convpm rootname
rem For example. If you have a file mesh.x run as "convpm mesh"

if not exist %1.x goto error_nofile

rem Option: merge faces topologically based on vertex geometry (-gmerge)
set filteropt1=
set filteropt1=-gmerge -genus


rem Option: regenerate normals based on dihedral angles between faces
set filteropt2=


rem Option: speed up simplification by including fewer edges in initial priority queue
set simpopt1=


rem Option: improve simplification fidelity, specify number of points (default= #vertices)
set simpopt2=

set filter1=-genus -triangulate -genus %filteropt1% -fixvertices -fixfaces
set filter2=-rmcomp 0 -renormalizenor -genus -nice %filteropt2%


rem Start of the conversion process

echo Converting X File to Internal mesh format
xtobm.exe %1.x
if errorlevel 1 goto error_xtobm

echo Filtering internal mesh format file
fmesh.exe %1.m %filter1% %filter2% > %1.nice.m
if errorlevel 1 goto error_filtermesh
echo .

echo Simplifying mesh to a base mesh and a set of vertex splits
meshsimp.exe %1.nice.m -prog %1.prog %simpopt1% %simpopt2% -simplify >%1.base.m
if errorlevel 1 goto error_simplify

rlines.exe %1.prog >%1.rprog
if errorlevel 1 goto error_reverse
echo .

echo Converting Base Mesh and Vsplits to .X file
fprog.exe -fbase %1.base.m -fprog %1.rprog -pm >%1.pm
if errorlevel 1 goto error_filterprog

pmtopmx.exe %1.pm
if errorlevel 1 goto error_pmtopmx

echo .
pmxtox.exe %1.pmx
if errorlevel 1 goto error_pmxtox

echo All Done!
goto exit

:error_nofile
echo %1.x doesn't exist. Usage convpm base_filename
goto exit

:error_xtobm
echo Converting the X File to Internal mesh format failed
goto exit

:error_filtermesh
echo .
echo Filtering the internal mesh failed
goto exit

:error_simplify
echo .
echo Simplifying the mesh failed
goto exit

:error_reverse
echo .
echo Reverselines failed
goto exit

:error_filterprog
echo .
echo Creating the PM file failed
goto exit

:error_pmtopmx
echo .
echo Converting the PM to PMX file format failed
goto exit

:error_pmxtox
echo .
echo Converting the PMX file to an X File failed
goto exit

:exit
rem cleanup on exit
if exist %1.m del %1.m
if exist mesh.mat del mesh.mat
if exist %1.nice.m del %1.nice.m
if exist %1.base.m del %1.base.m
if exist %1.prog del %1.prog
if exist %1.rprog del %1.rprog
if exist %1.pm del %1.pm
if exist %1.pmx del %1.pmx
