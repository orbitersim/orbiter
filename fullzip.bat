del orbiter.zip
del orbiter.7z

"\Program Files\7-Zip\7z" a -tzip -mx9 orbiter.zip @filelist.dat
"\Program Files\7-Zip\7z" a -t7z -mx9 orbiter.7z @filelist.dat
