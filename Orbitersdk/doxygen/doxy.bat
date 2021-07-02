"c:\program files\doxygen\bin\doxygen.exe" Doxyfile 2> doxy.log
rem copy API_reference\*.png html
rem del html\_v_e_s_s_e_l2_8cpp-example.html
rem copy html\_v_e_s_s_e_l2_8cpp-example.html.2 html\_v_e_s_s_e_l2_8cpp-example.html
rem del latex\_v_e_s_s_e_l2_8cpp-example.tex
rem copy latex\_v_e_s_s_e_l2_8cpp-example.tex.2 latex\_v_e_s_s_e_l2_8cpp-example.tex

cd latex
pdflatex refman.tex
makeindex refman.idx
pdflatex refman.tex
rem copy refman.pdf ..\..\doc\API_Reference.pdf
cd ..

exit 0
