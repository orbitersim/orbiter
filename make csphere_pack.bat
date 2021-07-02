rem ---------------------------------------------------------
rem   Constructs zip file zip\csphere.zip containing the
rem   celestial background images not included in the main
rem   distribution
rem ---------------------------------------------------------

set zip="c:\program files\7-Zip\7z.exe" a -tzip -mx9
if not exist zip mkdir zip

%zip% zip\csphere.zip Config\CSphere\bkgimage.cfg Textures\csphere\h_alpha.tex Textures\csphere\fermi.tex Textures\csphere\planck.tex Textures\csphere\radio.tex Textures\csphere\rass.tex Textures\csphere\wmap*.tex
