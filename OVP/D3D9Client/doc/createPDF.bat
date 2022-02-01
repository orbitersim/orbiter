@echo off
setlocal
:: ----------------------------------------------------------------------------
:: -- setup
:: ----------------------------------------------------------------------------

set WKHTMLTOPDF_PATH=C:\Program Files\wkhtmltopdf\bin
:: set PRG="C:\Program Files\wkhtmltopdf\bin\wkhtmltopdf.exe"
set INPUT=D3D9Client.html
set OUTPUT=..\..\..\Doc\D3D9Client.pdf

:: ----------------------------------------------------------------------------
:: -- check
:: ----------------------------------------------------------------------------

set PRG="%WKHTMLTOPDF_PATH%\wkhtmltopdf.exe"
if not exist %PRG% (
  echo Sorry, could not find wkhtmltopdf at '%WKHTMLTOPDF_PATH%'!
  echo   To create the PDF from the %INPUT% source wkhtmltopdf has to be
  echo   installed ^(http://wkhtmltopdf.org^)!
  echo.
  echo.
  pause
  goto :eof
)


:: ----------------------------------------------------------------------------
:: -- run
:: ----------------------------------------------------------------------------

%PRG% --margin-left 20mm ^
      --margin-right 20mm ^
      --zoom 1.3 ^
      --footer-center "- [page] -" ^
      %INPUT% %OUTPUT%
