@rem ---------------------------------------------------------------------------
@rem -- setup
@rem ---------------------------------------------------------------------------

@set WKHTMLTOPDF_PATH=C:\Program Files\wkhtmltopdf\bin
@rem set PRG="C:\Program Files\wkhtmltopdf\bin\wkhtmltopdf.exe"
@set INPUT=D3D9Client.html
@set OUTPUT=..\..\..\Doc\D3D9Client.pdf

@rem ---------------------------------------------------------------------------
@rem -- check
@rem ---------------------------------------------------------------------------

@set PRG="%WKHTMLTOPDF_PATH%\wkhtmltopdf.exe"
@if not exist %PRG% (
  @echo Sorry, could not find wkhtmltopdf at '%WKHTMLTOPDF_PATH%'!
  @echo   To create the PDF from the %INPUT% source wkhtmltopdf has to be
  @echo   installed ^(http://wkhtmltopdf.org^)!
  @echo.
  @echo.
  @pause
  @goto :eof
)


@rem ---------------------------------------------------------------------------
@rem -- run
@rem ---------------------------------------------------------------------------

@%PRG% --margin-left 25mm --zoom 1.3 %INPUT% %OUTPUT%


@rem ---------------------------------------------------------------------------
@rem -- cleanup
@rem ---------------------------------------------------------------------------

@set WKHTMLTOPDF_PATH=
@set PRG=
@set INPUT=
@set OUTPUT=
