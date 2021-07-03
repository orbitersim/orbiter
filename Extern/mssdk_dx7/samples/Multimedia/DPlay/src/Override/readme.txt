OVERRIDE.EXE
------------

Sample application to demonstrate how to override the DirectPlay service
provider dialogs.

Note: MSVC may include older versions of the DirectX header files and
libraries.  In order to avoid compile errors, make sure the path to the
latest DirectX header files and libraries are listed BEFORE the MSVC header
files and libraries through the Tools -> Options -> Directories menu.

In order to support multi-byte character sets like Kanji, the dialogs in the
rc file need to be changed to use the "SYSTEM" font and the rc file needs to
be recompiled.

Once the user has selected which service provider to use, the application
displays a custom dialog to collect the information needed for that service
provider.

If the user selected an unknown service provider, then the dialog (if any)
cannot be overidden and the application must prepare for a dialog to be
displayed.

Once the information has been collected from the user, it must be formatted
into a DirectPlay Address which can be used to initialize the service
provider.

EnumSessions must be called with the DPENUMSESSIONS_RETURNSTATUS flag and
Open must be called with the DPOPEN_RETURNSTATUS flag to supress any status
dialogs that may pop up to show to progress of the connection.
