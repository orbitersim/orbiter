DPSLOTS.EXE
------------

Sample application to demonstrate how to build a client/server
application using DirectPlay.

Note: MSVC may include older versions of the DirectX header files and
libraries.  In order to avoid compile errors, make sure the path to the
latest DirectX header files and libraries are listed BEFORE the MSVC header
files and libraries through the Tools -> Options -> Directories menu.

In order to support multi-byte character sets like Kanji, the dialogs in the
rc file need to be changed to use the "SYSTEM" font and the rc file needs to
be recompiled.

CLIENT/SERVER
-------------

DPSLOTS is really two applications in one. When you host a session, it acts
as a server. The server maintains a simple database of account balances for
every user that is allowed to log in. Clients can request their current
balance from the server and can request that the server spin the wheels and
calculate the amount won or lost.

The server uses a simple text file to record the balance for each user. Each
line of the text file contains the user account ID and a 8-character balance
figure separated by a comma, as in:

Account1,    1000
Account2,     500

The default name for this text database file is "slotsdb.txt".  A sample file
that is set up as a template for a secure host, as described below, is
included in the source directory.  You will have to manually add lines to
this file for each user.

When you join a session, DPSLOTS acts as a client and sends messages to the
server for all operations. After requesting a spin from the server, it
animates the tumblers and displays the results. The client may be required
to log the user into the server using a name and password.

SECURITY
--------

DPSLOTS can be hosted using security by clicking the "Require Secure Login"
checkbox after choosing to host.  By leaving the "Security Provider" editbox
blank the default NTLM security package will be used.  If you have an
alternate SSPI security package installed, it may be specified here.

Once this is done, the server will host a secure session and clients will
have to securely log in with a user name and a password before being
allowed access to the server. All messages between the client and server
will be digitally signed and encrypted.

When hosting a secure server, the account ID for each user will be defined
by the security package being used. This account ID must be stored in the
database file along with the appropriate balance.

The default security package is NTLM, which uses a domain and a user name
for the account ID. For example, if your domain is "GAMES" and you have two
users "Bob" and "Jane", your database file would look like:

GAMES/Bob,    1000
GAMES/Jane,     500

Currently you can only host a secure session using NTLM on Windows NT
workstation or server. However, you can log into the server from Windows 95
or Windows NT.
