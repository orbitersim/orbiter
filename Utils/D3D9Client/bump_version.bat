@set @junk=1 /*
@echo off
set @junk=
if %PROCESSOR_ARCHITECTURE%==x86 (
  cscript //nologo //E:jscript %0 %*
) else (
  %windir%\SysWOW64\cscript //nologo //E:jscript %0 %*
)
goto :eof
*/
// -----------------------------------------------------------------------------
// InputBox Helper code
// from https://code.google.com/archive/p/jsxt/
//      http://with-love-from-siberia.blogspot.de/2009/12/msgbox-inputbox-in-jscript.html
// -----------------------------------------------------------------------------
var vb = {
  Function : function (func) {
    return function () {
      return vb.Function.eval.call(this, func, arguments);
    };
  }
};
vb.Function.eval = function (func)
{
  var args = Array.prototype.slice.call(arguments[1]);
  for (var i = 0; i < args.length; i++) {
    if (typeof args[i] != "string") {
      continue;
    }
    args[i] = args[i].replace(/["\n\r]/g, function($0) {
      return '" + Chr(' + $0.charCodeAt($0) + ') + "';
    });
    args[i] = '"' + args[i] + '"';
  }

  var vbe;
  vbe = new ActiveXObject("ScriptControl");
  vbe.Language = "VBScript";

  return vbe.eval(func + "(" + args.join(",") + ")");
};

/**
 * InputBox(prompt[, title][, default][, xpos][, ypos][, helpfile, context])
 */
var InputBox = vb.Function("InputBox");

/**
 * MsgBox(prompt[, buttons][, title][, helpfile, context])
 */
var MsgBox = vb.Function("MsgBox");
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
// Replacement tasks setup
// -----------------------------------------------------------------------------
var tasks = [{
     file : "Orbitersdk/D3D9Client/D3D9Client.cpp",
     rexp : /D3D9Client(?:\s+Beta)?\s+(?:R)?([\d\.]+)\s/,
    subst : "{Major}.{Minor}" // "{MajorMinor}"
  }, {
     file : "Orbitersdk/D3D9Client/D3D9Client.rc",
     rexp : /FILEVERSION\s+((\d+),(\d+),(\d+),(\d+))/,
    subst : "{Major},{Minor},{Build},{Release}"
  }, {
     file : "Orbitersdk/D3D9Client/D3D9Client.rc",
     rexp : /PRODUCTVERSION\s+((\d+),(\d+),(\d+),(\d+))/,
    subst : "{Major},0,0,0"
  }, {
     file : "Orbitersdk/D3D9Client/D3D9Client.rc",
     rexp : /VALUE\s+\"FileVersion\"\,\s+\"((\d+)\.(\d+)\.(\d+)\.(\d+))\"/,
    subst : "{Major}.{Minor}.{Build}.{Release}"
  }, {
     file : "Orbitersdk/D3D9Client/D3D9Client.rc",
     rexp : /VALUE\s+\"ProductVersion\"\,\s+\"((\d+)\.(\d+)\.(\d+)\.(\d+))\"/,
    subst : "{Major}.0.0.0"
  }, {
     file : "Orbitersdk/D3D9Client/doc/Doxyfile",
     rexp : /PROJECT_NUMBER\s*=\s*\"(?:Beta|R)?\s*([\d\.]+)\"/,
    subst : "{MajorMinor}",
     unix : !true // UNIX EOL
  }, {
     file : "Orbitersdk/D3D9Client/doc/Doxyfile-gcAPI",
     rexp : /PROJECT_NUMBER\s*=\s*\"(?:Beta|R)?\s*([\d\.]+)\"/,
    subst : "{MajorMinor}",
     unix : !true // UNIX EOL
  }, {
     file : "Utils/D3D9Client/build_release.bat",
     rexp : /set VERSION=(?:Beta)?(?:R)?([\d\.]+)$/,
    subst : "{Major}.{Minor}" // "{MajorMinor}"
  }
];
// -----------------------------------------------------------------------------

// Convert to DOS path-separators
for (var i=0; i<tasks.length; ++i) {
  tasks[i].file = tasks[i].file.replace('/','\\');
}

// -----------------------------------------------------------------------------
// Main
// -----------------------------------------------------------------------------
var WshShell = WScript.CreateObject ("WScript.Shell");
WshShell.CurrentDirectory = "..\\..";

var fso = new ActiveXObject("Scripting.FileSystemObject");
var ForReading   = 1; // const
var ForWriting   = 2;
var ForAppending = 8;

var BetaStr = "";

function getCurrentVersion () {
  var version = [0,0,0,0];
  var f = fso.OpenTextFile(tasks[0].file, ForReading);
  while (!f.AtEndOfStream) {
    var line = f.ReadLine();
    var matches = line.match(tasks[0].rexp);
    if (matches !== null) {
      var vs = matches[1].split(".");
      for (var i=0; i<vs.length && i<3; ++i) { version[i] = vs[i]; }
      var m2 = line.match(/D3D9Client\s+(Beta)/);
      if (m2 && m2[1]) {
        BetaStr = m2[1];
      }
      break;
    }
  }
  f.Close();
  return version.join("."); // {String}
}

// Ask user for (new) version number
var Title   = "D3D9Client Version Bumper";
var Message = "Enter (new) version number:"
            + "\n"
            + "Only numeric values separated by dot (.), please!"
            + "\n\n"
            + "Format: Major[.Minor[.Build[.Release]"
            + "\n"
            + 'Exmples: "1.2"  or "21.0.8.15"'
            ;
var versionString = InputBox(Message, Title, getCurrentVersion());
if (versionString==null) {
  WScript.Quit(666);
}
// MsgBox("You choose: "+versionString,1);

// Prepare the "replacement macros"
var v = versionString.split(".");
var Major      = parseInt(v[0]||"0", 10);
var Minor      = parseInt(v[1]||"0", 10);
var Build      = parseInt(v[2]||"0", 10);
var Release    = parseInt(v[3]||"0", 10);
var MajorMinor = ""+Major+(Minor?("."+Minor):"");

for (var i=0; i<tasks.length; ++i) {
  var task = tasks[i];
  task.subst = task.subst.replace("{Major}", Major);
  task.subst = task.subst.replace("{Minor}", Minor);
  task.subst = task.subst.replace("{Build}", Build);
  task.subst = task.subst.replace("{Release}", Release);
  task.subst = task.subst.replace("{MajorMinor}", MajorMinor);
}

var _files = [];
// For each task ...
var lastFile = null;
for (var i=0; i<tasks.length; ++i)
{
  var task  = tasks[i];  // local 'shortcuts'
  var file  = task.file, //  "       "
      rexp  = task.rexp,
      subst = task.subst;

  // --- Info ---
  if (lastFile != file) {
    _files.push(file);
    WScript.Echo("- "+file);
    lastFile = file;
  }

  // --- Search & Replace ---
  var lines = [];
  var f = fso.OpenTextFile(file, ForReading);
  while (!f.AtEndOfStream) {
    var line = f.ReadLine();
    var matches = line.match(rexp);
    if (matches !== null) {
      // WScript.Echo("> "+line);
      // WScript.Echo(">>" +line.replace(matches[1], subst) );
      lines.push( line.replace(matches[1], subst) );
    } else {
      lines.push( line );
    }
  }
  f.Close(); // needed?

  // --- Write back ---
  // WScript.Echo( lines.join("\r\n") );
  var f = fso.OpenTextFile(file, ForWriting);
  if (task.unix) {
    f.Write( lines.join("\n") + "\n" );
  } else {
    for (var j=0,k=lines.length; j<k; ++j) {
      f.WriteLine( lines[j] );
    }
  }
  f.Close();
}
var logmsg = "- bumped version to " + [BetaStr,MajorMinor].join(" ");
var svn = "TortoiseProc.exe"
        + " /command:commit"
        + " /path:\"" + _files.join("*") + "\""
        + " /logmsg:\"" + logmsg + "\""
        + " /closeonend:0"
        ;
WshShell.Run(svn, 1, true);

WScript.Echo("Done!");