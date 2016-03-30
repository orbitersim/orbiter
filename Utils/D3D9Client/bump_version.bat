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
     file : "Orbitersdk\\D3D9Client\\D3D9Client.cpp",
     rexp : /D3D9Client (?:Beta)? (\S+)(?: Debug)? Build/,
    subst : "{MajorMinor}" // "{Major}.{Minor}"
  }, {
     file : "Orbitersdk\\D3D9Client\\D3D9Client.rc",
     rexp : /FILEVERSION\s+((\d+),(\d+),(\d+),(\d+))/,
    subst : "{Major},{Minor},{Build},{Release}"
  }, {
     file : "Orbitersdk\\D3D9Client\\D3D9Client.rc",
     rexp : /PRODUCTVERSION\s+((\d+),(\d+),(\d+),(\d+))/,
    subst : "{Major},0,0,0"
  }, {
     file : "Orbitersdk\\D3D9Client\\D3D9Client.rc",
     rexp : /VALUE\s+\"FileVersion\"\,\s+\"((\d+).(\d+).(\d+).(\d+))\"/,
    subst : "{Major}.{Minor}.{Build}.{Release}"
  }, {
     file : "Orbitersdk\\D3D9Client\\D3D9Client.rc",
     rexp : /VALUE\s+\"ProductVersion\"\,\s+\"((\d+).(\d+).(\d+).(\d+))\"/,
    subst : "{Major}.0.0.0"
  }, {
     file : "Utils\\D3D9Client\\build_release.bat",
     rexp : /set VERSION=(?:Beta)?([\d\.]+)$/,
    subst : "{MajorMinor}"
  }
];
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
// Main
// -----------------------------------------------------------------------------
var WshShell = WScript.CreateObject ("WScript.Shell");
WshShell.CurrentDirectory = "..\\..";

var fso = new ActiveXObject("Scripting.FileSystemObject");
var ForReading   = 1; // const
var ForWriting   = 2;
var ForAppending = 8;

function getCurrentVersion () {
  var version = [0,0,0,0];
  var f = fso.OpenTextFile(tasks[0].file, ForReading);
  while (!f.AtEndOfStream) {
    var line = f.ReadLine();
    var matches = line.match(tasks[0].rexp);
    if (matches !== null) {
      var vs = matches[1].split(".");
      for (var i=0; i<vs.length && i<3; ++i) { version[i] = vs[i]; }
      break;
    }
  }
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
// MsgBox("You choose: "+versionString,1);

// Prepare the "replacement macros"
var v = versionString.split(".");
var Major      = parseInt(v[0], 10);
var Minor      = parseInt(v[1], 10);
var Build      = parseInt(v[2], 10);
var Release    = parseInt(v[3], 10);
var MajorMinor = ""+Major+(Minor?("."+Minor):"");

for (var i=0; i<tasks.length; ++i) {
  var task = tasks[i];
  task.subst = task.subst.replace("{Major}", Major);
  task.subst = task.subst.replace("{Minor}", Minor);
  task.subst = task.subst.replace("{Build}", Build);
  task.subst = task.subst.replace("{Release}", Release);
  task.subst = task.subst.replace("{MajorMinor}", MajorMinor);
}

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
  // f.Write( lines.join("\r\n") );
  for (var j=0,k=lines.length; j<k; ++j) {
    f.WriteLine( lines[j] );
  }
  f.Close();
}
WScript.Echo("Done!");