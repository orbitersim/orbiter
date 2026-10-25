import os
import sys
import subprocess
import shutil
import urllib.request
import urllib.error
import stat
import time
import json
import ssl
import hashlib
import threading
import concurrent.futures

# ANSI escape
if sys.platform == 'win32':
    os.system("")

class Colors:
    HEADER = '\033[95m'    # Magenta
    INFO = '\033[96m'      # Cyan
    SUCCESS = '\033[92m'   # Green
    WARNING = '\033[93m'   # Yellow
    ERROR = '\033[91m'     # Red
    RESET = '\033[0m'      # Reset

printLock = threading.Lock()

def tsPrint(*args, **kwargs):
    with printLock:
        print(*args, **kwargs)

def printHeader(text): tsPrint(f"\n{Colors.HEADER}{text}{Colors.RESET}")
def printSuccess(text): tsPrint(f"{Colors.SUCCESS}[SUCCESS]  {text}{Colors.RESET}")
def printWarning(text): tsPrint(f"{Colors.WARNING}[WARNING]  {text}{Colors.RESET}")
def printError(text): tsPrint(f"{Colors.ERROR}[ERROR]  {text}{Colors.RESET}")
def printInfo(text): tsPrint(f"{Colors.INFO}{text}{Colors.RESET}")

def removeReadOnly(action, filePath, _):
    os.chmod(filePath, stat.S_IWRITE)
    action(filePath)

def runCommand(commandArgs, workingDirectory=None, ignoreExitCode=False, quiet=False):
    commandString = ' '.join(str(arg) for arg in commandArgs)
    printInfo(f"[Running]: {commandString}")
    
    try:
        if quiet:
            executionResult = subprocess.run(commandArgs, cwd=workingDirectory, capture_output=True, text=True)
        else:
            executionResult = subprocess.run(commandArgs, cwd=workingDirectory)
        
        if executionResult.returncode != 0 and not ignoreExitCode:
            printError(f"Command failed with exit code {executionResult.returncode}.")
            
            if quiet:
                tsPrint(f"\n{Colors.ERROR}OUTPUT LOG{Colors.RESET}")
                if executionResult.stdout: tsPrint(executionResult.stdout)
                if executionResult.stderr: tsPrint(executionResult.stderr)
                tsPrint(f"{Colors.ERROR}============================={Colors.RESET}")
                
            # Loops on failure
            raise RuntimeError(f"Command execution failed with exit code {executionResult.returncode}")
            
        return executionResult.returncode
        
    except FileNotFoundError as error:
        printError(f"Command executable not found: {error}")
        raise RuntimeError("Missing executable.")

def downloadWithRetry(downloadUrl, destinationFile, toolName, expectedSizeMb=None, maxRetries=3):
    opener = urllib.request.build_opener()
    opener.addheaders = [('User-Agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64)')]
    urllib.request.install_opener(opener)

    tempFile = destinationFile + ".part"
    for attempt in range(1, maxRetries + 1):
        try:
            printInfo(f"[{toolName}] Downloading: {destinationFile} (Attempt {attempt}/{maxRetries})")
            
            # Progress tracking hook
            lastProgress = [0]
            def reportHook(blockNum, blockSize, totalSize):
                downloaded = blockNum * blockSize
                
                # Fallback to expectedSizeMb if we have no Content-Length header
                actualTotal = totalSize
                if actualTotal <= 0 and expectedSizeMb:
                    actualTotal = expectedSizeMb * 1024 * 1024
                
                if actualTotal > 0:
                    percent = int((downloaded / actualTotal) * 100)
                    percent = min(percent, 100) # Cap at 100 in case expected size is slightly off
                    
                    # 5% step size
                    if percent >= lastProgress[0] + 5:
                        barLength = 20
                        filled = int(barLength * percent // 100)
                        bar = '#' * filled + '-' * (barLength - filled)
                        
                        printInfo(f"[{toolName}] [{bar}] {percent}%")
                        lastProgress[0] = (percent // 5) * 5
                else:
                    # Absolute fallback
                    mbDownloaded = downloaded / (1024 * 1024)
                    if mbDownloaded >= lastProgress[0] + 5:
                        printInfo(f"[{toolName}] Downloaded: {mbDownloaded:.1f} MB")
                        lastProgress[0] = (int(mbDownloaded) // 5) * 5

            urllib.request.urlretrieve(downloadUrl, tempFile, reporthook=reportHook)
            os.replace(tempFile, destinationFile)
            printSuccess(f"[{toolName}] Download complete.")
            return True
            
        except (urllib.error.URLError, ValueError, ConnectionResetError) as error:
            if os.path.exists(tempFile):
                os.remove(tempFile)
            printWarning(f"[{toolName}] Download failed: {error}")
            if attempt < maxRetries:
                time.sleep(5)
            else:
                printError(f"[{toolName}] Failed to download after {maxRetries} attempts.")
                raise RuntimeError("Download failed.")

def verifySystemRequirements():
    printHeader("Running OS dependency checks")
    if sys.platform != 'win32':
        printError("This build script works exclusively for Windows.")
        sys.exit(1)
    currentWorkingDirectory = os.getcwd()
    try:
        testFilePath = os.path.join(currentWorkingDirectory, '.test.tmp')
        with open(testFilePath, 'w') as testFile: testFile.write("test")
        os.remove(testFilePath)
    except Exception:
        printError(f"Missing write permissions in {currentWorkingDirectory}")
        sys.exit(1)
    printSuccess("Requirements met.")

def getLatestGithubRelease(repo, suffix, fallbackUrl):
    url = f"https://api.github.com/repos/{repo}/releases/latest"
    try:
        req = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
        with urllib.request.urlopen(req, timeout=10) as response:
            data = json.loads(response.read().decode('utf-8'))
            for asset in data.get('assets', []):
                if asset['name'].endswith(suffix):
                    return asset['browser_download_url']
    except Exception:
        pass
    return fallbackUrl

def checkGit():
    if shutil.which("git"): return True
    if os.path.exists(r"C:\Program Files\Git\cmd\git.exe"):
        os.environ["PATH"] += os.pathsep + r"C:\Program Files\Git\cmd"
        return True
    return False

def checkCmake():
    if shutil.which("cmake"): return True
    if os.path.exists(r"C:\Program Files\CMake\bin\cmake.exe"):
        os.environ["PATH"] += os.pathsep + r"C:\Program Files\CMake\bin"
        return True
    return False

def getVsState():
    vsLocatorPath = os.path.expandvars(r"%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe")
    if not os.path.exists(vsLocatorPath):
        return False, False, None, None
        
    cmd = [vsLocatorPath, "-products", "*", "-requires", "Microsoft.VisualStudio.Component.VC.Tools.x86.x64", "-format", "json"]
    try:
        res = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdoutStr = res.stdout.decode('utf-8', errors='ignore').strip()
        if not stdoutStr:
            return False, False, None, None
            
        installations = json.loads(stdoutStr)
        for inst in installations:
            path = inst.get("installationPath", "")
            version = inst.get("installationVersion", "")
            
            msvcToolsDir = os.path.join(path, "VC", "Tools", "MSVC")
            if os.path.exists(path) and os.path.exists(msvcToolsDir):
                hasMfc = False
                try:
                    for verFolder in os.listdir(msvcToolsDir):
                        if os.path.exists(os.path.join(msvcToolsDir, verFolder, "atlmfc", "include", "atlstr.h")):
                            hasMfc = True
                            break
                except Exception:
                    pass
                return True, hasMfc, path, version
    except Exception:
        pass
    return False, False, None, None

def checkVs():
    isInstalled, hasMfc, _, _ = getVsState()
    return isInstalled and hasMfc

def getValidDxSdk():
    envDir = os.environ.get("DXSDK_DIR", "")
    if envDir and os.path.exists(os.path.join(envDir, "Include", "d3d9.h")):
        return envDir
        
    defaultDir = r"C:\Program Files (x86)\Microsoft DirectX SDK (June 2010)"
    if os.path.exists(os.path.join(defaultDir, "Include", "d3d9.h")):
        return defaultDir
    return None

def checkDx():
    return getValidDxSdk() is not None

TOOLS_CONFIG = {
    "Git": {
        "check": checkGit,
        "urlFunc": lambda: getLatestGithubRelease("git-for-windows/git", "64-bit.exe", "https://github.com/git-for-windows/git/releases/download/v2.44.0.windows.1/Git-2.44.0-64-bit.exe"),
        "installer": "Git-Installer.exe",
        "installCmd": ["powershell", "-NoProfile", "-Command", "try { Start-Process -FilePath '{path}' -ArgumentList '/VERYSILENT /NORESTART /NOCANCEL /SP- /CLOSEAPPLICATIONS /RESTARTAPPLICATIONS' -Wait -Verb RunAs -ErrorAction Stop } catch { exit 1 }"],
        "postInstall": lambda: os.environ.update({"PATH": os.environ["PATH"] + os.pathsep + r"C:\Program Files\Git\cmd"})
    },
    "CMake": {
        "check": checkCmake,
        "urlFunc": lambda: getLatestGithubRelease("Kitware/CMake", "windows-x86_64.msi", "https://github.com/Kitware/CMake/releases/download/v3.29.3/cmake-3.29.3-windows-x86_64.msi"),
        "installer": "CMake-Installer.msi",
        "installCmd": ["powershell", "-NoProfile", "-Command", "try { Start-Process -FilePath 'msiexec.exe' -ArgumentList '/i \"{path}\" /qn /norestart ADD_CMAKE_TO_PATH=System' -Wait -Verb RunAs -ErrorAction Stop } catch { exit 1 }"],
        "postInstall": lambda: os.environ.update({"PATH": os.environ["PATH"] + os.pathsep + r"C:\Program Files\CMake\bin"})
    },
    "VS Build Tools": {
        "check": checkVs,
        "urlFunc": lambda: "https://aka.ms/vs/17/release/vs_buildtools.exe",
        "installer": "vs_buildtools.exe",
        "installCmd": [],
        "postInstall": lambda: None
    },
    "DirectX SDK": {
        "check": checkDx,
        "urlFunc": lambda: "https://download.microsoft.com/download/A/E/7/AE743F1F-632B-4809-87A9-AA1BB3458E31/DXSDK_Jun10.exe",
        "installer": "DXSDK_Jun10.exe",
        "expectedSize": 571,
        "installCmd": ["powershell", "-NoProfile", "-Command", "try { Start-Process -FilePath '{path}' -ArgumentList '/U' -Wait -Verb RunAs -ErrorAction Stop } catch { exit 1 }"],
        "postInstall": lambda: None
    }
}

SPICE_ASSETS = {
    "SPICE Kernels": {
        "url": "https://cdn.openorbiter.space/spice/Kernels.7z",
        "checksumUrl": "https://cdn.openorbiter.space/spice/Kernels.7z.sha256sum",
        "archive": "Kernels.7z",
        "targetSubdir": "Kernels"
    },
    "SPICE Textures": {
        "url": "https://cdn.openorbiter.space/spice/Textures.7z",
        "checksumUrl": "https://cdn.openorbiter.space/spice/Textures.7z.sha256sum",
        "archive": "Textures.7z",
        "targetSubdir": "Textures"
    }
}

def resolveMissingPrerequisites():
    printHeader("Scanning for prerequisite build tools")
    
    missingTools = [name for name, cfg in TOOLS_CONFIG.items() if not cfg["check"]()]
    if not missingTools:
        printSuccess("All required build tools found.")
        return

    printWarning("Missing or corrupted tools detected: " + ", ".join(missingTools))
    if input(f"\n{Colors.HEADER}Download and install? [Y/n]: {Colors.RESET}").strip().lower() not in ['', 'y', 'yes']:
        sys.exit(1)

    toolsToDownload = [tool for tool in missingTools if not os.path.exists(TOOLS_CONFIG[tool]["installer"])]
    
    if toolsToDownload:
        printHeader("Downloading required installers...")
        with concurrent.futures.ThreadPoolExecutor(max_workers=len(toolsToDownload)) as executor:
            futureToTool = {}
            for tool in toolsToDownload:
                config = TOOLS_CONFIG[tool]
                future = executor.submit(
                    downloadWithRetry, 
                    config["urlFunc"](), 
                    config["installer"], 
                    tool, 
                    config.get("expectedSize")
                )
                futureToTool[future] = tool
            
            for future in concurrent.futures.as_completed(futureToTool):
                tool = futureToTool[future]
                try:
                    future.result()
                except Exception as exc:
                    printError(f"[{tool}] Download process generated an exception: {exc}")
                    sys.exit(1)

    # This has to be sequential because Microsoft is dumb
    printHeader("Installing missing dependencies...")
    for tool in missingTools:
        config = TOOLS_CONFIG[tool]
        absInstaller = os.path.abspath(config["installer"]).replace("'", "''")
        
        if tool == "VS Build Tools":
            isInstalled, hasMfc, vsPath, _ = getVsState()
            setupExe = r"C:\Program Files (x86)\Microsoft Visual Studio\Installer\setup.exe"
            
            if isInstalled and not hasMfc and os.path.exists(setupExe):
                printInfo(f"[{tool}] Using local VS Installer core to inject the ATLMFC package...")
                psArgs = f"modify --installPath `\"{vsPath}`\" --add Microsoft.VisualStudio.Component.VC.ATLMFC --passive --norestart"
                cmd = ["powershell", "-NoProfile", "-Command", f"Start-Process -FilePath '{setupExe}' -ArgumentList \"{psArgs}\" -Wait -Verb RunAs"]
                runCommand(cmd, ignoreExitCode=True)
                
                printInfo("Waiting for VS Installer to finish writing ATLMFC files...")
                for _ in range(60): 
                    _, hasMfcNow, _, _ = getVsState()
                    if hasMfcNow:
                        printSuccess("ATLMFC files detected")
                        break
                    time.sleep(5)
            else:
                printInfo(f"[{tool}] Installing fresh C++ Build Tools...")
                psArgs = f"--passive --wait --norestart --nocache --add Microsoft.VisualStudio.Workload.VCTools --add Microsoft.VisualStudio.Component.VC.ATLMFC --includeRecommended"
                cmd = ["powershell", "-NoProfile", "-Command", f"Start-Process -FilePath '{absInstaller}' -ArgumentList \"{psArgs}\" -Wait -Verb RunAs"]
                runCommand(cmd, ignoreExitCode=True)
            
        else:
            printInfo(f"[{tool}] Installing... (Accept UAC Prompts. This may take a few minutes.)")
            res = runCommand([c.replace('{path}', absInstaller) for c in config["installCmd"]], ignoreExitCode=True)
            if tool == "DirectX SDK" and res != 0:
                printWarning("[DirectX SDK] Installer threw a known bug error code (S1023). Bypassing safely.")
                
        config["postInstall"]()

    if any(not TOOLS_CONFIG[t]["check"]() for t in missingTools):
        printError("Some tools failed to verify after installation. Please reboot and try again.")
        sys.exit(1)
    printSuccess("All dependencies successfully installed!")

def manageRepository(repositoryUrl, targetDirectory):
    printHeader(f"Setting up repository: {targetDirectory}")
    if os.path.exists(targetDirectory) and os.path.exists(os.path.join(targetDirectory, "CMakeLists.txt")):
        printInfo("Pulling latest updates...")
        runCommand(["git", "pull"], workingDirectory=targetDirectory)
        runCommand(["git", "submodule", "update", "--init", "--recursive"], workingDirectory=targetDirectory)
    else:
        if os.path.exists(targetDirectory): shutil.rmtree(targetDirectory, onerror=removeReadOnly)
        runCommand(["git", "clone", "--recursive", repositoryUrl, targetDirectory])

def getValidVsInstance():
    isInstalled, hasMfc, path, version = getVsState()
    if isInstalled and hasMfc and version:
        if version.startswith("17."): return "Visual Studio 17 2022", path
        if version.startswith("16."): return "Visual Studio 16 2019", path
        if version.startswith("15."): return "Visual Studio 15 2017", path
    return None, None

def getSystemTar():
    systemTar = os.path.join(os.environ.get("SystemRoot", r"C:\Windows"), "System32", "tar.exe")
    if os.path.exists(systemTar):
        return systemTar
    return shutil.which("tar") or "tar.exe"

def calculateSha256(filePath):
    sha256Hash = hashlib.sha256()
    with open(filePath, "rb") as file:
        for chunk in iter(lambda: file.read(1024 * 1024), b""):
            sha256Hash.update(chunk)
    return sha256Hash.hexdigest().lower()

def fetchRemoteSha256(checksumUrl, assetName):
    try:
        req = urllib.request.Request(
            checksumUrl,
            headers={'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64)'}
        )
        with urllib.request.urlopen(req, timeout=10) as response:
            content = response.read().decode('utf-8-sig', errors='replace').strip()
            if content:
                # Supports both "<hash>  <filename>" (standard sha256sum output) and raw "<hash>"
                remoteHash = content.split()[0].lower()
                if len(remoteHash) == 64 and all(c in '0123456789abcdef' for c in remoteHash):
                    return remoteHash
            printWarning(f"[{assetName}] Invalid checksum format received from {checksumUrl}.")
    except Exception as error:
        printWarning(f"[{assetName}] Could not fetch remote checksum from {checksumUrl}: {error}")
    return None

def installSpiceAssets(gameInstallDirectory):
    printHeader("Setting up SPICE Kernels and Textures")
    
    archivesToDownload = []
    remoteChecksums = {}

    for name, cfg in SPICE_ASSETS.items():
        archivePath = cfg["archive"]
        checksumUrl = cfg.get("checksumUrl", f"{cfg['url']}.sha256sum")
        remoteHash = fetchRemoteSha256(checksumUrl, name)
        remoteChecksums[name] = remoteHash

        if not os.path.exists(archivePath):
            printInfo(f"[{name}] Local archive '{archivePath}' not found. Queuing download...")
            archivesToDownload.append(name)
        elif remoteHash:
            printInfo(f"[{name}] Checking SHA-256 checksum for '{archivePath}'...")
            localHash = calculateSha256(archivePath)
            if localHash != remoteHash:
                printWarning(f"[{name}] Checksum mismatch (local: {localHash[:12]}..., remote: {remoteHash[:12]}...). Queuing update...")
                archivesToDownload.append(name)
            else:
                printSuccess(f"[{name}] Local archive '{archivePath}' checksum verified and up to date.")
        else:
            printWarning(f"[{name}] Remote checksum unavailable; using cached '{archivePath}'.")
    
    if archivesToDownload:
        printInfo("Downloading required SPICE assets...")
        with concurrent.futures.ThreadPoolExecutor(max_workers=len(archivesToDownload)) as executor:
            futureToAsset = {
                executor.submit(
                    downloadWithRetry,
                    SPICE_ASSETS[name]["url"],
                    SPICE_ASSETS[name]["archive"],
                    name,
                    SPICE_ASSETS[name].get("expectedSize")
                ): name
                for name in archivesToDownload
            }
            for future in concurrent.futures.as_completed(futureToAsset):
                assetName = futureToAsset[future]
                try:
                    future.result()
                    expectedHash = remoteChecksums.get(assetName)
                    if expectedHash:
                        downloadedHash = calculateSha256(SPICE_ASSETS[assetName]["archive"])
                        if downloadedHash != expectedHash:
                            raise RuntimeError(
                                f"SHA-256 mismatch after download (expected {expectedHash}, got {downloadedHash})"
                            )
                        printSuccess(f"[{assetName}] SHA-256 checksum verified.")
                except Exception as exc:
                    printError(f"[{assetName}] Asset download failed: {exc}")
                    raise RuntimeError(f"Failed to download {assetName}")

    tarExe = getSystemTar()
    for name, cfg in SPICE_ASSETS.items():
        archivePath = os.path.abspath(cfg["archive"])
        extractDir = os.path.join(gameInstallDirectory, cfg["targetSubdir"])
        os.makedirs(extractDir, exist_ok=True)
        
        printInfo(f"[{name}] Extracting {cfg['archive']} into {extractDir}...")
        res = runCommand([tarExe, "-xf", archivePath, "-C", extractDir], ignoreExitCode=True, quiet=True)
        if res != 0:
            printWarning(f"[{name}] Archive extraction failed; re-downloading {cfg['archive']}...")
            if os.path.exists(archivePath):
                os.remove(archivePath)
            downloadWithRetry(cfg["url"], cfg["archive"], name, cfg.get("expectedSize"))
            expectedHash = remoteChecksums.get(name)
            if expectedHash:
                downloadedHash = calculateSha256(archivePath)
                if downloadedHash != expectedHash:
                    raise RuntimeError(
                        f"[{name}] SHA-256 mismatch after re-download (expected {expectedHash}, got {downloadedHash})"
                    )
            runCommand([tarExe, "-xf", archivePath, "-C", extractDir], quiet=True)

        # Guard against double-nested Kernels/Kernels directory if the archive contains a top-level folder
        nestedDir = os.path.join(extractDir, cfg["targetSubdir"])
        if os.path.isdir(nestedDir):
            for item in os.listdir(nestedDir):
                src = os.path.join(nestedDir, item)
                dst = os.path.join(extractDir, item)
                if os.path.exists(dst):
                    if os.path.isdir(dst):
                        shutil.rmtree(dst, onerror=removeReadOnly)
                    else:
                        os.remove(dst)
                shutil.move(src, dst)
            os.rmdir(nestedDir)
            
    printSuccess("SPICE kernels and textures installed successfully.")

def buildAndInstall(targetDirectory):
    os.environ.pop("CC", None)
    os.environ.pop("CXX", None)
    os.environ.pop("CMAKE_GENERATOR", None)

    printHeader("Configuring CMake build")
    buildDirectory = "build"
    absoluteBuildDir = os.path.abspath(os.path.join(targetDirectory, buildDirectory)) # Just in case I need this later on, unused
    
    htmlHelpIncludePath = os.path.abspath(os.path.join(targetDirectory, "Extern", "Htmlhelp", "include"))
    htmlHelpLibraryPath = os.path.abspath(os.path.join(targetDirectory, "Extern", "Htmlhelp", "lib-x86", "Htmlhelp.Lib"))
    
    generator, instancePath = getValidVsInstance()
    
    if generator:
        printSuccess(f"Using CMake Generator: {generator}")
    else:
        printWarning("Could not determine specific CMake Generator. Automatically choosing the default.")
    
    dxDir = getValidDxSdk()
    if dxDir:
        if not dxDir.endswith('\\'):
            dxDir += '\\'
        os.environ["DXSDK_DIR"] = dxDir
    
    cmakeConfigCommand = [
        "cmake",
        "-B", buildDirectory,
        "-S", ".",
        "-A", "Win32",
        "-DCMAKE_INSTALL_PREFIX=install",
        "-DCMAKE_POLICY_VERSION_MINIMUM=3.5",
        f"-DHTML_HELP_INCLUDE_PATH={htmlHelpIncludePath}",
        f"-DHTML_HELP_LIBRARY={htmlHelpLibraryPath}",
        "-DHTML_HELP_COMPILER=hhc.exe"
    ]
    
    if generator:
        cmakeConfigCommand.insert(1, generator)
        cmakeConfigCommand.insert(1, "-G")
        
    if instancePath and generator:
        cmakeConfigCommand.append(f"-DCMAKE_GENERATOR_INSTANCE={instancePath}")
    
    runCommand(cmakeConfigCommand, workingDirectory=targetDirectory, quiet=True)

    printHeader("Compiling Project")
    cmakeBuildCommand = ["cmake", "--build", buildDirectory, "--config", "Release", "--parallel"]
    runCommand(cmakeBuildCommand, workingDirectory=targetDirectory, quiet=False)
    
    printHeader("Assembling Game Files")
    cmakeInstallCommand = ["cmake", "--install", buildDirectory, "--config", "Release"]
    runCommand(cmakeInstallCommand, workingDirectory=targetDirectory, quiet=True)
    
    gameInstallDirectory = os.path.abspath(os.path.join(targetDirectory, 'install'))
    orbiterRootDirectory = os.path.join(gameInstallDirectory, 'Orbiter')
    if not os.path.isdir(orbiterRootDirectory):
        orbiterRootDirectory = gameInstallDirectory
    installSpiceAssets(orbiterRootDirectory)
    
    tsPrint("\n" + "="*60)
    tsPrint(f"{Colors.SUCCESS}SUCCESS: Orbiter built successfully!{Colors.RESET}")
    tsPrint("="*60)
    tsPrint(f"{Colors.INFO}Build directory: {gameInstallDirectory}{Colors.RESET}")
    tsPrint("="*60 + "\n")

def main():
    try:
        verifySystemRequirements()
        resolveMissingPrerequisites()
    except KeyboardInterrupt:
        printWarning("\nSetup interrupted by user. Exiting.")
        sys.exit(1)
    except Exception as e:
        printError(f"\nFailed during prerequisite setup: {e}")
        sys.exit(1)

    while True:
        try:
            printHeader("Orbiter Build Tool")
            print(f"{Colors.INFO}1.{Colors.RESET} Clone or pull & build master repository")
            print(f"{Colors.INFO}2.{Colors.RESET} Clone or pull & build a custom remote repository")
            print(f"{Colors.INFO}3.{Colors.RESET} Build an existing local repository")
            print(f"{Colors.INFO}4.{Colors.RESET} Exit")
            
            choice = input(f"\n{Colors.HEADER}Select an option: {Colors.RESET}").strip()
            
            if choice == '1':
                manageRepository("https://github.com/orbitersim/orbiter.git", "OpenOrbiter")
                buildAndInstall("OpenOrbiter")
                
            elif choice == '2':
                repoUrl = input(f"{Colors.INFO}Enter remote repository URL: {Colors.RESET}").strip()
                if not repoUrl:
                    printError("Repository URL cannot be empty.")
                    continue
                targetDir = input(f"{Colors.INFO}Enter target directory name (e.g., CustomOrbiter): {Colors.RESET}").strip()
                if not targetDir:
                    printWarning("Directory name empty. Defaulting to 'CustomOrbiter'.")
                    targetDir = "CustomOrbiter"
                
                manageRepository(repoUrl, targetDir)
                buildAndInstall(targetDir)
                
            elif choice == '3':
                # Grab the current directory for the prompt default
                currentDir = os.getcwd()
                
                localPath = input(f"{Colors.INFO}Enter the full path to the local repository or press ENTER for the current one: {Colors.RESET}").strip().strip('"').strip("'")
                
                # Default to the current directory if input is empty
                if not localPath:
                    localPath = currentDir
                
                if not os.path.isdir(localPath):
                    printError(f"Directory '{localPath}' does not exist.")
                    continue
                if not os.path.exists(os.path.join(localPath, "CMakeLists.txt")):
                    printWarning("Warning: No CMakeLists.txt found in the root of the specified directory. Build will likely fail.")
                
                # No manageRepository() since we're fully local
                buildAndInstall(localPath)
                
            elif choice == '4':
                printSuccess("Bye!")
                break
                
            else:
                printError("Invalid option. Please enter a number between 1 and 4.")
                
        except KeyboardInterrupt:
            printWarning("\nOperation interrupted by user. Returning to menu...")
        except Exception as e:
            printError(f"\nAn error occurred: {e}")
            printWarning("Returning to main menu...\n")

if __name__ == "__main__":
    main()
