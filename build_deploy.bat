cd ..
rename Orbiter_release Orbiter_release.bak
rename Orbiter_deploy Orbiter_release
cd Orbiter_src

"c:\Program Files (x86)\Microsoft Visual Studio 9.0\Common7\IDE\devenv" /build Release Orbiter.sln
"c:\Program Files (x86)\Microsoft Visual Studio 9.0\Common7\IDE\devenv" /build "Release Server" Orbiter.sln
"c:\Program Files (x86)\Microsoft Visual Studio 9.0\Common7\IDE\devenv" /project Orbiter_wix /build Release Orbiter.sln

cd ..
rename Orbiter_release Orbiter_deploy
rename Orbiter_release.bak Orbiter_release
cd Orbiter_src