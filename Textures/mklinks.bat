set tgtdir=%1

for %%p in (Mercury Earth Moon Mars Phobos Deimos Vesta Jupiter Io Ganymede Callisto Europa Mimas Enceladus Tethys Dione Rhea Iapetus Titan Hyperion) do (
	if exist %%p rmdir %%p
	if not exist %%p mklink /J %%p %tgtdir%\%%p
)
