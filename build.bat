echo "COMPILATION..."
set root=D:\zx-spectrum
%root%\tools\compiler\zxb.exe %root%\src\main.bas --autorun -B -t -o %root%\build\build.tap
%root%\tools\emu\Speccy.exe %root%\build\build.tap
pause
