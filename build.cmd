@echo off
setlocal

path %path%;C:\Users\maxim\Documents\Code\C#\bmp2tile;C:\Users\maxim\Documents\Code\C\wla-dx\binaries

for %%f in (*.1bpp.png) do bmp2tile.exe "%%f" -noremovedupes -nomirror -savetiles "%%~nf" || exit /b

call Compile.bat "X-Terminator 2.1.gg.asm"
