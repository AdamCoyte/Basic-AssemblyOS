@echo off

SET PATH=.\;

:: Assemble the operating system
ML.EXE /nologo /AT /c os.asm

:: Link the operating system, the warning is normal
LINK.EXE /nologo /TINY /NOD os.obj, os.flp, NUL, NUL, NUL

PAUSE
