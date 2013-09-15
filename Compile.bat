@ECHO OFF

:A

ghc -package GLUT -o out/GLTest.exe -odir out/o/ -hidir out/hi/ Main.hs
PAUSE

GOTO A