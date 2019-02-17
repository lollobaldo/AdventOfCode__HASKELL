echo Compiling code for Day %1 challenge
cd "Day %1
ghc -o %1 %1.hs -O2
%1.exe
cd ../