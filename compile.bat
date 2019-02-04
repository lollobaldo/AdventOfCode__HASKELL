echo Compiling code for Day %1 challenge
cd "Day %1
ghc -o %1ab %1ab.hs -O2
%1ab.exe
cd ../