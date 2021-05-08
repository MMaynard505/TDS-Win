echo start timing1 > timing3.dat
stackey ! CR
time   >> timing3.dat
sf -o C:*.* >> timing3.dat
sf -o D:*.* >> timing3.dat
echo stop  timing1 >> timing3.dat
stackey ! CR
time   >> timing3.dat
echo start timing2 >> timing3.dat
stackey ! CR
time    >> timing3.dat
sf    C:*.* >> timing3.dat
sf    D:*.* >> timing3.dat
echo stop  timing2 >> timing3.dat
stackey ! CR
time   >> timing3.dat
