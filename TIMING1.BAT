echo start timing1 > timing1.dat
stackey ! CR
time   >> timing1.dat
sf -o C:*.* >> timing1.dat
sf -o D:*.* >> timing1.dat
echo stop  timing1 >> timing1.dat
stackey ! CR
time   >> timing1.dat
