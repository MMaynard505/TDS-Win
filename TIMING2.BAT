echo start timing2 > timing2.dat
stackey ! CR
time    >> timing2.dat
sf    C:*.* >> timing2.dat
sf    D:*.* >> timing2.dat
echo stop  timing2 >> timing2.dat
stackey ! CR
time   >> timing2.dat
