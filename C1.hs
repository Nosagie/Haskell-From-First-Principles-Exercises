module Chapter1 where


f1 = y 
    where y = 5

f2 = y * y
    where y = 5

f3 = x * y
    where x = 5; y =6

f4 = x + 3
    where x = 3; y = 1000;

f5 = x * 3 + y
    where y = 1000; x = 3 

f6 = x + 5
    where y = 10; x = 10 * 5 + y

f7 = z / x + y 
    where x = 7;y = (-x);z = y * 10

waxOn = x * 5
    where z = 7; y = z + 8; x = y ^ 2;

triple x =  x * 3

waxOff x =   (triple x) ^ 2 / 2