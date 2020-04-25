import Data.Char
import Data.List

m1 = [(a,b,c,d,e) | x <- [10..], a <- [0,1..],b <- [0,1..],c <- [0,1..],d <- [0,1..],e <- [0,1..], a*3^4+b*3^3+c*3^2+d*3+e == 0]

m314gu = length [[x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] | 
 x0 <- [1..6], x1 <- [1..6], x2 <- [1..6], x3 <- [1..6], x4 <- [1..6], x5 <- [1..6], x6 <- [1..6], x7 <- [1..6], x8 <- [1..6], x9 <- [1..6],
 x0 /= 1 && x1 /= 1 && x2 /= 1 && x3 /= 1 && x4 /= 1 && x5 /= 1 && x6 /= 1 && x7 /= 1 && x8 /= 1 && x9 /= 1 || 
 x0 /= 2 && x1 /= 2 && x2 /= 2 && x3 /= 2 && x4 /= 2 && x5 /= 2 && x6 /= 2 && x7 /= 2 && x8 /= 2 && x9 /= 2 || 
 x0 /= 3 && x1 /= 3 && x2 /= 3 && x3 /= 3 && x4 /= 3 && x5 /= 3 && x6 /= 3 && x7 /= 3 && x8 /= 3 && x9 /= 3 || 
 x0 /= 4 && x1 /= 4 && x2 /= 4 && x3 /= 4 && x4 /= 4 && x5 /= 4 && x6 /= 4 && x7 /= 4 && x8 /= 4 && x9 /= 4 || 
 x0 /= 5 && x1 /= 5 && x2 /= 5 && x3 /= 5 && x4 /= 5 && x5 /= 5 && x6 /= 5 && x7 /= 5 && x8 /= 5 && x9 /= 5 || 
 x0 /= 6 && x1 /= 6 && x2 /= 6 && x3 /= 6 && x4 /= 6 && x5 /= 6 && x6 /= 6 && x7 /= 6 && x8 /= 6 && x9 /= 6 ]

asd = length [(0,x,y,13) | x <- [1..12],y <- [1..12], x /= y, x < y ]
-- mathe3b1a4 = [(((n^2 -3*n + 3) `div` n^2),n) | n <- [1..1000], (n^2 -3*n + 3) / n^2 >= 0.99]

-- ferma = [(x,y,z,n) | x <- [1..100],y <- [1..100],z <- [1..],n <- [1..],n >= 3,x^n+y^n==z^n]

sub1 x y = [(x)]
sub2 x y = [(a,b)     | a <- [1..y],b <- [1..y], (a-b) == x ]
sub3 x y = [(a,b,c)   | a <- [1..y],b <- [1..y],c <- [1..y], (a-b-c) == x ]
sub4 x y = [(a,b,c,d) | a <- [1..y],b <- [1..y],c <- [1..y],d <- [1..y], (a-b-c-d) == x ]

add1 x y = [(x) ]
add2 x y = [(a,b) | a <- [1..y],b <- [1..y],a <= b ,(a+b) == x ]
add3 x y = [(a,b,c) | a <- [1..y],b <- [1..y],c <- [1..y],a <= b, b <= c, (a+b+c) == x ]
add4 x y = [(a,b,c,d) | a <- [1..y],b <- [1..y],c <- [1..y],d <- [1..y],a <= b, b <= c, c <= d, (a+b+c+d) == x ]

mul1 x y = [(x) ]
mul2 x y = [(a,b) | a <- [1..y],b <- [1..y],a <= b ,(a*b) == x ]
mul3 x y = [(a,b,c) | a <- [1..y],b <- [1..y],c <- [1..y],a <= b, b <= c, (a*b*c) == x ]
mul4 x y = [(a,b,c,d) | a <- [1..y],b <- [1..y],c <- [1..y],d <- [1..y],a <= b, b <= c, c <= d, (a*b*c*d) == x ]
mul5 x y = [(a,b,c,d,e) | a <- [1..y],b <- [1..y],c <- [1..y],d <- [1..y],e <- [1..y],a <= b, b <= c, c <= d,d <= e , (a*b*c*d*e) == x ]
selB ([],ys) = []
selB (xs,[]) = []
selB ((x:xs),y:ys) = y:x:(selB (ys,xs))

seltest = ([1,2,3,4,5,6],[10,20,30,40])

oplus3:: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
oplus3 (a,b,c) (x,y,z) = ((mod (a+x) 3),(mod (b+y) 3),(mod (c+z) 3) )

mathef53 = Data.List.sort(nub [(v,x,y,z) | a <- [0..5], b <- [0..5], c <- [0..5],(v,x,y,z) <- [vektorsum (vektormultilam a (1,2,2,3)) (vektormultilam b (2,4,3,4)) (vektormultilam c (3,1,0,2))]])

vektorsum (q,w,e,r) (a,s,d,f) (y,x,c,v) = ((mod (q+a+y) 5),(mod (w+s+x) 5),(mod (e+d+c) 5),(mod (r+f+v) 5))

vektormultilam x (a,b,c,d) = ((mod (x*a) 5),(mod (x*b) 5),(mod (x*c) 5),(mod (x*d) 5))

mathef54 = [(a,b,c,d) | a <- [0..5], b <- [0..5], c <- [0..5], d <- [0..5]]

mathef27 = [(a,b,c,d,e,f,g) | a <- [0,1], b <- [0,1], c <- [0,1], d <- [0,1], e <- [0,1], f <- [0,1], g <- [0,1]]

mathe5 = nub [dist3 a b | a <- [(0,0,0),(1,0,1),(2,0,2),(1,2,0),(2,1,0),(0,2,2),(0,1,1),(2,2,1),(1,1,2)],b <- [(0,0,0),(1,0,1),(2,0,2),(1,2,0),(2,1,0),(0,2,2),(0,1,1),(2,2,1),(1,1,2)], a/=b ]

mathe2c = [(a,b,c,d,e) | a <- [0,1,2], b <- [0,1,2], c <- [0,1,2], d <- [0,1,2], e <- [0,1,2], dist5 (a,b,c,d,e) (0,0,0,0,0) == 4, dist5 (a,b,c,d,e) (0,1,1,1,1) == 4, dist5 (a,b,c,d,e) (1,0,1,2,2) == 4,dist5 (a,b,c,d,e)(1,2,2,0,1) == 4,dist5 (a,b,c,d,e)(2,1,2,2,0) == 4,dist5 (a,b,c,d,e) (2,2,0,1,2) == 4 ]

mathe2d = [(a,b,c,d,e) | a <- [0,1], b <- [0,1], c <- [0,1], d <- [0,1], e <- [0,1]]

dist5 (a,b,c,d,e) (v,w,x,y,z)
 | a==v = dist4 (b,c,d,e) (w,x,y,z)
 | otherwise = 1 + dist4 (b,c,d,e) (w,x,y,z)

dist4 (a,b,c,d) (w,x,y,z)
 | a==w = dist3 (b,c,d) (x,y,z)
 | otherwise = 1 + dist3 (b,c,d) (x,y,z)

dist3 (b,c,d) (x,y,z)
 | b==x = dist2 (c,d) (y,z)
 | otherwise = 1 +  dist2 (c,d) (y,z)

dist2 (c,d) (y,z)
 | c==y = dist1 (d) (z)
 | otherwise = 1 + dist1 (d) (z)

dist1 (d) (z)
 | d == z = 0
 | otherwise = 1

mathe2try = [(a,b,c,d) | a <- [0,1,2], b <- [0,1,2], c <- [0,1,2], d <- [0,1,2], dist4 (a,b,c,d) (0,1,0,1) == 2 ]

mathe2 = [(a,b,c,d) | a <- [0,1], b <- [0,1], c <- [0,1], d <- [0,1]]

mathe2a = [[a,b,c,d] | a <- mathe2, b <- mathe2, c <- mathe2, d <- mathe2, length (nub [a,b,c,d]) == 4]

ep1 = [x | x<- [1..1000], mod x 3 == 0 || mod x 5 == 0] 

ep2 1 = 1
ep2 2 = 1
ep2 x = ep2 (x-1) + ep2 (x-2)
--ep2e = sum [y | x <- [1..500], y <- [1..500], y == ep2 x, mod y 2 == 0,y < 4000000]

-- test = prime 600851475143
-- test = ep3 600851475143
-- ep3help = []
-- ep3 x = [y | y <- [x..0], mod x head (prime x)) == 0]

prims (0,x) = 0
prims (1,x) = 0
prims (n,x)
    | mod n (n-x) == 0 = 0
    | n-x==2 = n
    | otherwise = prims(n,(x+1))

prime 0 = []
prime 1 = []
prime n 
    | prims(n,1) == 0 = (prime(n-1))
    | otherwise = prims(n,1):(prime(n-1))

-- ep4 = filter (palin (toS)) [x*y*z | x <- [100..999], y <- [100..999], z <- [100..999]]
-- 
-- toSting x = show x
-- palin (x:xs)
--  | x == last xs = True
--  | otherwise = False


-- zahl :: [Char]
-- zahl = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
-- 
-- 
-- count (x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:xs) y
--     | y <  digitToInt x1 * digitToInt x2 * digitToInt x3 * digitToInt x4 * digitToInt x5 * digitToInt x6 * digitToInt x7 * digitToInt x8 * digitToInt x9 * digitToInt x10* digitToInt x11 * digitToInt x12 * digitToInt x13 = count (x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:xs) (digitToInt x1 * digitToInt x2 * digitToInt x3 * digitToInt x4 * digitToInt x5 * digitToInt x6 * digitToInt x7 * digitToInt x8 * digitToInt x9 * digitToInt x10* digitToInt x11 * digitToInt x12 * digitToInt x13)
--     | y >= digitToInt x1 * digitToInt x2 * digitToInt x3 * digitToInt x4 * digitToInt x5 * digitToInt x6 * digitToInt x7 * digitToInt x8 * digitToInt x9 * digitToInt x10* digitToInt x11 * digitToInt x12 * digitToInt x13 = count (x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:xs) y
-- 
-- count _ y = y

-- ep5 = [(a*a) + (b*b) + (c*c)| a <- [1..100], b <- [1..100], c <- [1..100], a<b,b<c,a+b+c]

-- alle mit max hamming abstand 3
a2 = [(a,b,c,d)| a <- [0..6], b <- [0..6], c <- [0..6], d <- [0..6], a==0||b==0||c==0||d==0]

-- nur eine 0
a3 = [(a,b,c,d)| a <- [0..6], b <- [0..6], c <- [0..6], d <- [0..6], (a==0&&b/=0&&c/=0&&d/=0)||(b==0&&a/=0&&c/=0&&d/=0)||(c==0&&a/=0&&b/=0&&d/=0)||(d==0&&a/=0&&b/=0&&c/=0)]

-- a == 0
a4 = [(a,b,c,d)| a <- [0..6], b <- [0..6], c <- [0..6], d <- [0..6], (a==0&&b/=0&&c/=0&&d/=0)||(b==0&&a/=0&&c/=0&&d/=0)||(c==0&&a/=0&&b/=0&&d/=0)||(d==0&&a/=0&&b/=0&&c/=0),a==0]

prg2b3a1 string = map (\x -> if x== 'Z' then 'Y' else if x=='Y' then 'Z' else x ) (map toUpper (map (\x -> if isNumber x then ' ' else x) (map (\x -> if x==' ' then '_' else x)  (filter (\x -> not (isPunctuation x)) string))))


-- Sudoku
undef x
    | x == -1 = [1..9]
    | otherwise = [x]

t 0 = 1
t n = t (n-1) + 3^n

sieb :: [Int] -> [Int]
sieb [] = []
sieb (l:ls) = l:sieb[x | x <- ls, mod x l /= 0]
take_primes n = take n $ sieb [2..]

as = sieb [2,3..]
ad n = sieb [2,3..n]

-- alle Leeten Videos

idchars = ['a'..'z'] ++ ['A'..'Z'] ++ ['_','-'] ++ ['0'..'9']
allids = [ (a0:a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:[]) | a0 <- idchars,a1 <- idchars,a2 <- idchars,a3 <- idchars,a4 <- idchars,a5 <- idchars,a6 <- idchars,a7 <- idchars,a8 <- idchars,a9 <- idchars,a10 <- idchars]
allleetids = [ (a0:a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:[]) | a0 <- idchars,a1 <- idchars,a2 <- idchars,a3 <- idchars,a4 <- idchars,a5 <- idchars,a6 <- idchars,a7 <- idchars,a8 <- idchars,a9 <- idchars,a10 <- idchars, (a0,a1,a2,a3) == ('1','3','3','7') || (a1,a2,a3,a4) == ('1','3','3','7') || (a2,a3,a4,a5) == ('1','3','3','7') || (a3,a4,a5,a6) == ('1','3','3','7') || (a4,a5,a6,a7) == ('1','3','3','7') || (a5,a6,a7,a8) == ('1','3','3','7') || (a6,a7,a8,a9) == ('1','3','3','7') || (a7,a8,a9,a10) == ('1','3','3','7')]



