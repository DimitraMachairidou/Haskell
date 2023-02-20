--Dimitra Machairidou 4108
-----------------------------------------------------------------------------------------

-- ASKHSH 1
onedigit :: Integer -> Int

onedigit m = if 0<=m && m<=9 then
                 1
			 else 0

count :: Integer -> Int

count n = if (onedigit w) == 1 then
			if w==0||w==3||w==6||w==9 then
				1
			else 0
		  else if (w `mod` 10) == 0 || (w `mod` 10) ==3 || (w `mod` 10) ==6 || (w `mod` 10) ==9 then
				(count (w `div` 10)) +1
		  else (count (w `div` 10)) +0
		  where w=abs(n)

-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

kgcd :: Int->Int->Int->Int

kgcd m n k = if m==n then
				n
			 else if m==0 then
				n
			 else if n==0 then
				m
			 else if m<n && m `mod` k ==0 && n `mod` k ==0 then
				(kgcd (n-m) m k) `div` k
			 else if  m>n && m `mod` k ==0 && n `mod` k ==0 then
				(kgcd (m-n) n k) `div` k
			 else if  m<n && (m `mod` k /=0 || n `mod` k /=0) then
				if ((kgcd (n-m) m k) `div` k+1) >=2 then
					(kgcd (n-m) m k) `div` k+1
				else 0
			 else if m>n && (m `mod` k /=0 || n `mod` k /=0) then
				if ((kgcd (m-n) n k) `div` k+1) >=2 then
					(kgcd (m-n) n k) `div` k+1
				else 0
			 else 0

-----------------------------------------------------------------------------------------
     
-- ASKHSH 3

minas :: Int -> Int

minas a = if a==1 then
		      0
		  else if a==2 then
		      31
		  else if a==3 then 
			  59
		  else if a==4 then
		      90
		  else if a==5 then 
		      120
		  else if a==6 then
		      151
		  else if a==7 then
		      181
		  else if a==8 then
		      212
		  else if a==9 then
			  243
		  else if a==10 then
			  273
		  else if a==11 then
		      304
		  else if a==12 then
		      334
		  else error "error"

seconds :: (Int,Int)->(Int,Int,Int)->Int

seconds (d,mm) (h,m,s) = if 1<=d && d<=31 then
							if 1<=mm && mm<=12 then
								if 0<=h && h<=23 then
									if 0<=m && m<=59 then
										if 0<=s && s<=59 then
										    if (mm==1 && d>31)||(mm==2 && d>28)||(mm==3 && d>31)||(mm==4 && d>30)||(mm==5 && d>31)||(mm==6 && d>30)||(mm==7 && d>31)||(mm==8 && d>31)||(mm==9 && d>30)||(mm==10 && d>31)||(mm==11 && d>30)||(mm==12 && d>31) then
												-1
											else (((minas mm)+(d-1))*86400)+(h*3600)+(m*60)+s
										else -1
									else -1
								else -1
							else -1
						else -1
								

-----------------------------------------------------------------------------------------
     
-- ASKHSH 4

--sumfab :: (Int->Int->Int->Int)->Int->Int->Int

--sumfab f a b = if a>=b then
                   --0
			   --else sumfab f a (b-1) + f a b





-----------------------------------------------------------------------------------------
     



