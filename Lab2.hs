--Dimitra Machairidou 4108
-----------------------------------------------------------------------------------------

-- ASKHSH 1

thesi :: Int->[Int]->Int

thesi a l = if l==[] then
				0
			else if a==(head l) then
				1
			else (thesi a (tail l))+1
			
nearest :: [Int]->Int->Int

nearest s n 
		| null s
			=0
	    | otherwise
			=thesi (min (abs(w)) (nearest (tail s) n)) s
			where w=n-(head s)
			-- thelo tin thesi tou min


-----------------------------------------------------------------------------------------
     
-- ASKHSH 2
			 
upolista :: Int->[Int]->[Int]
upolista b u = if b==0 then
			       []
			   else if b>(length u) then
			       u
			   else (head u) : (upolista (b-1) (tail u))

sumIntList :: [Int]->Int
sumIntList (h:t) = h + sumIntList t
sumIntList [] = 0

smooth :: [Int]->Int->[Int]

smooth s k = if (length s)<k then
				[]
			 else if k==1 then
				s
			 else if s==[] || k==0 then
				error "empty or no input"
			 else (w `div` k) : smooth (tail s) k
			 where w=(sumIntList (upolista k s))

-----------------------------------------------------------------------------------------
     
-- ASKHSH 3

--swap :: String->String

--swap s = if s==[] then
			 --error "empty"		 
		-- else if x:y:swap

-----------------------------------------------------------------------------------------
     
-- ASKHSH 4

--upolistas :: Int->[Int]->[Int]
--upolistas b u = if b==0 then
			       --[]
			   --else if b>(length u) then
			       --u
			   --else (head u) : (upolistas (b-1) (tail u))

--mapi :: [u]->(u->Int->v)->[v]

--mapi [] f = [] 

--mapi s f = mapi (upolistas ((length s)-1) s ) f ++ s f 







