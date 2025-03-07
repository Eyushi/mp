let twice f x = f (f x)
let compose f g x = f (g x)



let rec sumto n =
  if n = 0 then 0
  else n + sumto (n - 1)
  
  
  
  
  
  