f list n = f' list [] 0 where
  f' [] idxs _ = idxs
  f' (num:nums) idxs idx = 
    if n == num 
       then f' nums (idx:idxs) (idx+1)
       else f' nums idxs (idx+1)