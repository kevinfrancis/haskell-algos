-- Find out all unique combinations of coin denominations that add up to same total
-- @author: Kevin Francis

denominations = [25, 10, 5, 1]

-- Find all combinations such that each has items not exceeding the thresh.
coin_comb :: Int -> Int -> [[Int]]
coin_comb thresh n_cents
  | n_cents <= 0 = [[]]
  | otherwise    = concat $ map combK (denominations)
  where combK k
          | n_cents < k = []
          | k <= thresh = map (k:) $ coin_comb k (n_cents-k)
          | otherwise  = []

-- For user
coin_combinations :: Int -> [[Int]]
coin_combinations = coin_comb (maximum denominations)
