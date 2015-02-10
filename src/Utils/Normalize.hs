module Utils.Normalize where

-- | Normalize a value to a specific range.
normalize :: Fractional a
             => (a, a)   -- ^ Lowest value, highest value pair.
             -> (a, a)   -- ^ Lower bound, upper bound pair.
             -> a        -- ^ Input value.
             -> a        -- ^ Normalized value in range [lower bound, upper bound].
normalize (lowest, highest) (bLowest, bHighest) n = norm * range
  where
    norm = (n - lowest) / (highest - lowest)
    range = (bHighest - bLowest)

-- | Normalize a list of values to a specific range.
normalizeList :: (Ord a, Fractional a)
                 => [a]     -- ^ The list to normalize.
                 -> (a, a)  -- ^ Lower bound, upper bound pair.
                 -> [a]     -- ^ Normalized list. Each item in range [lower bound, upper bound].
normalizeList [] _                   = []
normalizeList xs (bLowest, bHighest) = map (normalize (l, h) (bLowest, bHighest)) xs
  where
    (l, h) = (minimum xs, maximum xs)
