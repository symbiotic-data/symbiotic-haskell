module Data.Symbiotic.Primitives.Integral.Integer8
  ( Integer8, getInteger8 -- , makeInteger8
  ) where

-- import GHC.Integer.Type ()


newtype Integer8 = Integer8 {getInteger8 :: Integer}

-- instance Serialize Integer where
--   put n | n >= lo && n <= hi = do
--     putWord8 0
--     put (fromIntegral n :: Int32)  -- fast path
--     where
--       lo :: Integer
--       lo = fromIntegral (minBound :: Int32)
--       hi :: Integer
--       hi = fromIntegral (maxBound :: Int32)

--   put n = do
--     putWord8 1
--     put sign
--     let len = ((nrBits (abs n) + 7) `div` 8) -- in bytes
--     putWord64be (fromIntegral len)     -- NOTE point of interest
--     mapM_ put (unroll (abs n))         -- unroll the bytes
--     where
--       sign :: Word8
--       sign = fromIntegral (signum n)

--   get = do
--     tag <- get :: Get Word8
--     case tag of
--       0 -> liftM fromIntegral (get :: Get Int32)
--       _ -> do
--         sign  <- get
--         bytes <- get                   -- NOTE point of interest
--         let v = roll bytes
--         return $! if sign == (1 :: Word8) then v else - v

-- makeInteger8 :: Integer -> Maybe Integer8
-- makeInteger8 x =
