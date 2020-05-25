candidates :: Integral a => a -> [a]
candidates n = [vn, (vn-1) .. 2]
    where
        vn = floor . sqrt $ (fromIntegral $ n :: Double)


factors :: Integral a => a -> [a]
factors n = filter isPrimeFactor $ candidates n
    where
        isFactor k      = n `mod` k == 0
        isPrime k       = factors k == []
        isPrimeFactor k = isFactor k && isPrime k


answer :: Integral a => a
answer = head $ drop 10000 $ filter isPrime [2..]
    where
        isPrime n = factors n == []