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