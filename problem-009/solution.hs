list = [ [a,b,c] |  a <- [1..995],
                    b <- [(a+1)..996],
                    let c = 1000 - a - b,
                    a^2 + b^2 == c^2  ]

answer = product $ head list