-- listing 10.1
cup flOz = \message -> message flOz


-- listing 10.3
getOz cup = cup (\flOz -> flOz)

-- listing 10.4
drink aCup amount = if ozDiff >=0
                    then cup ozDiff
                    else cup 0
    where flOz   = getOz aCup
          ozDiff = flOz - amount

-- listing 10.6
isEmpty aCup = getOz aCup == 0

-- listing 10.7
-- coffee = cup 20
-- afterManySips = foldl drink coffee [1,2,3,4,3,2,1]
-- -> 4



