
calcChange owed given = if change > 0
                        then change
                        else 0
    where change = given - owed

main = do
    print "What is owed?"
    owed <- getLine
    print "How much given?"
    given <- getLine
    show calcChange owed given
