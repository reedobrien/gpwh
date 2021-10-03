
toPart recipient = "Dear amazing" ++ recipient ++ ",\n"
bodyPart bookTitle = "Thanks for reading " ++ bookTitle ++ ".\n"
fromPart author = "Thanks much,\n"++author

createEmail recipient bookTitle author = toPart recipient ++
                                          bodyPart bookTitle ++
                                          fromPart author

main = do
    print "Who is the email for?"
    recipient <- getLine
    print "What is the Subject?"
    subj <- getLine
    print "Who is the author?"
    author <- getLine
    print (createEmail recipient subj author)

