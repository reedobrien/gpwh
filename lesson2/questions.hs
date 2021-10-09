-- You used Haskell’s if then else expression to write calcChange. In Haskell,
-- all if statements must include an else component. Given our three rules for
-- functions, why can’t you have an if statement all by itself?
-- There would be no return value if there was no else and the if did not match.
--

inc x = x + 1
double x = x * 2
square x = x * x

boopit n = if even n 
           then n-2
           else 3*n+1
