Title: Peeking into Haskell
Tags: haskell
Date: 2014-02-24 20:32:18
Summary: Last semester I dipped into functional programming. Today I want to show you the part which stood out the most for me: Readability.

##Divide And Conquer
Last semester I dipped into functional programming. Today I want to show you the part which stood out the most for me: **Readability**. Even without extensive programming knowledge, you can sort of understand what is going on, since you don't need to worry about variables/memory and all that other nasty stuff that makes programming complicated at the beginning.

This is part of an assignment we got to show the adaptability of [divide and conquer](http://en.wikipedia.org/wiki/Divide_and_conquer_algorithm) (which is used for example in quicksort, which we will examine later). To do so we were given the following function:

    :::Haskell:
    divAndConquer :: (p -> Bool) -> (p -> s) -> (p -> [p]) -> (p -> [s] -> s) -> p -> s
    divAndConquer ind solve divide combine initProblem 
            = dac initProblem
                    where dac problem 
                            | ind problem = solve problem
                            | otherwise   = combine problem (map dac (divide problem))
                            
The first line may look intimidating if you have never worked with functional programming before, but let me take your fear because it is actually fairly simple to read. 

*   **divAndConquer**
    
    This is the name of the function, no surprises here

*   **(p -> Bool)**
    
    This specifies the first argument we pass to the function, which happens to be another function taking a variable of the type **p** (note that this is not a real type, it is only used to specify that later occurances of **p** must all have the same type) and returning a **Bool**. In the second line we see we call this function *ind* which is maybe a bad name, as later this function will be called on a **p**roblem to determine if it is solvable.

*   **(p -> s)**
    
    So for the second argument we got another function, taking a **p**roblem and returning a **s**olution. We fittingly name this one *solve*.

*   **(p -> [p])**
    
    Now we go for the divide part of the algorithm. Again a function that takes p and something new for the return type, a list of p.

*   **(p -> [s] -> s)**
    
    This is our combine function to use on the solved subproblems.

*   **p**
    
    The problem we are going to solve

*   **s**
    
    The return type of our divAndConquer function

##Quicksort

In the next step we are going to use this abstract function to implement [quicksort](http://en.wikipedia.org/wiki/Quicksort). If you never heard of this sorting algorithm before, don't worry, we will have a look at it right now.

    :::Haskell:
    quickSort :: Ord a => [a] -> [a]
    quickSort = divAndConquer indq solveq divideq combineq
    
If we break this function down, you see **Ord a** which requires that **a** derives from the Ord class. This enables the use of comperatores < and > and all their [forms](http://hackage.haskell.org/package/base-4.2.0.1/docs/Data-Ord.html) on elements of **a**.

The rest should be fairly obvious. Our input is a list of **a**, as is our output. In the second line we see that we call divAndConquer with subfunctions we have to define now.

    :::Haskell:
    indq :: [a] -> Bool
    indq problem
            | length(problem) <= 1 = True   -- This is called a guard. It is similar to if's in imperative
            | otherwise            = False  -- languages.

As mentioned above this function checks if we can solve a problem or if we have to divide it further. In our case this is really simple since an unordered list is always ordered when it's length is smaller or equal to 1. Else we continue dividing.

    :::Haskell:
    solveq :: [a] -> [a]
    solveq p = p
    

Here we 'solve' the list if its length is 1. This is not really needed in quickSort, but again this is just to show that general idea of divAndConquer.

    :::Haskell:
    divideq :: Ord a => [a] -> [[a]]
    divideq []      = [[]]              -- If used on an empty List we return a list of empty lists
    divideq [x]     = [[x]]             -- If used on an List with only one element we return a list 
                                        -- of a list with the single element
    divideq (x:xs)  = [smaller,[x],bigger]      --(x:xs) <- this splits up a list, 
                                                --x beeing the first element, xs the rest
                            where smaller =  [y | y <- xs, y <= x]
                                  bigger  =  [y | y <- xs, y > x]

Now for the magic: If a list is not solvable we split it up in three parts. A list of elements which is smaller than the first element of the list, the element we use to split up the list itself and a list of bigger elements.

To construct the list of smaller and bigger elements we use a thing called [list comprehension](http://en.wikipedia.org/wiki/List_comprehension). Without going into too much detail, it is similar to the [set-builder annotation](http://en.wikipedia.org/wiki/Set-builder_notation) used in math. You can read it like this: we build a list of **y**. But what is **y**? **y** is an element of **xs** with the added condition that **y** is smaller and equal or bigger then **x**.

    :::Haskell:
    combineq :: [a] -> [[a]] -> [a]
    combineq problem solutions = concat solutions

Here we take a all divided and solved solutions and glue them together using concat.

And that is all for Quicksort. Of course you can implement it simpler, but then the whole demonstration of Divide And Conquer and abstraction is missing. 

##Binominal Coefficients

But now for the interesting part, we can use divAndConquer for other things as well. For example calculating [Binomial coefficients](http://en.wikipedia.org/wiki/Binomial_coefficient).[^1] 

Compare the subfunctions to the ones used in the Quicksort example. The function *indb*, as *indq* above, decides if we can solve the problem with *solveb*. If we can't, we have to call *divideb* on our problem to split it up, try to solve it again (and so on...) and *combineb* the solutions together.

    :::Haskell:
    indb :: (Integer,Integer) -> Bool
    indb (n,k) 
            | n == k     = True
            | k == 0     = True
            | k == 1     = True
            | k == 2     = True
            | (k+1) == n = True
            | k > n      = True
            | otherwise  = False

    solveb :: (Integer,Integer) -> Integer
    solveb (n,k) 
            | n == k     = 1
            | k == 0     = 1
            | k == 1     = n
            | (k+1) == n = n
            | k == 2     = n*(n-1) `div` 2
            | k > n      = 0
            | otherwise  = error "not solvable"
     

    divideb :: (Integer,Integer) -> [(Integer,Integer)]
    divideb (n,k) = [(n-1,k-1),(n-1,k)]        

    combineb :: (Integer,Integer) -> [Integer] -> Integer
    combineb _ solutions = sum solutions

    binom :: (Integer,Integer) -> Integer
    binom = divAndConquer indb solveb divideb combineb


##Conclusion

As you can see, even though the goal of both algorithms is completly different they use the same structure and overall idea to solve their problems. And thanks to functional programming this abstract way of solving problems is easier to see and use than in imperative programming languages.

If you want to learn more about functional programming, check out [learnyouahaskell.com](http://learnyouahaskell.com/) which is a great ressource to start with haskell.





[^1]:Please note that this implementation lacks a proper handling of symmetry (n,k) == (n,n-k). So it is by far not efficient to run it like this.

