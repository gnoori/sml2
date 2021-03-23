# sml2
ProblemThe goal of this lab is to implement quicksort in SML. The idea of quicksort was to pick a pivotelementp, partition the input in elements smaller thanpand elements larger thanp, and thenrecursively sort both parts.As pivot element, we will pick the median of the first, the middle, and the last element in a given list.To implement quicksort, implement the following functions.–A functionlast(lst)of type'a list ->'awhich returns the last element of a given list.–A functionmiddle(lst)of type'a list ->'awhich returns the middle element of a givenlist. Hint: One way implement this functions is to create a helper function which recursively callitself with one list reduced by one item and the other reduced by two item until the smaller list isempty.–A functionmedian(a, b, c)of typeint * int * int -> intwhich returns the medianof the given three elements.–A functionpartition(lst, p)of typeint list * int -> int list * int listwhichpartitions the given list into a list containing all elements smaller than or equal topand a listcontaining all elements larger thanp.–A functionquicksort(lst)of typeint list -> int listwhich sorts a given list usingquicksort and a pivot element as defined above.Do not use any pre-implemented functions of the formList.xyz(...).
