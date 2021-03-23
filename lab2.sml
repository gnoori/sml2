(*Guzel Noori
Lab 2 - Quick Sort (CS 362)
1/24/2021*)

(*of type 'a list -> 'a 
which returns last element of a given list*)
fun last nil = last(nil)
 |  last [l] = l
 |  last (l::ls) = 
    let
      val x = last(ls)
    in
      x
    end;
    
(*of type 'a list -> 'a 
which return the middle element of a given list*)
fun middle lst = 
    (*helper function that call itself recursively 
    that removes elements from both sides until the smaller list is empty*)
    let
        fun mid_help(m, n) = 
            if null (tl m) orelse null (tl (tl m)) then hd n
            else mid_help(tl (tl m), tl n)
    in
        mid_help(lst, lst)
    end;

(*of type int*int*int -> int 
which returns the median of the given three elements*)
fun median (a, b, c) = 
    let 
        fun findLeast(x, y) = 
            if x<y then x
            else if x>y then y
            else x
        fun findGreatest(x, y) = 
            if y<x then x
            else if y>x then y
            else x
        (*among the three elements, find the gratest number and least number to perform the median calculatoin*)
        val least = findLeast(a, findLeast(b, c))
        val greatest = findGreatest(a, findGreatest(b, c))
    in
        (*calculation to find the median*)
        (a+b+c)-(greatest+least)
    end;

(*of type int list*int -> int list*int list 
which partitions the given list containing all elements 
smaller than or equal to pivot 
and a list containing all elements larger than pivot*)
fun partition ([], pivot) = ([], [])
 |  partition(m::mn, pivot) = 
        let
            val (least, greatest) = partition(mn, pivot)
        in
            (*if greater than pivot, place the element on the right side*)
            if m>pivot then (least, m::greatest)
            (*if less than or equl to pivot, place the element on the left side*)
            (*I once I put the equal sign next to < sign, my program is failing.*)
            else if m<pivot then (m::least, greatest)
            (*else (m::least, m::greatest)*)
            else (least, greatest)
        end;

(*int list -> int list
sorts the list by picking the pivot*)
fun quickSort [] = []
 |  quickSort [l] = [l]
 |  quickSort (m::mn) = 
        let 
            val pvt = median(m, middle(m::mn), last(m::mn))
            val (least, greatest) = partition(m::mn, pvt)
        in
            quickSort(least)@[pvt]@quickSort(greatest)
        end;
