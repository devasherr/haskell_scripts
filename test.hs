doubleMe x = x * 2
doubleSum x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else x * 2
doubleSmallNumber' x = (if x > 100 then x else doubleSmallNumber x) + 1