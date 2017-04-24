--0nLengths = [4,3,3,5,4,4,3,5,5,4]
--1nLengths = [3,6,6,8,8,7,7,9,8,8]
one = 3
two = 3
three = 5
four = 4
five = 4
six = 3
seven = 5
eight = 5
nine = 4
ten = 3
eleven = 6
twelve = 6
thirteen = 8
fourteen = 8
fifteen = 7
sixteen = 7
seventeen = 9
eighteen = 8
nineteen = 8
twenty = 6
thirty = 6
forty = 5
fifty = 5
sixty = 5
seventy = 7
eighty = 6
ninety = 6
hundred = 7
thousand = 8
ands = 3

d1 = (one + two + three + four + five + six + seven + eight + nine)
d2 = d1 * 9 + 
      ten + eleven + twelve + thirteen + fourteen + fifteen + sixteen + seventeen + eighteen + nineteen +
        (twenty + thirty + forty + fifty + sixty + seventy + eighty + ninety)*10
d3 = d2 * 10 + (d1 + hundred * 9)*100 + ands * (999-99-9)
d4 = d3 + one + thousand

main = print d4
