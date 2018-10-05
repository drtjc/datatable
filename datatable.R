library(flights)
library(purrr)
library(data.table)


flights <- fread("flights14.csv")
flights
class(flights)

DT = data.table(
  ID = c("b","b","b","a","a","c"),
  a = 1:6,
  b = 7:12,
  c = 13:18
)
DT

ans <- flights[origin == "JFK" & month == 6L]
head(ans)

ans <- flights[1:2]
ans

ans <- flights[order(origin, -dest)]
head(ans)


ans <- flights[, arr_delay]
head(ans)

ans <- flights[, .(arr_delay)]
head(ans)

ans <- flights[, .(arr_delay, dep_delay)]
head(ans)


ans <- flights[, c("arr_delay", "dep_delay")]
head(ans)



ans <- flights[, .(delay_arr = arr_delay, delay_dep = dep_delay)]
head(ans)


# How many trips have had total delay < 0?
ans <- flights[, sum( (arr_delay + dep_delay) < 0 )]
ans
typeof(ans)

ans <- flights[, .(delay = sum( (arr_delay + dep_delay) < 0 ))]
ans
typeof(ans)


# Calculate the average arrival and departure delay for all flights with “JFK” as the origin airport in the month of June.
ans <- flights[origin == "JFK" & month == 6L,
               .(m_arr = mean(arr_delay), m_dep = mean(dep_delay))]
ans
typeof(ans)



# How many trips have been made in 2014 from “JFK” airport in the month of June?
ans <- flights[origin == "JFK" & month == 6L, length(dest)]
ans


ans <- flights[origin == "JFK" & month == 6L, .N]
ans



select_cols = c("arr_delay", "dep_delay")
flights[ , ..select_cols]
flights[ , select_cols, with = FALSE]


ans <- flights[, !c("arr_delay", "dep_delay")]
ans
ans <- flights[, -c("arr_delay", "dep_delay")]
ans

ans <- flights[, year:day]
ans

# How can we get the number of trips corresponding to each origin airport?
ans <- flights[, .(.N), by = .(origin)]
ans

ans <- flights[, .N, by = origin]
ans

ans <- flights[, .(.N), by = "origin"]
ans


# How can we calculate the number of trips for each origin airport for carrier code "AA"?
# The unique carrier code "AA" corresponds to American Airlines Inc.

ans <- flights[carrier == "AA", .N, by = origin]
ans


# How can we get the total number of trips for each origin, dest pair for carrier code "AA"?
ans <- flights[carrier == "AA", .N, by = .(origin, dest)]
head(ans)


# How can we get the average arrival and departure delay for each orig,dest pair for each month for carrier code "AA"?
ans <- flights[carrier == "AA",
                 .(mean(arr_delay), mean(dep_delay)),
                 by = .(origin, dest, month)]
ans


ans <- flights[carrier == "AA",
               .(mean(arr_delay), mean(dep_delay)),
               keyby = .(origin, dest, month)]
ans



# How can we order ans using the columns origin in ascending order, and dest in descending order?
ans <- flights[carrier == "AA", .N, by = .(origin, dest)]
ans <- ans[order(origin, -dest)]
head(ans)


ans <- flights[carrier == "AA", .N, by = .(origin, dest)][order(origin, -dest)]
head(ans)


# Can by accept expressions as well or does it just take columns?
# Yes it does. As an example, if we would like to find out how many flights started 
# late but arrived early (or on time), started and arrived late etc…

ans <- flights[, .N, .(dep_delay>0, arr_delay>0)]
ans


ans <- flights[, .N, .(dep_delayed = dep_delay>0, arr_delayed = arr_delay>0)]
ans

DT[, print(.SD), by = ID]
DT[, lapply(.SD, mean), by = ID]

DT[, map(.SD, mean), by = ID]
DT[, .SD %>% map(mean), by = ID]


flights[carrier == "AA",                       ## Only on trips with carrier "AA"
        lapply(.SD, mean),                     ## compute the mean
        by = .(origin, dest, month),           ## for every 'origin,dest,month'
        .SDcols = c("arr_delay", "dep_delay")]

# How can we return the first two rows for each month?
ans <- flights[, head(.SD, 2), by = month]
ans


# How can we concatenate columns a and b for each group in ID?
DT
DT[, .(val = c(a,b)), by = ID]



# What if we would like to have all the values of column a and b concatenated, but returned as a list column?
DT[, .(val = list(c(a,b))), by = ID]



####################################################################################
# REFERENCE SEMANTICS

DF = data.frame(ID = c("b","b","b","a","a","c"), a = 1:6, b = 7:12, c = 13:18)
DF

DF$c <- 18:13               # (1) -- replace entire column
DF
# or
DF$c[DF$ID == "b"] <- 15:13 # (2) -- subassign in column 'c'
DF

#(1) shallow copy in R as entire column
#(2) still deep copy in R


# The := operator
#It can be used in j in two ways:
  
#  The LHS := RHS form

DT[, c("colA", "colB", ...) := list(valA, valB, ...)]

# when you have only one column to assign to you
# can drop the quotes and list(), for convenience
DT[, colA := valA]

# The functional form

DT[, `:=`(colA = valA, # valA is assigned to colA
          colB = valB, # valB is assigned to colB
          ...
)]


# Add columns by reference
# – How can we add columns speed and total delay of each flight to flights data.table?
head(flights)
flights[, `:=`(speed = distance / (air_time/60), # speed in mph (mi/h)
                 delay = arr_delay + dep_delay)]   # delay in minutes
head(flights)


flights <- fread("flights14.csv")
head(flights)

flights[, c("speed", "delay") := list(distance / (air_time/60), arr_delay + dep_delay)]


flights[, sort(unique(hour))]
flights[hour == 24L, hour := 0L]
flights[, sort(unique(hour))]
flights[hour == 24L, hour := 0L][]

# Delete column by reference
# – Remove delay column
flights[, c("delay") := NULL]
head(flights)

## or using the functional form
flights[, `:=`(delay = NULL)]
head(flights)

flights[, delay := NULL]

#How can we add a new column which contains for each orig,dest pair the maximum speed?
flights[, max_speed := max(speed), by = .(origin, dest)]
head(flights)


# How can we add two more columns computing max() of dep_delay and arr_delay for each month, using .SD?
in_cols  = c("dep_delay", "arr_delay")
out_cols = c("max_dep_delay", "max_arr_delay")
flights[, c(out_cols) := lapply(.SD, max), by = month, .SDcols = in_cols]
head(flights)



####################################################################################
# FAQ
flights <- fread("flights14.csv")
flights[2, 5]
flights[, 5]

str(flights[, 5, drop = FALSE])
str(flights[[5]])
str(flights[5])
str(flights[, 5])
str(flights[, (5)])
str(flights[, .(5)])
str(flights$arr_delay)
head(flights)

rm(x)
x<- "test"
flights[, .(x)]

arr_delay <- "test"
flights[, .(arr_delay)]

mycol <- "arr_delay"
flights[, mycol] #error
flights[, ..mycol]
flights[, mycol, with=FALSE]
flights[[mycol]] # atomic vector






View(options())

getOption(datatable.WhenJisSymbolThenCallingScope)

flights[, "arr_delay"]
flights[, c("arr_delay")]
flights[, .("arr_delay")]
flights[, ("arr_delay")]


flights[, c("dep_delay", "arr_delay")]

flights[, c(dep_delay, arr_delay)]
l1 <- c(1,2)
l2 <- c(3,4)
c(l1,l2)
list(l1,l2)



flights[, c(3,4,5)]


flights[, 3:5]
flights[, .(3:5)]
flights[3:5]

str(flights[, air_time])
str(flights[, .(air_time)])
flights[, air_time:hour]

flights[, .(air_time, distance, hour)]





#################################################################################################
# KEYS
# How can we set the column origin as key in the data.table flights?
flights <- fread("flights14.csv")
flights

setkey(flights, origin)
head(flights)

# or
setkeyv(flights, "origin")

# Use the key column origin to subset all rows where the origin airport matches “JFK”
flights[.("JFK")]
flights[J("JFK")]  
flights[list("JFK")]

flights["JFK"] 

flights[.("JFK", "EWR")]

flights[c("JFK", "LGA")]     
flights[.(c("JFK", "LGA"))]


# How can we get the column(s) a data.table is keyed by?
# Using the function key().

key(flights)

# How can I set keys on both origin and dest columns?
setkey(flights, origin, dest)
head(flights)
setkeyv(flights, c("origin", "dest"))

key(flights)

# Subset all rows using key columns where first key column origin matches “JFK” and second key column dest matches “MIA”
flights[.("JFK", "MIA")]
flights[.("JFK", "MIA")]

flights[.(c("JFK", "LGA"), "MIA")]



# Subset all rows where just the first key column origin matches “JFK”

flights[.("JFK")] 
## or in this case simply 
flights["JFK"]


# Subset all rows where just the second key column dest matches “MIA”
flights[.(unique(origin), "MIA")]



# Return arr_delay column as a data.table corresponding to origin = "LGA" and dest = "TPA".
key(flights)
# [1] "origin" "dest"
flights[.("LGA", "TPA"), .(arr_delay)]


#On the result obtained above, use chaining to order the column in decreasing order.
flights[.("LGA", "TPA"), .(arr_delay)][order(-arr_delay)]

# Find the maximum arrival delay corresponding to origin = "LGA" and dest = "TPA".
flights[.("LGA", "TPA"), max(arr_delay)]


flights[, sort(unique(hour))]

setkey(flights, hour)
key(flights)
# [1] "hour"

flights[J(24)]


flights[.(24), hour := 0L]
key(flights)
# NULL

flights[, sort(unique(hour))]



setkey(flights, origin, dest)
key(flights)
# Get the maximum departure delay for each month corresponding to origin = "JFK". Order the result by month
ans <- flights["JFK", max(dep_delay), keyby = month]
head(ans)

attributes(ans)



################################################################################
# SECONDARY INDEXING AND AUTO INDEXING
flights <- fread("flights14.csv")
flights

setindex(flights, origin)
head(flights)

attributes(flights)

indices(flights)
setindex(flights, origin, dest)
indices(flights)


###########################################################################################
# Efficient reshaping using data.tables
flights <- fread("flights14.csv")
flights

s1 <- "family_id age_mother dob_child1 dob_child2 dob_child3
1         30 1998-11-26 2000-01-29         NA
2         27 1996-06-22         NA         NA
3         26 2002-07-11 2004-04-05 2007-09-02
4         32 2004-10-10 2009-08-27 2012-07-21
5         29 2000-12-05 2005-02-28         NA"

DT <- fread(s1)
DT


DT.m1 = melt(DT, id.vars = c("family_id", "age_mother"),
             measure.vars = c("dob_child1", "dob_child2", "dob_child3"), variable.name = "child", value.name = "dob")
DT.m1

dcast(DT.m1, family_id + age_mother ~ child, value.var = "dob")


dcast(DT.m1, family_id ~., value.var = "dob")


s2 <- "family_id age_mother dob_child1 dob_child2 dob_child3 gender_child1 gender_child2 gender_child3
1         30 1998-11-26 2000-01-29         NA             1             2            NA
2         27 1996-06-22         NA         NA             2            NA            NA
3         26 2002-07-11 2004-04-05 2007-09-02             2             2             1
4         32 2004-10-10 2009-08-27 2012-07-21             1             1             1
5         29 2000-12-05 2005-02-28         NA             2             1            NA"
DT <- fread(s2)
DT


DT.m1 = melt(DT, id = c("family_id", "age_mother"))
DT.m1
DT.m1[, c("variable", "child") := tstrsplit(variable, "_", fixed = TRUE)]
DT.c1 = dcast(DT.m1, family_id + age_mother + child ~ variable, value.var = "value")
DT.c1

colA = paste("dob_child", 1:3, sep = "")
colA


colA = paste("dob_child", 1:3, sep = "")
colB = paste("gender_child", 1:3, sep = "")
DT.m2 = melt(DT, id.vars = c("family_id", "age_mother"), measure.vars = list(colA, colB), value.name = c("dob", "gender"))
DT.m2



##########################################################################################
# joins



indices(flights)

tables()
tables(index = TRUE)
tt <- tables(silent = TRUE)
tt




############################################################################################33
## examples
example(data.table)
DF
DT
tables(index = TRUE)

DF = data.frame(x=rep(c("b","a","c"),each=3), y=c(1,3,6), v=1:9)
DF

DT = data.table(x=rep(c("b","a","c"),each=3), y=c(1,3,6), v=1:9)
DT

identical(dim(DT), dim(DF))    # TRUE
dt.tblR> identical(DF$a, DT$a)          # TRUE
is.list(DF)                    # TRUE
is.list(DT)                    # TRUE
is.data.frame(DT)              # TRUE
is.data.table(DF)             #FALSE

tables()

# basic row subset operations
DT[2]                                       # 2nd row
DT[2:3]                                     # 2nd and 3rd row
w=2:3; DT[w]  # same
DT[3:2]   # 3rd and 2nd row

DT[order(x)]                                # no need for DT$ prefix on column x
DT[order(x), ]                              # same; the ',' is optional

DT[y>2]                                     # all rows where DT$y > 2
DT[y>2 & v>5]                               # compound logical expressions
DT[!2:4]                                    # all rows other than 2:4
DT[-(2:4)]                                  # same

# select|compute columns data.table way
DT[, v]                        # v column (as vector)
DT[, list(v)]                  # v column (as data.table)
DT[, .(v)]                     # same as above, .() is a shorthand alias to list()

DT[, sum(v)]                   # sum of column v, returned as vector
DT[, .(sum(v))]                # same, but return data.table (column autonamed V1)
DT[, .(sv=sum(v))]             # same, but column named "sv"
DT[, .(v, v*2)]                # return two column data.table, v and v*2


# subset rows and select|compute data.table way
DT[2:3, sum(v)]                # sum(v) over rows 2 and 3, return vector
DT[2:3, .(sum(v))]             # same, but return data.table with column V1
DT[2:3, .(sv=sum(v))]          # same, but return data.table with column sv
DT[2:5, cat(v, "\n")]          # just for j's side effect

# select columns the data.frame way
DT[, 2]                        # 2nd column, returns a data.table always

colNum = 2                     # to refer vars in `j` from the outside of data use `..` prefix
DT[, ..colNum]                 # same, equivalent to DT[, .SD, .SDcols=colNum]
DT[["v"]]                      # same as DT[, v] but much faster
DT[, c(2,3)] # cols 2 and 3
DT[, .(2,3)]  # evaluates to data table containing integer 2 and 3
DT[, .("y","v")] # evaluates to data table containing characters "y" and "v" 
DT[, .(y,v)]
DT[, c("y", "v")]


# grouping operations - j and by
DT[, sum(v), by=x]             # ad hoc by, order of groups preserved in result
DT[, sum(v), keyby=x]          # same, but order the result on by cols
DT[, sum(v), by=x][order(x)]   # same but by chaining expressions together

# fast ad hoc row subsets (subsets as joins)
DT["a", on="x"]                # same as x == "a" but uses binary search (fast)
DT["a", on=.(x)]               # same, for convenience, no need to quote every column
DT[.("a"), on="x"]             # same
DT[x=="a"]                     # same, single "==" internally optimised to use binary search (fast)

DT[x!="b" | y!=3]              # not yet optimized, currently vector scan subset
DT[.("b", 3), on=c("x", "y")]  # join on columns x,y of DT; uses binary search (fast)
DT[.("b", 3), on=.(x, y)]      # same, but using on=.()

DT[.("b", 1:2), on=c("x", "y")]             # no match returns NA
#   x y  v
#1: b 1  1
#2: b 2 NA

DT[.("b", 1:2), on=.(x, y), nomatch=0]      # no match row is not returned
#   x y v
#1: b 1 1

DT[x=="b" & (y==1 | y==2)]   


DT[.("b", 1:2), on=c("x", "y"), roll=Inf]   # locf, nomatch row gets rolled by previous row
#   x y v
#1: b 1 1
#2: b 2 1

DT[.("b", c(1,9)), on=.(x, y), roll=-Inf]      # nocb, nomatch row gets rolled by next row
#   x y v
#1: b 1 1
#2: b 2 2

DT["b", sum(v*y), on="x"]                   # on rows where DT$x=="b", calculate sum(v*y)


# all together now
DT[x!="a", sum(v), by=x]                    # get sum(v) by "x" for each i != "a"

DT[!"a", sum(v), by=.EACHI, on="x"] # same, but using subsets-as-joins




# joins as subsets
X = data.table(x=c("c","b"), v=8:7, foo=c(4,2))
X
DT

e <- 2:3

DT[e]

DT[X, on="x"]                               # right join
X[DT, on="x"]                               # left join
DT[X, on="x", nomatch=0]                    # inner join
X[DT, on="x", nomatch=0]                    # inner join
DT[!X, on="x"]  # not join
DT[X, on=c(y="v")]                          # join DT$y to X$v
DT[X, on="y==v"]                            # same
DT[X, on=.(y<=foo)]                         # non-equi join
DT[X, on=.(y>=foo)] # non-equi join

