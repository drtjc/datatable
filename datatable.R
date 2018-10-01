library(flights)
library(data.table)
library(purrr)

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
