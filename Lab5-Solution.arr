use context dcic2024

include csv
include data-source


# 1) Load the table

flights53 = load-table:
  rownames :: Number,
  dep_time :: Number,
  sched_dep_time :: Number,
  dep_delay :: Number,
  arr_time :: Number,
  sched_arr_time :: Number,
  arr_delay :: Number,
  carrier :: String,
  flight :: Number,
  tailnum :: String,
  origin :: String,
  dest :: String,
  air_time :: Number,
  distance :: Number,
  hour :: Number,
  minute :: Number,
  time_hour :: String
  source: csv-table-file("flights_sample53.csv",default-options)
  sanitize rownames using num-sanitizer
  sanitize dep_time using num-sanitizer
  sanitize sched_dep_time using num-sanitizer
  sanitize dep_delay using num-sanitizer
  sanitize arr_time using num-sanitizer
  sanitize sched_arr_time using num-sanitizer
  sanitize arr_delay using num-sanitizer
  sanitize flight using num-sanitizer
  sanitize air_time using num-sanitizer
  sanitize distance using num-sanitizer
  sanitize hour using num-sanitizer
  sanitize minute using num-sanitizer
end

# flights53

# -----------------------
#  helper functions
# -----------------------

# Trim spaces at both ends
fun trim(s :: String) -> String:
  doc: "Remove spaces from the given string."
  n = string-length(s)
  if n == 0:
    ""
  else:
    string-replace(s, " ", "")
  end
end

# trim(flights53.row-n(13)["carrier"])


# Normalise departure times. For numeric inputs like 517 -> "05:17"
fun hhmmToClock(n :: Number) -> String block:
  doc: "Convert a numeric time value (e.g., 517) into a zero-padded clock string (e.g., '05:17')"
  h = num-floor(n / 100)
  m = n - (h * 100)

  var hh = ""
  if h < 10: 
    hh := string-append("0", to-string(h)) 
  else: hh := to-string(h) 
  end
  
  var mm = ""
  if m < 10: 
    mm := string-append("0", to-string(m)) 
  else: 
    mm := to-string(m) 
  end

  string-append(hh, string-append(":", mm))
end


# Map standardized carrier code -> airline name 
fun carrierToAirline(code :: String) -> String:
  doc: "Convert a carrier code (e.g., 'UA', 'AA') to its full airline name"
  c = string-to-upper(trim(code))
  ask:
    | c == "UA" then: "United Airlines"
    | c == "AA" then: "American Airlines"
    | c == "B6" then: "JetBlue"
    | c == "DL" then: "Delta Air Lines"
    | c == "EV" then: "ExpressJet"
    | c == "WN" then: "Southwest Airlines"
    | c == "OO" then: "SkyWest Airlines"
    | otherwise: "Other"
  end
end


# -----------------------
# Task 2 — Handle Missing Data, Clean Data, and Identify Duplicates
# -----------------------

# 1)
# Fill missing/blank tailnum with "UNKNOWN"
filledTail =
  transform-column(flights53, "tailnum",
    lam(s :: String):
      if string-length(s) == 0:
        "UNKNOWN"
      else:
        s
      end
    end)

# 2)
# Clamp negative dep_delay / arr_delay to 0
cleanDelays1 =
  transform-column(filledTail, "dep_delay",
    lam(num):
      if num < 0: 
        0 
      else: 
        num 
      end
    end)

cleanDelays =
  transform-column(cleanDelays1, "arr_delay",
    lam(num):
      if num < 0: 
        0 
      else: 
      num
      end
    end)


# 3)
# Identify duplicate rows
withKey =
  build-column(flights53, "dedup_key",
    lam(r :: Row):
      string-append(
        trim(to-string(r["flight"])),
        string-append("-",
          string-append(
            string-to-upper(trim(r["carrier"])),
            string-append("-",hhmmToClock(r["dep_time"]))
          )
        )
      )
    end)

groupDuplicated = group(withKey, "dedup_key")
# groupDuplicated
countDuplicated = count(withKey, "dedup_key")
# countDuplicated

# -----------------------
# Task 3 — Normalising Categorical Values and Outliers (Optional)
# -----------------------

# 1)
carrierClean =
  transform-column(cleanDelays, "carrier",
    lam(c): string-to-upper(trim(to-string(c))) end)

withAirline =
  build-column(carrierClean, "airline",
    lam(r): carrierToAirline(r["carrier"]) end)

# 2) 
# Outliers
noOutliers =
  filter-with(withAirline,
    lam(r):
      (r["distance"] <= 5000) and (r["air_time"]  <= 500)
    end)


# -----------------------
# Task 4 — List stats with for each; plot
# -----------------------

# 1) Visualisations of the dataset
freq-bar-chart(noOutliers, "airline")
scatter-plot(noOutliers, "distance", "hour")
histogram(noOutliers, "distance", 100)

# 2)
distances = noOutliers.get-column("distance")

# 3)
# The total distance flown.
fun totalDistance(lst :: List) block:
  doc: "Compute the total of all numbers in the given list using a for-each loop"
  var total = 0
  for each(d from lst):
    total := total + d
  end
  total
where:
  totalDistance([list: 0, 1, 2, 3]) is 6
end

# The average distance
fun avgDistance(lst :: List):
  doc: "Return the average value of all numbers in the given list"
  totalDistance(lst) / length(lst)
where:
  avgDistance([list: 0, 1, 2, 3]) is 1.5
end

# The maximum distance
fun maxDistance(lst :: List) block:
  doc: "Find the maximum value in the given list using a for-each loop"
  var maxd = lst.get(0)
  for each(d from lst):
    if d > maxd:
      maxd := d
    else: 
      maxd
    end   
  end
  maxd
where:
  maxDistance([list: 0, 1, 2, 3, 7, 5]) is 7
end


# Optional: quick peek at a few columns
# preview =
#   select-columns(noOutliers, [list: "carrier", "airline", "flight", "dep_time", "arr_delay", "distance"])
# preview

