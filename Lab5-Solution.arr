use context dcic2024

include table 
include image
include csv
include data-source

# 1) Load the table

flights0 = load-table:
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
  source: csv-table-file("flights_sample50_messy.csv",default-options)
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

flights0

# -----------------------
# Lightweight helpers (no string-trim, floor, etc.)
# -----------------------

# Trim spaces at both ends
# --- generic substring helper: slice(s, start, stop) returns s[start:stop)
fun sliceRaw(s :: String, i :: Number, j :: Number) -> String:
  if i >= j: ""
  else: string-append(string-char-at(s, i), sliceRaw(s, i + 1, j))
  end
end

fun slice(s :: String, start :: Number, stop :: Number) -> String:
  n = string-length(s)
  i = if start < 0: 0 else: start end
  j = if stop > n: n else: stop end
  if i >= j: "" else: sliceRaw(s, i, j) end
end

# --- trim without string-slice
fun trim(s :: String) -> String:
  fun dropLeft(t :: String) -> String:
    if string-length(t) == 0:
      t
    else if string-char-at(t, 0) == " ":
      dropLeft(slice(t, 1, string-length(t)))
    else:
      t
    end end
  fun dropRight(t :: String) -> String:
    if string-length(t) == 0:
      t
    else:
      last1 = string-length(t) - 1
      if string-char-at(t, last1) == " ":
        dropRight(slice(t, 0, last1))
      else:
        t
      end
    end
  end
  dropRight(dropLeft(s))
end

# --- 517 / "517"  ->  "05:17", no string-slice needed
fun hhmmToClock(x) -> String:
  s0 = if is-string(x): x else: to-string(x) end
  s  = trim(s0)
  # take the rightmost 4 chars (pad-left with zeros first)
  padded = string-append("0000", s) 
  n = string-length(padded)
  last4 = slice(padded, n - 4, n)
  hh = slice(last4, 0, 2)
  mm = slice(last4, 2, 4)
  string-append(hh, string-append(":", mm))

where:
  hhmmToClock(517)   is "05:17"
  hhmmToClock("233") is "02:33"
  hhmmToClock("5")   is "00:05"
end


# Map standardized carrier code -> airline name (use ask; valid identifier name)
fun carrierToAirline(code :: String) -> String:
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
# Task 2 — Deduplicate & fill missing / clamp negatives
# Strategy: make a key (carrier|flight|time_hour), sort by it, keep first of each group
# -----------------------

withKey =
  build-column(flights0, "dedup_key",
    lam(r):
      string-append(
        string-to-upper(trim(to-string(r["carrier"]))),
        string-append("|",
          string-append(
            to-string(r["flight"]),
            string-append("|", trim(to-string(r["time_hour"])))
          )
        )
      )
    end)

sortedByKey = order-by(withKey, "dedup_key", true)




# keep first of each key by remembering last key we saw (side-effect in lam)
var lastKey = "⌀"
deduped =
  filter-with(sortedByKey,
    lam(r) block:
      k = r["dedup_key"]
      keep = (k <> lastKey)   # use <> instead of !=
      lastKey := k
      keep
    end)


# Fill missing/blank tailnum with "UNKNOWN"
filledTail =
  transform-column(deduped, "tailnum",
    lam(t):
      s = to-string(t)
      trimmed = trim(s)
      if string-length(trimmed) == 0:
        "UNKNOWN"
      else:
        s
      end
    end)
filledTail
# Clamp negative dep_delay / arr_delay to 0 (if present)
cleanDelays1 =
  transform-column(filledTail, "dep_delay",
    lam(d):
      if d < 0: 
        0 
      else: 
        d 
      end
    end)

cleanDelays =
  transform-column(cleanDelays1, "arr_delay",
    lam(a):
      if a < 0: 
        0 
      else: 
      a end
    end)

# -----------------------
# Task 3 — Fix formatting & airline names
# -----------------------

carrierClean =
  transform-column(cleanDelays, "carrier",
    lam(c): string-to-upper(trim(to-string(c))) end)

withAirline =
  build-column(carrierClean, "airline",
    lam(r): carrierToAirline(r["carrier"]) end)

# -----------------------
# Task 4 — Remove outliers; normalize departure time
# -----------------------

noOutliers =
  filter-with(withAirline,
    lam(r):
      (is-number(r["distance"]) and (r["distance"] <= 5000))
      and
      (is-number(r["air_time"])  and (r["air_time"]  <= 500))
    end)

normalizedTimes =
  transform-column(noOutliers, "dep_time",
    lam(t): hhmmToClock(t) end)

cleanedFlights = normalizedTimes

# -----------------------
# Task 5 — List stats with for each; plot
# -----------------------

distances = cleanedFlights.get-column("distance")

fun statsDistance(lst :: List<Number>) -> { total :: Number, max :: Number, avg :: Number } block:
  var total = 0
  var maxd =
    if length(lst) == 0: 0 else: lst.get(0) end
  for each(d from lst) block:
    total := total + d
    when d > maxd:
      maxd := d
    end
  end
  avg =
    if length(lst) == 0: 0
    else: total / length(lst) end
  { total: total, max: maxd, avg: avg }
where:
  statsDistance([list: 100, 300, 600]).total is 1000
  statsDistance([list: 100, 300, 600]).max   is 600
end

distStats = statsDistance(distances)
# Inspect in the interactions pane:
# distStats.total, distStats.max, distStats.avg

# Frequency chart by airline (requires your plotting util to be included above)
# freq-bar-chart(cleanedFlights, "airline")

# Optional: quick peek at a few columns
preview =
  select-columns(cleanedFlights, [list: "carrier", "airline", "flight", "dep_time", "arr_delay", "distance"])

