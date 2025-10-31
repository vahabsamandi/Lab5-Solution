use context starter2024


# 1
fun long-station-names(l :: List<String>) -> List<String>:
  doc: "return station names with length > 12"
  cases (List) l:
    | empty => empty
    | link(f, r) =>
      ask:
        | string-length(f) > 12 then: link(f, long-station-names(r))
        | otherwise: long-station-names(r)
      end
  end
where:
  long-station-names([list: "Main St", "University Avenue", "Central Station"]) 
    is [list: "University Avenue", "Central Station"]
  long-station-names([list: "Park", "Downtown Crossing", "Elm Rd"]) 
  is [list: "Downtown Crossing"]
end


# 2

fun peak-dock-load-acc(cur-max, l :: List<Number>) -> Number:
  cases (List) l:
    | empty => cur-max
    | link(f, r) =>
      ask:
        | f > cur-max then: peak-dock-load-acc(f, r)
        | otherwise:       peak-dock-load-acc(cur-max, r)
      end
  end
end

fun peak-dock-load(readings :: List<Number>) -> Number:
  cases (List) readings:
    | empty      => raise("No dock readings available")
    | link(f, r) => peak-dock-load-acc(f, r)
  end
where:
  peak-dock-load([list: 7, 12, 9, 15, 11]) is 15
  peak-dock-load([list: -2, -5]) is -2  # handles odd sensor glitches too
  peak-dock-load([list: 3, 3, 3]) is 3
peak-dock-load([list: ]) raises "No dock readings available"
end


