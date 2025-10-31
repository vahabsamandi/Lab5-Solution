use context starter2024

#| Task 2 — Sensor Network (Selected Tasks)
Leaf nodes are sensors (data rate in KB/s). Internal nodes are hubs that combine
two sub-networks and have a bandwidth capacity (KB/s).
|#

data SensorNet:
  | hub(bandwidth :: Number, left :: SensorNet, right :: SensorNet)
  | sensor(rate :: Number)
end

# Example network
sA = sensor(60)
sB = sensor(120)
sC = sensor(45)

hub1 = hub(150, sA, sB)        # load here = 180 (60+120) > 150 (overloaded)
core = hub(200, hub1, sC)      # load here = 225 (180+45) > 200 (overloaded)

# 1) Total offered load (sum of leaf rates)
fun total-rate(n :: SensorNet) -> Number:
  cases (SensorNet) n:
    | sensor(rate)      => rate
    | hub(bw, l, r)     => total-rate(l) + total-rate(r)
  end
where:
  total-rate(hub1) is 180
  total-rate(core) is 225
end

# Helper: subtree load (alias to total-rate for readability)
fun offered(n :: SensorNet) -> Number:
  total-rate(n)
end

# 2) Feasibility: every hub's bandwidth must be >= load of its subtree
fun fits-capacities(n :: SensorNet) -> Boolean:
  cases (SensorNet) n:
    | sensor(rate)  => true
    | hub(bw, l, r) =>
      block:
        load = offered(l) + offered(r)
        (load <= bw) and fits-capacities(l) and fits-capacities(r)
      end
  end
where:
  fits-capacities(hub(190, hub1, sC)) is false  # still overloaded at hub1
  fits-capacities(hub(225, hub(180, sA, sB), sC)) is true
end

# 4a) (Given helper) Minimal scale factor s >= 1 so that
# for every hub: (scaled subtree load) <= bandwidth.
fun needed-scale(n :: SensorNet) -> Number:
  cases (SensorNet) n:
    | sensor(rate)  => 1
    | hub(bw, l, r) =>
      block:
        load = offered(l) + offered(r)
        here = load / bw
        num-max( num-max(here, needed-scale(l)), needed-scale(r) )
      end
  end
where:
  # For 'core': max(225/200 = 1.125, hub1: 180/150 = 1.2) = 1.2
  needed-scale(core) is 1.2
end

# 4b) Apply a scaling factor to all sensor rates (divide by s).
fun apply-scale(n :: SensorNet, s :: Number) -> SensorNet:
  cases (SensorNet) n:
    | sensor(rate)  => sensor(rate / s)
    | hub(bw, l, r) => hub(bw, apply-scale(l, s), apply-scale(r, s))
  end
end

# 4c) (Optional) Scale just enough to make the network feasible.
fun scale-to-fit(n :: SensorNet) -> SensorNet:
  s = needed-scale(n)
  if s <= 1:
    n
  else:
    apply-scale(n, s)
  end
where:
  total-rate(scale-to-fit(core)) is 225 / 1.2
  fits-capacities(scale-to-fit(core)) is true
end
