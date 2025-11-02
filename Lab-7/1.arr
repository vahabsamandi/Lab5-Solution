use context starter2024
#| Binary Trees (Sensor Network Analysis) 
   Leaf nodes are sensors (data rate in KB/s). Internal nodes are hubs that combine two sub-networks and have a bandwidth capacity (KB/s).
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

# 1) Total load (sum of leaf rates)
fun total-load(n :: SensorNet) -> Number:
  cases (SensorNet) n:
    | sensor(rate) => rate
    | hub(bw, l, r) => total-load(l) + total-load(r)
  end
where:
  total-load(hub1) is 180
  total-load(core) is 225
end


# 2) Feasibility: every hub's bandwidth must be >= load of its subtree
fun fits-capacities(n :: SensorNet) -> Boolean:
  cases (SensorNet) n:
    | sensor(rate) => true
    | hub(bw, l, r) => 
      block:
        load = total-load(l) + total-load(r)
        (load <= bw) and fits-capacities(l) and fits-capacities(r)
      end
  end
      
where:
  fits-capacities(hub(190, hub1, sC)) is false  # still overloaded at hub1
  fits-capacities(hub(225, hub(190, sA, sB), sC)) is true
end

# 3) Depth of deepest sensor (root at depth 0)
fun deepest-depth(n :: SensorNet) -> Number:
  cases (SensorNet) n:
    | sensor(rate) => 0
    | hub(bw, l, r) => 1 + num-max(deepest-depth(l), deepest-depth(r))
  end
  
where:
  deepest-depth(core) is 2    # core -> hub1 -> sA/sB
  deepest-depth(hub1) is 1
end

# (Given helper) Minimal scale factor s >= 1 so that
fun needed-scale(n :: SensorNet) -> Number:
  doc: "Compute the minimal scaling factor s ≥ 1 required for the network to become feasible under all hub bandwidths."
  cases (SensorNet) n:
    | sensor(rate)  => 1
    | hub(bw, l, r) =>
      block:
        load = total-load(l) + total-load(r)
        s = load / bw
        num-max(num-max(s, needed-scale(l)), needed-scale(r))
      end
  end
where:
  # For 'core': max(225/200 = 1.125, hub1: 180/150 = 1.2) = 1.2
  needed-scale(core) is 1.2
end

# 4) Apply a scaling factor to all sensor rates (divide by s).
fun apply-scale(n :: SensorNet, s :: Number) -> SensorNet:
  cases (SensorNet) n:
    | sensor(rate) => sensor(rate / s)
    | hub(bw, l, r) => hub(bw, apply-scale(l, s), apply-scale(r, s))
  end
  
where:
  apply-scale(core, needed-scale(core)).left.right.rate is 100    
  apply-scale(core, needed-scale(core)).right.rate is 37.5        
  apply-scale(core, needed-scale(core)).bandwidth is 200         
  apply-scale(core, needed-scale(core)).left.bandwidth is 150
end

# 5) (Optional) Scale just enough to make the network feasible.
fun scale-to-fit(n :: SensorNet) -> SensorNet:
  s = needed-scale(n)
  if s <= 1:
    n
  else:
    apply-scale(n, s)
    end

where:
  total-load(scale-to-fit(core)) is 225 / 1.2
  fits-capacities(scale-to-fit(core)) is true
end


