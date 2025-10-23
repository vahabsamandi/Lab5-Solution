use context starter2024
# =============================
# Lab 7 — Location Tags (Pyret)
# =============================

# -----------------------------
# Given data definitions
# -----------------------------

# A TagId is a String
TAG1 = "t1"
TAG2 = "t2"
TAG3 = "t3"

# A Time is a Number (seconds since Jan 1, 2025)
TIME1 = 10
TIME2 = 20
TIME3 = 30

data Encounter:
  | own-tag-encounter(tagid :: String, time :: Number)
  | other-tag-encounter(tagid :: String, time :: Number)
end

ENCOUNTER1 = own-tag-encounter(TAG1, TIME1)
ENCOUNTER2 = own-tag-encounter(TAG1, TIME2)
ENCOUNTER3 = other-tag-encounter(TAG2, TIME3)

# -----------------------------
# Helpers
# -----------------------------
fun max2(a :: Number, b :: Number) -> Number:
  if a > b: a
  else: b
  end
end

# -----------------------------
# Problem 1
# own-encounters : List<Encounter> -> List<Encounter>
# Keep only own-tag-encounters (list recursion, no loops)
# -----------------------------
fun own-encounters(encs :: List<Encounter>) -> List<Encounter>:
  cases (List<Encounter>) encs:
    | empty => empty
    | link(first, rest) =>
      cases (Encounter) first:
        | own-tag-encounter(_, _) =>
          link(first, own-encounters(rest))
        | other-tag-encounter(_, _) =>
          own-encounters(rest)
      end
  end
end

# -----------------------------
# Problem 2
# tag-activity-count : TagId, List<Encounter> -> Number
# Count encounters for a given tag id (own or other)
# -----------------------------
fun tag-activity-count(tid :: String, encs :: List<Encounter>) -> Number:
  cases (List<Encounter>) encs:
    | empty => 0
    | link(first, rest) =>
      cases (Encounter) first:
        | own-tag-encounter(tagid, _) =>
          (if tagid == tid: 1 else: 0 end) + tag-activity-count(tid, rest)
        | other-tag-encounter(tagid, _) =>
          (if tagid == tid: 1 else: 0 end) + tag-activity-count(tid, rest)
      end
  end
end

# -----------------------------
# Problem 3
# last-encounter : TagId, List<Encounter> -> Time
# Return latest time for that tag, or -1 if not present
# -----------------------------
fun last-encounter(tid :: String, encs :: List<Encounter>) -> Number:
  cases (List<Encounter>) encs:
    | empty => -1
    | link(first, rest) =>
      block:
        rest-max = last-encounter(tid, rest)
        cases (Encounter) first:
          | own-tag-encounter(tagid, time) =>
            if tagid == tid: max2(time, rest-max) else: rest-max end
          | other-tag-encounter(tagid, time) =>
            if tagid == tid: max2(time, rest-max) else: rest-max end
        end
      end
  end
end

# -----------------------------
# Tests
# -----------------------------
check:
  own-encounters([list: ENCOUNTER1, ENCOUNTER3, ENCOUNTER2])
    is [list: ENCOUNTER1, ENCOUNTER2]

  tag-activity-count(TAG1, [list: ENCOUNTER1, ENCOUNTER3, ENCOUNTER2]) is 2
  tag-activity-count(TAG2, [list: ENCOUNTER1, ENCOUNTER3, ENCOUNTER2]) is 1
  tag-activity-count(TAG3, [list: ENCOUNTER1, ENCOUNTER3, ENCOUNTER2]) is 0

  last-encounter(TAG1, [list: ENCOUNTER1, ENCOUNTER3, ENCOUNTER2]) is TIME2
  last-encounter(TAG2, [list: ENCOUNTER1, ENCOUNTER3, ENCOUNTER2]) is TIME3
  last-encounter(TAG3, [list: ENCOUNTER1, ENCOUNTER3, ENCOUNTER2]) is -1
end

# -----------------------------
# Problem 4 — Contextual Integrity (comments)
# -----------------------------
# Question: What type of information is shared?
# Answer: Location (precise/approx), timestamps, tag unique ID (or rotating IDs),
#         proximity observations (which phones saw it), and some device metadata.

# Question: Who is the subject of the information?
# Answer: Nominally the tagged item & owner's belongings; in practice, the person
#         carrying/near the tag (the tracked individual).

# Question: Who is the sender of the information?
# Answer: The location tag device.

# Question: Who are the potential recipients of the information?
# Answer: Intended — the purchaser of the tag tracking their own belongings.
#         Unintended — (1) Abusive ex-partners (explicitly mentioned),
#                      (2) Stalkers/harassers with physical access to plant a tag,
#                      (3) Thieves/burglars tracking assets or parked vehicles.

# Question: What principles govern the collection/transmission?
# Answer: Purchase implies consent for tracking one's own items; people unknowingly
#         being tracked did not consent. Minimal regulation on who can buy/use tags
#         or their properties. Data transmitted via global phone networks without
#         verifying legitimate use.
#
# Notes/Gaps: Rotating IDs and on-d

