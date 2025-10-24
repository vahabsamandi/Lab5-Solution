use context dcic2024


include lists
include data-source
include csv

# ============================================================
# Task 1 — Structured Data
# ============================================================

# Load the CSV into a table named `student_score`
student_score = load-table:
  Name, Surname, Email, Score
  source: csv-table-file("students_gate_exam_score.csv",default-options)
  sanitize Score using num-sanitizer
end

# 1.1 Top-3 (Name, Surname, Score) by Score descending
top3 = rder-by(student_score, "Score", tfalse   
    
# 1.2 Structured data for a Student
data Student:
  | student(name :: String, surname :: String, score :: Number)
end

# 1.3 Make s1, s2, s3 from the top-3 table
r1 = top3.row-n(0)
r2 = top3.row-n(1)
r3 = top3.row-n(2)

s1 = student(r1["Name"], r1["Surname"], r1["Score"])
s2 = student(r2["Name"], r2["Surname"], r2["Score"])
s3 = student(r3["Name"], r3["Surname"], r3["Score"])

# 1.4 Recursive function: count how many scores > 90
scores :: List<Number> =
  link(s1.score, link(s2.score, link(s3.score, empty)))

fun count-above-90(nums :: List<Number>) -> Number:
  cases (List) nums:
    | empty => 0
    | link(first, rest) =>
        (if first > 90: 1 else: 0 end) + count-above-90(rest)
  end
end
# count-above-90(scores)


# 1.5 (Optional) Return the Students with score > 80
students3 :: List<Student> = link(s1, link(s2, link(s3, empty)))

fun students-above-80(studs :: List<Student>) -> List<Student>:
  cases (List<Student>) studs:
    | empty => empty
    | link(first, rest) =>
        if first.score > 80:
          link(first, students-above-80(rest))
        else:
          students-above-80(rest)
        end
  end
end
# students-above-80(students3)

# ============================================================
# Task 2 — Processing Lists (Emails)
# ============================================================

# 2.1 Extract the Email column as a list
all-emails = student_score.get-column("Email")

# 2.2 Split email around "@" to extract domain names (second part), then split domain name around "." to get university name (first part).

fun get-domain(email :: String) -> String:
  parts = string-split(email, "@")
  domain-part = parts.get(1)     
  domain = string-split(domain-part, ".").get(0) 
  domain
end

uni-domain = map(get-domain, all-emails)

# Use distinct to return unique university name.
universities = distinct(uni-domain)
# universities


# 2.3 Replace `nulondon.ac.uk` domains with `northeastern.edu`
fun replace-domain(email :: String):
    parts = string-split(email, "@")
    username = parts.get(0)
    domain = parts.get(1)
    
    if domain == "nulondon.ac.uk":
      username + "@northeastern.edu"
    else:
      email
    end
  end

nulondon-transformed = map(replace-domain, all-emails)

# Compare these two lists
# all-emails
# nulondon-transformed
