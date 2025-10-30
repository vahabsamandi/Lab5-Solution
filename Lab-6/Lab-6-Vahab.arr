use context dcic2024


include lists
include data-source
include csv


student_score = load-table:
  name, surname, email, score
  source: csv-table-file("students_gate_exam_score.csv",default-options)
  sanitize score using num-sanitizer
end

top-3 = order-by(student_score, "score", false)


data Student:
  | student(name :: String, surename :: String, score :: Number)
end

r1 = top-3.row-n(0)
r2 = top-3.row-n(1)
r3 = top-3.row-n(2)

s1 = student(r1["name"], r1["surname"], r1["score"])
s2 = student(r2["name"], r2["surname"], r2["score"])
s3 = student(r3["name"], r3["surname"], r3["score"])

scores :: List<Number> = 
  link(s1.score, link(s2.score, link(s3.score, empty)))
scores

fun count-above-90(score :: List<Number>) -> Number:
  cases (List) score:
    | empty => 0
    | link(first, rest) =>
      (if first > 90: 1 else: 0 end) + count-above-90(rest)
  end
end

count-above-90(scores)

top-3-studs :: List<Student> = 
  link(s1, link(s2, link(s3, empty)))

fun students-above-80(studs :: List<Student>) -> List<Student>:
  cases (List<Student>) studs:
    | empty => empty
    | link(first, rest) => if first.score > 80: link(first, students-above-80(rest)) else: students-above-80(rest)
      end
  end
end

students-above-80(top-3-studs)

fun avg-scores(score :: List<Number>) -> Number:
  cases (List) score:
    | empty => 0
    | link(first, rest) =>
      (first / length(scores)) + avg-scores(rest) 
  end
end

avg-scores(scores)

string-index-of("Vahab", "ha")