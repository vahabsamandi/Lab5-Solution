use context starter2024
include lists

# List -----------------------
  
# data List<a>:
#   | empty
#   | link(first :: a, rest :: List<a>)
# end

# Example list
numbers = link(1, link(2, link(7, empty)))

fun sum-list(l :: List) -> Number:
  cases (List) l:
    | empty => 0
    | link(f, r) => f + sum-list(r)
  end
end

sum-list(numbers)


# Tree ----------------

data numTree:
  | leaf(value :: Number)
  | node(value :: Number, left :: numTree, right :: numTree)
end

# Example tree
t1 = leaf(3)
t2 = leaf(7)
t3 = node(5, t1, t2)     # Node value 5, children 3 and 7
t4 = leaf(10)
root = node(8, t3, t4)   # Root value 8, children (t3) and (10)

#           8
#         /   \
#       5      10
#      / \
#     3   7

fun sum-tree(t :: numTree) -> Number:
  cases (numTree) t:
    | leaf(v) => v
    | node(v, l, r) => v + sum-tree(l) + sum-tree(r)
  end
end

sum-tree(root)

