use context starter2024

data TaxonomyTree:
  node(rank :: String, name :: String, children :: List<TaxonomyTree>)
end

# Example: Part of the cat family
lion = node("Species", "Panthera leo", [list: ])
tiger = node("Species", "Panthera tigris", [list: ])
leopard = node("Species", "Panthera pardus", [list: ])
panthera = node("Genus", "Panthera", [list: lion, tiger, leopard])

house-cat = node("Species", "Felis catus", [list: ])
wildcat = node("Species", "Felis silvestris", [list: ])
felis = node("Genus", "Felis", [list: house-cat, wildcat])

felidae = node("Family", "Felidae", [list: panthera, felis])



fun count-leaves(t :: TaxonomyTree) -> Number:
  cases (List) t.children:
    | empty => 1
    | link(_, _) =>
        fun sum-children(cs :: List<TaxonomyTree>) -> Number:
          cases (List) cs:
            | empty => 0
            | link(f, r) => count-leaves(f) + sum-children(r)
          end
        end
        sum-children(t.children)
  end
  where:
  count-leaves(lion) is 1
  count-leaves(panthera) is 3
  count-leaves(felis) is 2
  count-leaves(felidae) is 5
end


# fun count-leaves(t :: TaxonomyTree) -> Number:
#   cases (List) t.children:
#     | empty => 1
#     | link(f, r) => 
#       cases (List) f.children:
#         | empty => 1
#         | else => count-leaves(r)
#       end
#   end
# where:
#   count-leaves(lion) is 1
#   count-leaves(panthera) is 3
#   count-leaves(felis) is 2
#   count-leaves(felidae) is 5
# end



# fun count-leaves(t :: TaxonomyTree) -> Number:
#   cases (List) t.children:
#     | empty => 1
#     | else => fun sum-children(cs :: List<TaxonomyTree>) -> Number:
#       cases (List) cs:
#         | empty => 0
#         | link(f, r) => count-leaves(f) + sum-children(r)
#       end
#     end
#     sum-children(t.children)
#   end
# where:
#   count-leaves(lion) is 1
#   count-leaves(panthera) is 3
#   count-leaves(felis) is 2
#   count-leaves(felidae) is 5
# end




