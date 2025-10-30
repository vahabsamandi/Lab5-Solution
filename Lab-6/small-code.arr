use context starter2024
data Animal:
  | animal(category :: String, name :: String, sound :: String)
end

# Create several Animal values
dog = animal("Pet", "Dog", "Bark")
cat = animal("Pet", "Cat", "Mew")
cow = animal("Farm", "Cow", "Moo")


# Function to return the sound of an animal
fun animal-sound(a :: Animal) -> String:
  cases (Animal) a:
    | animal(category, name, sound) =>
        "The " + category + " " + name + " says " + sound
  end
end

# Test calls
animal-sound(dog)
animal-sound(cat)
animal-sound(cow)


