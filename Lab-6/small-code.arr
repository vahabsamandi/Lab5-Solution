use context starter2024
data Animal:
  | animal(category :: String, name :: String, sound :: String)
end

# Create several Animal values
dog1 = animal("Pet", "Dog", "Bark")
cat1 = animal("Pet", "Cat", "Mew")
cow1 = animal("Farm", "Cow", "Moo")
sheep1 = animal("Farm", "Sheep", "Baa")
lion1 = animal("Wild", "Lion", "Roar")

# Function to return the sound of an animal
fun animal-sound(a :: Animal) -> String:
  cases (Animal) a:
    | animal(category, name, sound) =>
        "The " + category + " " + name + " says " + sound
  end
end

# Test calls
animal-sound(dog1)
animal-sound(cat1)
animal-sound(cow1)
animal-sound(sheep1)
animal-sound(lion1)

