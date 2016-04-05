module Fun =
struct
  let flip f x y = f y x
  let compose f g x = f (g x)

  let ( %> ) = compose
end
