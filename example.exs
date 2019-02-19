def length([]), do: 0

def length([x | xs]), do: (1 + length(xs)) -- def(length(x)) do
  case x do
    # Domain: DList 0 Any -> DInt (0, 0)
    [] -> 0
    # Domain: DList (1, Infinite) -> (Self + 1) # DInt (1, Infinite)
    [x | xs] -> 1 + length(xs)
  end
end
