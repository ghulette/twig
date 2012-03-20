# This is what we want to generate.

# [py(address) -> py(json(int,string))]
def gen1_py(in):
  x1 = in.name
  x2 = in.age
  x3 = string_to_json(x1)
  x4 = int_to_json(x2)
  x5 = tuple_to_json(x3,x4)
  return x5
