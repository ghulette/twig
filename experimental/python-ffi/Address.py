class Address():
  def __init__(self,name,age):
    self.name = name
    self.age = age
  
  def __str__(self):
    return '[' + ', '.join([self.name,str(self.age)]) + "]"

def string_to_json(s):
  # This should actually do some encoding
  return s

def int_to_json(x):
  # This should actually do some encoding
  return str(x)

def tuple_to_json(x,y):
  return '[' + ','.join(x,y) + ']'

class AddressBook():
  def __init__(self):
    self.book = {}
  
  def add(self,name,age):
    addr = Address(name,age)
    self.book[name] = addr
  
  def lookup(self,name):
    return self.book[name]

addr = Address("Geoff", 34)
print addr
