class Address():
  def __init__(self,name,age):
    self.name = name
    self.age = age

class AddressBook():
  def __init__(self):
    self.book = {}
  
  def add(self,name,age):
    addr = Address(name,age)
    self.book[name] = addr
  
  def lookup(self,name):
    return self.book[name]
