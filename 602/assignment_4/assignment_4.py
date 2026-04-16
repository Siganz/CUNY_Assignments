import math

# Q1
class BankAccount:
    def __init__(self, bankname, firstname, lastname, balance=0):
        self.bankname = bankname
        self.firstname = firstname
        self.lastname = lastname
        self.balance = balance

    def deposit(self, amount):
        self.balance += amount

    def withdrawal(self, amount):
        if amount <= self.balance:
            self.balance -= amount
        else:
            print("Insufficient funds")

    def __str__(self):
        return f"{self.bankname}, {self.firstname} {self.lastname}, Balance: ${self.balance}"


# test Q1
account = BankAccount("Chase", "Shawn", "Ganz")
print(account)
print()

account.deposit(500)
account.deposit(200)

print(account)
print()

account.withdrawal(800) # should fail
account.withdrawal(100)
print()

print(account)
print()

# Q2
class Box:
    def __init__(self, length, width):
        self.length = length
        self.width = width

    def render(self):
        for _ in range(self.width):
            print("*" * self.length)

    def invert(self):
        self.length, self.width = self.width, self.length

    def get_area(self):
        return self.length * self.width

    def get_perimeter(self):
        return 2 * (self.length + self.width)

    def double(self):
        self.length *= 2
        self.width *= 2

    def __eq__(self, other):
        if not isinstance(other, Box):
            return False
        return self.length == other.length and self.width == other.width

    def print_dim(self):
        print(f"Length: {self.length}, Width: {self.width}")

    def get_dim(self):
        return (self.length, self.width)

    def combine(self, other):
        self.length += other.length
        self.width += other.width

    def get_hypot(self):
        return math.sqrt(self.length ** 2 + self.width ** 2)


# test Q2
box1 = Box(5, 10)
box2 = Box(3, 4)
box3 = Box(5, 10)

box1.print_dim()
box2.print_dim()
box3.print_dim()
print()

print(f"Is box1 == box2? {box1 == box2}")
print(f"Is box1 == box3? {box1 == box3}")
print()

box1.combine(box3)
box2.double()
box1.combine(box2)

box1.print_dim()
box2.print_dim()
print()

box_test = Box(1, 3)
box_test.print_dim()
print()

print("render() check:")
box_test.render()
print()

box_test.invert()
print(f"invert() check: {box_test.get_dim()}")
print()

print(f"get_perimeter() check: {box_test.get_perimeter()}")
print(f"get_area() check: {box_test.get_area()}")
print(f"get_hypot() check: {box_test.get_hypot()}")