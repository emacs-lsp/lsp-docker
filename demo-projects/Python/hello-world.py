class Greeter():
    def __init__(self, greeting):
        self.greeting = greeting

    def say_hello(self):
        print(self.greeting + " world")


if __name__ == "__main__":
    a_greeter = Greeter("Hola")
    a_greeter.say_hello()
