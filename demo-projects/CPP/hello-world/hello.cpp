#include <iostream>
#include <string>

class Greeter {
  std::string greeting;

public:
  Greeter(std::string greeting) { this->greeting = greeting; }
  void SayHello() { std::cout << this->greeting << " world!"; }
};

int main() {
  auto greeter = Greeter("Salam");
  greeter.SayHello();

  return 0;
}
