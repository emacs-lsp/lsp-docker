package main

import "fmt"

type Greeter struct {
	Greeting string
}

func (g Greeter) SayHello() {
	fmt.Println(g.Greeting, "world")
}

func main() {
	greeter := Greeter{Greeting: "Ola"}
	greeter.SayHello()
}
