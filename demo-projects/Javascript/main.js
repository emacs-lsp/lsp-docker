class Greeter {
	constructor(greeting) {
		this.greeting = greeting
	}

	sayHello() {
		console.log(`${this.greeting} world`)
	}
}

function main() {
	const aGreeter = new Greeter("Konichiwa")
	aGreeter.sayHello()
}

main()
