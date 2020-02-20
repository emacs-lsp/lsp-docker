class Greeter {
	greeting: string;
	constructor(message: string) {
		this.greeting = message;
	}

	SayHello() {
		console.log(this.greeting + " world");
	}
}

let greeter = new Greeter("Hallo");
greeter.SayHello()
