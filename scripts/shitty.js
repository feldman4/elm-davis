
var shitty_start = function () {

	var window_asshole = function(midiAccess) {
		console.log('window asshole activated')
		window.fuckyou = midiAccess

		midiAccess.inputs.forEach(addMIDIPort);
		midiAccess.addEventListener("statechange", onStateChange);

	}


	var onStateChange = function(event) {
		var port = event.port;
		console.log('state change')

		if (port.type != "input") return;

		if (port.state == "disconnected")
			removeMIDIPort(port);
		else if (port.state == "connected")
			addMIDIPort(port);
	};


	navigator.requestMIDIAccess().then(window_asshole);

	var addMIDIPort = function(port) {
		console.log('added midi port ' + [port.name, port.type])
		window.lastport = port;
		port.addEventListener("midimessage", onMIDIMessage);
	}

	var onMIDIMessage = function(event) {
		window.latest = event;
		// console.log(event.data[0])
		// note on
		if (event.data[0] === 144) {

			// console.log(event.data[1])
		}
		// tell Elm something happened
		var info = [event.data[0], event.data[1], event.data[2]];
		window.app.ports.midiPort.send(info);

		}

}
