var shitty_start = function (app) {

	var ports = [];
	window.ports = ports;
	var events = [];
	window.events = events;

	var initialize = function () {
		navigator.requestMIDIAccess().then(window_asshole);
	};

	var window_asshole = function(midiAccess) {


		// add existing ports
		midiAccess.inputs.forEach(addMIDIPort);

		// listen to new ports
		// timeout to avoid onStateChange when addMIDIPort runs
		setTimeout(function () {
			midiAccess.addEventListener("statechange", onStateChange);
		}, 100);


	};

	var onStateChange = function(event) {
		var port = event.port;
		events.push(event);
		if (port.type != "input") return;

		if (port.state == "disconnected")
			removeMIDIPort(port);
		else if (port.state == "connected")
			addMIDIPort(port);
	};

	var addMIDIPort = function(port) {
		if (port.name.includes("Network Session")) {return;} 

		console.log('added midi port ' + [port.name, port.type]);

		port.addEventListener("midimessage", onMIDIMessage);
		// notify elm of midi port change
		ports.push(port);
		updateElm();
	};

	var removeMIDIPort = function(port) {
		console.log('removed midi port ' + [port.name, port.type]);
		// notify elm of midi port change
		remove(ports, port);
		updateElm();
	};

	var onMIDIMessage = function(event) {
		window.latest = event;
		// console.log(event.data[0])
		// note on
		if (event.data[0] === 144) {

			// console.log(event.data[1])
		};
		// tell Elm something happened
		var midiNote = [event.data[0], event.data[1], event.data[2]];
		window.app.ports.midiPort.send([event.target.name, midiNote]);

	};

	var updateElm = function() {
		app.ports.midiInputs.send(ports.map(x => x.name));
	}

	var remove = function(array, element){
    var index = array.indexOf(element);
    if (index > -1) {
        array.splice(index, 1);
    }
	}


	initialize();
};
