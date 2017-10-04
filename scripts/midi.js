var midi = (function() {
	"use strict";

	var module = {};
	var midiAccess;
	var port = null;
	var channel = -1;

	module.init = function() {
		if (navigator.requestMIDIAccess)
			navigator.requestMIDIAccess().then(onMIDIInit);
	};

	var onMIDIInit = function(mAccess) {
		midiAccess = mAccess;
		midiAccess.inputs.forEach(addMIDIPort);
		midiAccess.addEventListener("statechange", onStateChange);
	};

	var onStateChange = function(event) {
		var port = event.port;

		if (port.type != "input") return;

		if (port.state == "disconnected")
			removeMIDIPort(port);
		else if (port.state == "connected")
			addMIDIPort(port);
	};

	var removeMIDIPort = function(port) {
		port.removeEventListener("midimessage", onMIDIMessage);
		tonnetz.panic();
	};

	var addMIDIPort = function(port) {
		port.addEventListener("midimessage", onMIDIMessage);
		tonnetz.panic();
	};

	var MIDI_NOTE_ON = 0x90;
	var MIDI_NOTE_OFF = 0x80;
	var MIDI_CONTROL_CHANGE = 0xB0;
	var MIDI_CC_SUSTAIN = 64;
	var MIDI_CC_ALL_CONTROLLERS_OFF = 121;
	var MIDI_CC_ALL_NOTES_OFF = 123;

	var ALL_CHANNELS = -1;
	var ALL_EXCEPT_DRUMS = -10;

	var onMIDIMessage = function(event) {
		var msg = event.data;
		var msgType = msg[0] & 0xF0;
		var msgChannel = msg[0] & 0x0F;

		console.log(event.data)

		if (channel >= 0 && msgChannel != channel)
			return;

		if (channel == ALL_EXCEPT_DRUMS && msgChannel == 9)
			return;

		switch (msgType) {
		case MIDI_NOTE_ON:
			tonnetz.noteOn(msgChannel, msg[1]);
			break;
		case MIDI_NOTE_OFF:
			tonnetz.noteOff(msgChannel, msg[1]);
			break;
		case MIDI_CONTROL_CHANGE:
			switch (msg[1]) {
			case MIDI_CC_SUSTAIN:
				if (msg[2] >= 64)
					tonnetz.sustainOn(msgChannel);
				else
					tonnetz.sustainOff(msgChannel);
				break;
			case MIDI_CC_ALL_CONTROLLERS_OFF:
				tonnetz.sustainOff(msgChannel);
				break;
			case MIDI_CC_ALL_NOTES_OFF:
				tonnetz.allNotesOff(msgChannel);
				break;
			}
			break;
		}
	};

	return module;
})();
