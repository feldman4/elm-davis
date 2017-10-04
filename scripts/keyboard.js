var keyboard = (function() {
	"use strict";

	var module = {};

	var map = {};
	var hold = [];
	var keys = [
		[49, 50, 51, 52, 53, 54, 55, 56, 57, 48, 189, 187],
		[81, 87, 69, 82, 84, 89, 85, 73, 79, 80, 219, 221],
		[65, 83, 68, 70, 71, 72, 74, 75, 76, 186, 222],
		[90, 88, 67, 86, 66, 78, 77, 188, 190, 191]
	];

	for (let i = 0; i < keys.length; i++) {
		let row = keys[i];

		for (let j = 0; j < row.length; j++) {
			let key = row[j];

			map[key] = (4 * i + 7 * j) % 12;
		}
	}

	var base = 1;

	module.init = function() {
		$(window).keydown(onKeyDown);
		$(window).keyup(onKeyUp);
	};

	var getPitchFromKeyboardEvent = function(key) {
		var note = (base + map[key]) % 12;

		if (isFinite(note))
			return note;
		else
			return null;
	};

	var sustainNotes = {};
	var sustain = false;

	var sustainOn = function() {
		sustain = true;
		tonnetz.sustainOn(16);
	};

	var sustainOff = function() {
		sustain = false;
		tonnetz.sustainOff(16);

		for (let n in sustainNotes)
			noteOff(n);
	};

	var noteOn = function(note) {
		tonnetz.noteOn(16, note);
		audio.noteOn(note);

		delete sustainNotes[note];
	};

	var noteOff = function(note) {
		tonnetz.noteOff(16, note);

		if (sustain)
			sustainNotes[note] = true;
		else
			audio.noteOff(note);
	};

	var transpose = function(delta) {
		base = (base + delta + 12) % 12;
		tonnetz.panic();
	};

	var onKeyDown = function(event) {
		var key = event.which;

		if (hold[key])
			return;

		var note = getPitchFromKeyboardEvent(key);
		var special = event.ctrlKey || event.altKey || event.metaKey;

		if (special)
			return;

		if (note != null)
			noteOn(note);
		else if (16 == key)
			sustainOn();
		else if (32 == key)
			tonnetz.panic();
		else if (40 == key)
			transpose(4);
		else if (38 == key)
			transpose(-4);
		else if (39 == key)
			transpose(7);
		else if (37 == key)
			transpose(-7);

		hold[key] = true;
	};

	var onKeyUp = function(event) {
		var key = event.which;
		var note = getPitchFromKeyboardEvent(key);

		if (note != null)
			noteOff(note);
		else if (16 == key)
			sustainOff();

		hold[key] = false;
	};

	return module;
})();
