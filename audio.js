var audio = (function() {
	"use strict";

	var noctaves = 4;

	var shepard = function(freq) {
		var x = Math.log2(freq / 360) / (noctaves / 3);

		return Math.exp(-x * x);
	};

	var createWave = function(freq) {
		var n = 1 + 1 << (2 * noctaves + 1);
		var re = new Float32Array(n);
		var im = new Float32Array(n);

		for (let i = 1; i < n; i *= 2)
			re[i] = shepard(i * freq);

		return ctx.createPeriodicWave(re, im);
	};

	var Note = function(pitch) {
		var base = pitch - 12 * noctaves;
		var freq = Math.pow(2, (base - 9) / 12) * 440;
		var wave = createWave(freq);

		this.oscillator = ctx.createOscillator();
		this.oscillator.frequency.value = freq;
		this.oscillator.setPeriodicWave(wave);
		this.output = ctx.createGain();
		this.gain = this.output.gain;
		this.gain.value = 0;
		this.oscillator.connect(this.output);
		this.output.connect(ctx.destination);
		this.oscillator.start();
	};

	var maxVol = 1 / 12;
	var sustVol = maxVol / 4;
	var attack = 0.008;
	var release = 0.24;

	Note.prototype.start = function() {
		var now = ctx.currentTime;

		this.gain.cancelScheduledValues(now);
		this.gain.setTargetAtTime(maxVol, now, attack);
		this.gain.setTargetAtTime(sustVol, now + attack, release);
	};

	Note.prototype.stop = function() {
		var now = ctx.currentTime;

		this.gain.cancelScheduledValues(now);
		this.gain.setTargetAtTime(0, now, release);
	};

	var module = {};

	var ctx;

	var notes = [];

	module.init = function() {
		var AudioContext = window.AudioContext;

		if (AudioContext) {
			ctx = new AudioContext();

			for (let t = 0; t < 12; t++)
				notes[t] = new Note(t);

			module.noteOn = function(pitch) {
				notes[pitch].start();
			};

			module.noteOff = function(pitch) {
				notes[pitch].stop();
			};
		}
	};

	module.noteOn = function() {};

	module.noteOff = function() {};

	module.allNotesOff = function() {
		for (let t = 0; t < 12; t++)
			module.noteOff(t);
	};

	return module;
})();
