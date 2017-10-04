var tonnetz = (function() {
	"use strict";

	var module = {};

	var TONE_NAMES = [
		"C", "C#", "D", "D#", "E",
		"F", "F#", "G", "G#", "A", "A#", "B"
	];

	var STATE_OFF = 0;
	var STATE_GHOST = 1;
	var STATE_SUST = 2;
	var STATE_ON = 3;
	var STATE_NAMES = ["OFF", "GHOST", "SUSTAIN", "ON"];
	var colors = {
		fill: ["#eeeeee", "#777777", "#555555", "#333333"],
		stroke: ["#555555", "#333333", "#333333", "#000000"]
	};
	var majorFillOff = "#f2dede";
	var majorFillOn = "#d9534f";
	var minorFillOff = "#d9edf7";
	var minorFillOn = "#337ab7";

	var W, H;

	var ghostDuration = 1500;

	var toneGrid = [];
	var tones;
	var channels;

	var sustain = false;

	var CHANNELS = 17;

	module.init = function() {
		tones = $.map(Array(12), function(_, i) {
			return {
				pitch: i,
				name: TONE_NAMES[i],
				state: STATE_OFF,
				byChannel: {},
				channelsSust: {},
				released: null,
				cache: {}
			};
		});

		channels = $.map(Array(CHANNELS), function(_, i) {
			return {
				number: i,
				pitches: {},
				sustTones: {},
				sustain: false
			};
		});
		module.channels = channels;

		this.rebuild();
		window.onresize = function() {
			module.rebuild();
		};
	};

	module.noteOn = function(c, pitch) {
		if (!(pitch in channels[c].pitches)) {
			let i = pitch % 12;

			tones[i].state = STATE_ON;

			if (!tones[i].byChannel[c])
				tones[i].byChannel[c] = 1;
			else
				++tones[i].byChannel[c];

			channels[c].pitches[pitch] = 1;

			delete tones[i].channelsSust[c];
			delete channels[c].sustTones[i];
		}

		this.draw();
	};

	module.noteOff = function(c, pitch) {
		if (pitch in channels[c].pitches) {
			let i = pitch % 12;

			delete channels[c].pitches[pitch];
			--tones[i].byChannel[c];

			if (tones[i].byChannel[c] === 0) {
				delete tones[i].byChannel[c];

				if ($.isEmptyObject(tones[i].byChannel)) {
					if (channels[c].sustain) {
						tones[i].state = STATE_SUST;
						channels[c].sustTones[i] = 1;
					} else {
						releaseTone(tones[i]);
					}
				}
			}

			this.draw();
		}
	};

	module.allNotesOff = function(c) {
		audio.allNotesOff();

		for (let i = 0; i < 12; i++) {
			delete tones[i].byChannel[c];
			delete tones[i].channelsSust[c];

			if ($.isEmptyObject(tones[i].byChannel))
				tones[i].state = STATE_OFF;
		}

		channels[c].pitches = {};
		channels[c].sustTones = {};

		this.draw();
	};

	module.sustainOn = function(c) {
		channels[c].sustain = true;
	};

	module.sustainOff = function(c) {
		channels[c].sustain = false;
		channels[c].sustTones = {};

		for (let i = 0; i < 12; i++) {
			delete tones[i].channelsSust[c];

			if (tones[i].state == STATE_SUST)
				if ($.isEmptyObject(tones[i].channelsSust))
					releaseTone(tones[i]);
		}

		this.draw();
	};

	module.panic = function() {
		for (let i = 0; i < CHANNELS; i++) {
			this.sustainOff(i);
			this.allNotesOff(i);
		}
	};

	var releaseTone = function(tone) {
		tone.release = new Date();

		if (ghostDuration > 0) {
			tone.state = STATE_GHOST;
			ghosts();
		} else {
			tone.state = STATE_OFF;
		}
	};

	var ghostsInterval = null;

	var ghosts = function() {
		if (ghostsInterval !== null)
			return;

		ghostsInterval = setInterval(function() {
			let numAlive = 0;
			let numDead = 0;
			let now = new Date();

			for (let i = 0; i < 12; i++) {
				let tone = tones[i];

				if (tone.state == STATE_GHOST) {
					let age = now - tone.release;

					if (age >= ghostDuration) {
						tone.state = STATE_OFF;
						++numDead;
					} else {
						++numAlive;
					}
				}
			}

			if (numAlive == 0) {
				clearInterval(ghostsInterval);
				ghostsInterval = null;
			}

			if (numDead > 0)
				module.draw();
		}, Math.min(ghostDuration, 30));
	};

	var drawTimeout = null;

	module.draw = function(immediately) {
		if (immediately) {
			if (drawTimeout !== null)
				clearTimeout(drawTimeout);

			drawNow();
			return;
		}

		if (drawTimeout === null)
			drawTimeout = setTimeout(drawNow, 30);
	};

	var fillTriad = function(dom, med, color) {
		ctx.beginPath();
		ctx.moveTo(0, 0);
		ctx.lineTo(dom.x, dom.y);
		ctx.lineTo(med.x, med.y);
		ctx.closePath();
		ctx.fillStyle = color;
		ctx.fill();
	};

	var drawNow = function() {
		var now = new Date();

		drawTimeout = null;

		ctx.clearRect(0, 0, W, H);

		for (let tone = 0; tone < 12; tone++) {
			let grid = toneGrid[tone];
			let c = tones[tone].cache;

			let t3 = (tone + 3) % 12;
			let t4 = (tone + 4) % 12;
			let t7 = (tone + 7) % 12;

			c.s0 = tones[tone].state;
			c.s3 = tones[t3].state;
			c.s4 = tones[t4].state;
			c.s7 = tones[t7].state;

			let o0 = (c.s0 != STATE_OFF);
			let o3 = (c.s3 != STATE_OFF);
			let o4 = (c.s4 != STATE_OFF);
			let o7 = (c.s7 != STATE_OFF);

			for (let i = 0; i < grid.length; i++) {
				setTranslate(grid[i].x, grid[i].y);

				let minorOn = (o0 && o7 && o3);
				let majorOn = (o0 && o7 && o4);

				let $minorLabel = $(grid[i].minorLabel);
				let $majorLabel = $(grid[i].majorLabel);

				if (minorOn) {
					$minorLabel.addClass("state-ON");
					fillTriad(d7, d3, minorFillOn);
				} else {
					$minorLabel.removeClass("state-ON");
					fillTriad(d7, d3, minorFillOff);
				}

				if (majorOn) {
					$majorLabel.addClass("state-ON");
					fillTriad(d7, d4, majorFillOn);
				} else {
					$majorLabel.removeClass("state-ON");
					fillTriad(d7, d4, majorFillOff);
				}
			}
		}

		for (let tone = 0; tone < 12; tone++) {
			let c = tones[tone].cache;
			let state = c.s0;
			let grid = toneGrid[tone];

			for (let i = 0; i < grid.length; i++) {
				setTranslate(grid[i].x, grid[i].y);

				drawEdge(d7, state, c.s7);
				drawEdge(d3, state, c.s3);
				drawEdge(d4, state, c.s4);
			}
		}

		setTranslate(0, 0);

		for (let tone = 0; tone < 12; tone++) {
			let grid = toneGrid[tone];
			let t = tones[tone];
			let className = "state-" + STATE_NAMES[t.state];

			for (let i = 0; i < grid.length; i++) {
				let x = grid[i].x;
				let y = grid[i].y;

				ctx.beginPath();
				ctx.arc(x, y, d4.y * 0.28, 0, Math.PI * 2);
				ctx.closePath();

				ctx.fillStyle = colors.fill[t.state];
				ctx.strokeStyle = colors.stroke[t.state];
				grid[i].label.className = className;

				if (t.state == STATE_OFF)
					ctx.lineWidth = 1.5;
				else
					ctx.lineWidth = 2.25;

				ctx.fill();
				ctx.stroke();
			}
		}
	};

	var setTranslate = function(x, y) {
		ctx.setTransform(1, 0, 0, 1, x, y);
	};

	var drawEdge = function(endpoint, state1, state2) {
		var state = Math.min(state1, state2);

		ctx.beginPath();
		ctx.moveTo(0, 0);
		ctx.lineTo(endpoint.x, endpoint.y);
		ctx.strokeStyle = colors.stroke[state];
		ctx.lineWidth = (state != STATE_OFF) ? 2.25 : 1.5;
		ctx.stroke();
	};

	var createLabel = function(text, x, y) {
		var label = document.createElement("div");
		var inner = document.createElement("div");

		inner.appendChild(document.createTextNode(text));
		label.appendChild(inner);
		label.style.left = x + "px";
		label.style.top = y + "px";
		return label;
	};

	var addNode = function(tone, x, y) {
		x += W / 2;
		y += H / 2;

		if (x < -d7.x || y < -d4.y || x > W + d7.x || y > H + d4.y)
			return;

		var minor = {
			x: x + (d3.x + d7.x) / 3,
			y: y + (d3.y + d7.y) / 3
		};
		var major = {
			x: x + (d4.x + d7.x) / 3,
			y: y + (d4.y + d7.y) / 3
		};
		var name = tones[tone].name;
		var nameUp = name.toUpperCase();
		var nameLow = name.toLowerCase();
		var node = {
			x: x,
			y: y
		};

		node.label = createLabel(name, x, y);
		noteLabels.appendChild(node.label);

		node.majorLabel = createLabel(nameUp, major.x, major.y);
		node.minorLabel = createLabel(nameLow, minor.x, minor.y);
		node.majorLabel.className = "major";
		node.minorLabel.className = "minor";
		triadLabels.appendChild(node.majorLabel);
		triadLabels.appendChild(node.minorLabel);

		toneGrid[tone].push(node);
	};

	var geo = [];
	var d3, d4, d7;

	var updateInt = function(pos, neg, x, y) {
		geo[pos] = {
			x: x,
			y: y
		};
		geo[neg] = {
			x: -x,
			y: -y
		};
	};

	var updateGeo = function() {
		W = window.innerWidth;
		H = window.innerHeight;
		canvas.width = W;
		canvas.height = H;

		var h = 2 * Math.sqrt(3);
		var k = H / (11 * h);

		updateInt(3, 9, 3 * k, -h * k);
		updateInt(4, 8, 4 * k, h * k);
		updateInt(7, 5, 7 * k, 0);

		d3 = geo[3];
		d4 = geo[4];
		d7 = geo[7];
	};

	var radius = function(x, y) {
		return Math.sqrt(x * x + y * y);
	};

	module.rebuild = function() {
		updateGeo();

		var r3 = radius(d3.x, d3.y);
		var r4 = radius(d4.x, d4.y);
		var r = radius(W / 2, H / 2);
		var n3 = Math.ceil(r / r3) + 1;
		var n4 = Math.ceil(r / r4) + 1;

		for (let i = 0; i < 12; i++)
			toneGrid[i] = [];

		$(noteLabels).empty();
		$(triadLabels).empty();

		$(noteLabels).css("font-size", d4.y * 0.24 + "px");
		$(triadLabels).css("font-size", d4.y * 0.24 + "px");

		for (let i3 = -n3; i3 <= n3; i3++) {
			let x3 = i3 * d3.x;
			let y3 = i3 * d3.y;
			let t3 = 3 * i3;

			for (let i4 = -n4; i4 <= n4; i4++) {
				let x4 = i4 * d4.x;
				let y4 = i4 * d4.y;
				let t4 = 4 * i4;

				let t = ((8 + t3 + t4) % 12 + 12) % 12;

				addNode(t, x3 + x4, y3 + y4);
			}
		}

		this.draw(true);
	};

	return module;
})();
