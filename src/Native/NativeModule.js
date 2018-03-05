var _feldman4$audio$Native_NativeModule = (function () {

	function memoize(f){
		var cache = {};
		window.cache = cache;
		return function(n){
			var key = JSON.stringify(n);
			if (key in cache){
				// console.log('Cache lookup: ' + n);
				// console.log('Cache size:' +  Object.keys(cache).length);
				return cache[key];
			}
			else {
				console.log('Calculating result: ' + key);
				cache[key] = f(n);
				return cache[key];
			}
		};
	}


	return {
		memoize: memoize
	}

})();
