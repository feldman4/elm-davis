var _feldman4$audio$Native_NativeModule = (function () {

	function memoize(f){
		var cache = {};
		return function(n){
			if (n in cache){
				console.log('Cache lookup: ' + n);
				return cache[n];
			}
			else {
				console.log('Calculating result: ' + n);
				cache[n] = f(n);
				return cache[n]
			}
		};
	}


	return {
		memoize: memoize
	}

})();
