module NativeModule exposing (memoize)

import Native.NativeModule


memoize : (a -> b) -> a -> b
memoize =
    Native.NativeModule.memoize
