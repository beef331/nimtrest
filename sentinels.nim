import std/typetraits

type Sentinel*[sentinelVal: static auto] = distinct typeof(sentinelVal)

func sentinel*[T](val: T, sentVal: static T): auto =
  Sentinel[sentVal](val)

func sentinel*[T](val: T): auto =
  Sentinel[default(T)](val)

func asgn*(sentinel: var Sentinel, val: typeof(sentinel.sentinelVal)) =
  (sentinel.distinctBase) = val

func isVal*(s: Sentinel): bool =
  s.distinctBase != s.sentinelVal

func isSentinel*(s: Sentinel): bool =
  s.distinctBase == s.sentinelVal

var a: Sentinel[100]
assert a.isVal
a = sentinel(100, 100)
assert a.isSentinel

import std/math

var b = sentinel(0f, Tau)
assert b.isVal
b.asgn Tau
assert b.isSentinel
b.asgn 10f
assert b.isVal
