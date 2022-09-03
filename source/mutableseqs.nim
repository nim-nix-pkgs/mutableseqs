## Module imitates scala-like behavior of mutable sequences
##
## Its original use case was working with largish (but still manageable,
## i.e. fitting into memory without partitioning) sequencies, the transformed
## object is destroyed, so such methods will only work with vars, but not with lets


# Module mutableseqs

import sequtils, algorithm, tables, random, times
# types
type 
  ## since nim's requierement is to have named fields in tuples
  ## and the module relies on using tuples pretty heavily, we introduce two shortcut types 
  WtPair*[T] = tuple[
    item1: T,
    item2: T,
    weight: float
  ]
  KVPair*[T,U] = tuple[
    key: T,
    value: U
  ]
proc revert*[T](input: var seq[T]): seq[T] =
  ## returns a new sequence that is inverted input
  result = newSeq[T]()
  for item in input:
    result = result.concat(@[item])

proc groupBy*[T,U](
  input: var seq[T],
  action: proc(x: T): U
): seq[KVPair[U, seq[T]]] =
  ## Groups elements of a sequence according to keys that calculated from elements itself
  ## works with any types that can serve as a key for table, returns sequence of key-value pairs
  ##
  ## We can pass named functions or pass a function to generate keys implicitly
  ## 
  ## Example:
  ##
  ## .. code-block::
  ##   type   rndTuple = tuple[a: int,b: string] 
  ##   proc getA(x: rndTuple): int = x.a
  ##   proc getB(x: rndTuple): string = x.b
  ##   var a: seq[rndTuple] = @[(1,"a"),(2,"b"),(3,"b"),(4,"a")]
  ##   assert a.groupBy(proc(x: rndTuple): string = x.b) 
  ##     == @[
  ##           (key: a, value: @[(a: 4, b: a), (a: 1, b: a)]), 
  ##           (key: b, value: @[(a: 3, b: b), (a: 2, b: b)])
  ##          ]
  ##   assert a.groupBy(getB) 
  ##     == @[
  ##          (key: a, value: @[(a: 4, b: a), (a: 1, b: a)]), 
  ##          (key: b, value: @[(a: 3, b: b), (a: 2, b: b)])
  ##          ]
  ##
  ##  Can also be used on grouping sequences of primitive types:
  ## .. code-block::
  ##   var g =  @[1,2,-3,3,4,5,-5]
  ##   assert g.groupBy(proc(x: int): int = x*x ) == 
  ##      @[
  ##        (key: 25, value: @[-5, 5]), 
  ##        (key: 16, value: @[4]), 
  ##        (key: 9, value: @[3, -3]), 
  ##        (key: 4, value: @[2]), 
  ##        (key: 1, value: @[1])
  ##      ]      
    
  result = newSeq[KVPair[U,seq[T]]]()
  var
    ind: int = 0
    idxTable: Table[U, int] = initTable[U, int]()
    lx = input.high
  for idx in countdown(lx, 0):
    var item = input[idx]
    if (idxTable.hasKey(action(item))):
       result[idxTable[action(item)]].value.add(item)
    else:
      var 
        i = action(item)
        emptyS = newSeq[T]()
      emptyS.add(item)
      var  newTuple: KVPair[U,seq[T]] = (i, emptyS)
      result.add(newTuple)
      idxTable.add(action(item), ind)
      ind = ind + 1
    input.delete(idx, idx)  
  return result

proc keys*[T,U](x: seq[KVPair[T,U]]): seq[T] = 
  ## Convenience function, works with results of transformations: returns sequence of keys
  ## generated during grouping etc
  ## 
  ## .. code-block::
  ##     assert a.groupBy(getB).keys == @[a,b]
  ##
 
  result = newSeq[T]()
  for y in x:
    result.add(y.key)
  return result
proc values*[T,U](x: seq[KVPair[T,U]]): seq[U] =
  ## Same as keys, but returns values:
  ##
  ## .. code-block::
  ##    assert a.groupBy(getB).values ==
  ##      @[
  ##        @[(a: 4, b: a), (a: 1, b: a)], 
  ##        @[(a: 3, b: b), (a: 2, b: b)]
  ##      ]
  result = newSeq[U]()
  for y in x:
    result.add(y.value)
  return result

proc getItemVal(x: tuple, id: string): string =
  ## helper function, not imported
  for y,z in x.fieldPairs:
    if y == id:
      return $z

proc groupBy*[T](
  input: var seq[T],
  action: string
): seq[KVPair[string, seq[T]]] =
  ## shorter notation for groupBy, only works for tuples, if the sequence is not made of tuples
  ## an exception is thrown, moreover the keys in result will be cast to string
  ## in case a more general construction is needed, use "long notation" with full proc call passed
  ## .. code-block::
  ##    type   rndTuple = tuple[a: int,b: string] 
  ##    proc getA(x: rndTuple): int = x.a
  ##    proc getB(x: rndTuple): string = x.b
  ##    var a: seq[rndTuple] = @[(1,"a"),(2,"b"),(3,"b"),(4,"a")]
  ##    assert a.groupBy(proc(x: rndTuple): string = x.b) 
  ##      == a.groupBy("b")
  ##   
  result = newSeq[KVPair[string,seq[T]]]()
  var
    ind: int = 0
    idxTable: Table[string, int] = initTable[string, int]()
    lx = input.high
  for idx in countdown(lx, 0):
    var item = input[idx]
    if (idxTable.hasKey(item.getItemVal(action))):
       result[idxTable[item.getItemVal(action)]].value.add(item)
    else:
      var 
        i = item.getItemVal(action)
        emptyS = newSeq[T]()
      emptyS.add(item)
      var  newTuple: KVPair[string, seq[T]] = (i, emptyS)
      result.add(newTuple)
      idxTable.add(item.getItemVal(action), ind)
      ind = ind + 1
    input.delete(idx, idx)  
  return result
proc groupByReducing*[T, U, V](
  input: var seq[T],
  action: proc(x: T): U,
  transf: proc(x: T): V
): seq[KVPair[U,seq[V]]] =
  ## Same as groupBy, but now elements that are mapped into groupes can also be transformed by second function
  ## 
  ## .. code-block::
  ##    let c = a.groupByReducing(
  ##      proc(x:rndTuple):string = x.b,
  ##      proc(x:rndTuple):int = x.a
  ##    )
  ##    assert c == 
  ##       @[
  ##         (key: a, value: @[4, 1]), 
  ##         (key: b, value: @[3, 2])
  ##        ]
  result = newSeq[KVPair[U,seq[V]]]()
  var 
    idxTable = initTable[U, int]()
    lx = input.high
  for idx in countdown(lx, 0):
    var item = input[idx]
    let 
      trKey = action(item)  
    if (idxTable.hasKey(trKey)):
      result[idxTable[trKey]].value.add(transf(item))
    else:
      var  newTuple: KVPair[U, seq[V]] = (trKey, @[transf(item)])
      result.add(newTuple)
      let iii = result.high
      idxTable.add(trKey, iii)
    input.delete(idx, idx)
  return result

proc groupByKeeping*[T, U](
  input:  seq[T],
  action: proc(x: T): U
): seq[KVPair[U, seq[T]]] =
  ## Same as groupBy, but the original sequence is not transformed (can pass immutable values to it)
  result = newSeq[KVPair[U,seq[T]]]()
  var
    ind: int = 0
    idxTable: Table[U, int] = initTable[U, int]()
    lx = input.high
  for idx in countdown(lx, 0):
    var item = input[idx]
    if (idxTable.hasKey(action(item))):
       result[idxTable[action(item)]].value.add(item)
    else:
      var 
        i = action(item)
        emptyS = newSeq[T]()
      emptyS.add(item)
      var  newTuple: KVPair[U,seq[T]] = (i, emptyS)
      result.add(newTuple)
      idxTable.add(action(item), ind)
      ind = ind + 1
  return result
  
proc flatMap*[T, U]( x: var seq[T], tr: proc(y: T): seq[U]): seq[U] =
  ## Transforms every element of sequence into sequence of elements and combines them into a flat sequence
  ##
  ## .. code-block::
  ##      var v = @[("a",@[1,3,4],0.0),("b",@[3,4,5],4.6)]
  ##      assert v.flatMap(proc(x: tuple[a:string,b:seq[int],c:float]): seq[int]= x[1]) ==
  ##        @[3, 4, 5, 1, 3, 4] 
  result = newSeq[U]()
  var 
    lx = x.high
  for i in countdown(lx, 0):
    let
      item = x[i]
      tmp: seq[U] = tr(item)
    for z in tmp:
      result.add(z)
    x.delete(i,i)
  return result
# this functionality may be achieved by using flatMap
#proc flatten*[T, U](x: seq[tuple[a: U, b: seq[T]]]): seq[T] =
#  var result: seq[T] = newSeq[T]()
#  for item in x:
#    for subitem in item.b:
#      result.add(subitem)
#  return result

proc transform*[T, U](x:  seq[T], act: proc(y: T): U): seq[U] =
  ## Same as apply from sequtils, but can produce a sequence of different type, destroys input to save space
  ## .. code-block::
  ##    var s = @[1,2,4,5]
  ##    assert s.transform(proc(x: int): float = 1.0 / x.float) == @[1.0,0.5,0.25,0.2]
  result = newSeq[U]()
  for item in x:
    result.add(act(item))
  #var 
  #  result = newSeq[U]()
  #  l = x.len - 1
  #for i in countup(0, l):
  #  var 
  #    lastIdx = x.len - 1
  #    item  = x[lastIdx]
  #  result.add(act(item))
    #x.delete(lastIdx, lastIdx)
  #return revert(result)

proc makePairs*[T, U](
   x: seq[T], tr: proc(zz: T): U, wt: proc (zz: T, yy: T): float
): seq[WtPair[U]] = 
  ## Generates weighted pairs from elements of a sequence, accoring to procedures passed
  ## returns sequence of WtPair. tr function that transforms elements (before pairing), wt calculates each
  ## pair's weight
  ## 
  ## .. code-block::
  ## 
  ##      var 
  ##        j = @[("a",1),("b",2),("c",3)]
  ##        k = j.makePairs(
  ##          proc(
  ##            x: tuple[a: string,b: int]): string = x.a, 
  ##              proc(
  ##                x: tuple[a: string, b: int],
  ##                y: tuple[a: string, b: int]
  ##              ): float = 1.0 / (y.b.float + x.b.float)
  ##          )
  ##      assert k == 
  ##        @[
  ##          (item1: c, item2: a, weight: 0.25), 
  ##          (item1: a, item2: c, weight: 0.25), 
  ##          (item1: c, item2: b, weight: 0.2), 
  ##          (item1: b, item2: c, weight: 0.2), 
  ##          (item1: b, item2: a, weight: 0.3333333333333333), 
  ##          (item1: a, item2: b, weight: 0.3333333333333333)
  ##        ]
  
  result = newSeq[WtPair[U]]()
  var lx = x.high
  for i in countdown(lx, 0):
    var y = x[i]
    let up = if x.len > i - 1: i - 1 else: x.high
    for j in countup(0, up):
      var 
        z = x[j]
        t1,t2: WtPair[U]
        first: U = tr(y)
        second: U = tr(z)
        weight: float = wt(y,z)
      
      if first != second and  first != nil and second != nil:
        
        t1 = (first, second, weight)
        t2 = (second, first, weight)
        result.add(t1)
        result.add(t2)
  return result

proc take*[T](x: var seq[T], numIt: int): seq[T] =
  ## returns n first elements of sequence
  result = newSeq[T]()
  if numIt >= x.len:
    return x
  for i in countup(0, numIt - 1):
    result.add(x[i])
  x = newSeq[T]()
  return result

proc flatten*[T, U](x: seq[KVPair[U,seq[T]]]): seq[T] =
  ## in a sense "undoing" result of groupBy"
  result = newSeq[T]()
  if x.len == 0:
    return result
  for item in x:
    for subitem in item.value:
      result.add(subitem)
  return result

proc insertSort*[T](a: var openarray[T], cmp: proc(x, y: T): bool) =
  ## yet another sorting procedure with passed comparison function
  for i in 1 .. < a.len:
    let value = a[i]
    var j = i
    while j > 0 and cmp(value,a[j-1]):
      a[j] = a[j-1]
      dec j
    a[j] = value

proc insertSort*[T](a: var openarray[T]) =
  ## simple sort for values that understand `<` operation
  for i in 1 .. < a.len:
    let value = a[i]
    var j = i
    while j > 0 and value < a[j-1]:
      a[j] = a[j-1]
      dec j
    a[j] = value

proc shuffle*[T](x: var seq[T]): seq[T] =
  ## shuffle part of the sequence
  result = newSeq[T](x.len)
  if x.len == 0:
    return x
  let 
    total = x.len 
  var
    track = initTable[int,bool]()
    j = 0
  randomize()
  var time = int(epochTime() * 1000000)
  while time mod 10 == 0:
    time = (time / 10).toInt
  var rem =  time mod total
  while true:
    var idx =  random(total)
    if track.contains(idx) == false:
      track.add(idx,true)
      result[j] = x[idx]
      inc(j)
    if track.len == total:
      break
  return result

proc clone* [T] (x: seq[T]): seq[T]  = 
  ## clone a sequence
  result = newSeq[T]()
  for y in x:
    result.add(y)
  return result

proc extract* [T,U] (x: seq[T], extractor: proc(y: T): U): seq[U] = 
  ## extract something from a sequence
  result = newSeq[U]()
  for item in x:
    result.add(extractor(item))
  return result

proc grep* [T] (x: seq[T], f: proc(y: T): bool): seq[T] = 
  ## simplified imitation of perl's grep
  result = newSeq[T]()
  for y in x:
    if f(y):
      result.add(y)
  return result

proc getMedian* [T](x: var seq[T], cmp: proc(y, z: T): bool): T =
  ## calculate a median for an array of things based on supplied comparison function
  insertSort(x, cmp) 
  var idx: int = 
    if x.len mod 2 > 0:
      ((x.len - 1) / 2).int
    else:
      (x.len / 2).int
  return x[idx]
  
proc min*[T](x: seq[T]): T =
  ## calculate a median for an array of things based on supplied comparison function
  if x.len == 0:
    return 
  var result: T = x[0]
  for y in x:
    if y < result:
        result = y
  return result

proc zipWithIndex*[T](x: seq[T]): seq[KVPair[int, T]] = 
  ## calculate a median for an array of things based on supplied comparison function
  result = newSeq[KVPair[int,  T]]()
  for y in 0 .. x.high:
    result.add((y, x[y]))
  return result



