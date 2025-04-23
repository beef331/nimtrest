##[
This is a not a sincere attempt at an ECS, more a sincere attempt at making an ECS without an inane amount of macros.
That means yeacs uses a fair bit of runtime type information.
]##

import std/[typetraits, macros, genasts, strformat, tables, hashes, sequtils, packedsets, strutils]
import alignedseq
export tables

proc onlyUniqueValues*(t: typedesc[tuple]): bool =
  result = true
  let def = default(t)
  for nameA, fieldA in def.fieldPairs:
    for nameB, fieldB in def.fieldPairs:
      when nameA != nameB and fieldA is typeof(fieldB):
        return false

type
  ComponentTuple* = concept ct, type CT
    onlyUniqueValues(CT)
  Not*[T] = distinct T


type
  TypeInfo = distinct int
  TypeInfos = PackedSet[TypeInfo]

  Entity* = ref object
    archIndex, entIndex: int

  Archetype* = ref object
    types: TypeInfos
    typeToIndex: Table[TypeInfo, int]
    data: seq[AlignedSeq[byte]] # type erased data collections, the RTTI is used to allocate/move these
    sizes: seq[int] # Size of the data
    sinkHooks: seq[proc(a, b: pointer){.nimcall, raises: [].}]
    destroyHooks: seq[proc(a: pointer){.nimcall, raises: [].}]
    stringHooks: seq[proc(a: pointer): string{.nimcall, raises: [].}]
    len: int

  World* = object
    archetypes*: seq[Archetype]
    entityPointers: seq[Entity]

  QueryIndex*[T: ComponentTuple] = object
    indices: seq[int]
    generation: int

proc hash*(t: TypeInfo): Hash {.borrow.}
proc `==`*(a, b: TypeInfo): bool {.borrow.}

var
  typeInfoCounter {.compiletime.} = 0
  typeInfoNames: Table[TypeInfo, string]

proc getTheTypeInfo*[T](t: typedesc[T]): TypeInfo =
  const id = typeInfoCounter
  result = TypeInfo(id)
  once:
    discard typeInfoNames.hasKeyOrPut(result, $T)
  static: inc typeInfoCounter

proc getTheTypeInfo*[T](t: typedesc[Not[T]]): TypeInfo =
  T.getTheTypeInfo()

proc `$`*(t: TypeInfos): string =
  result = "{"
  for x in t:
    result.add typeInfoNames[x]
    result.add ", "

  if result.endsWith ", ":
    result.setLen(result.high - 1)
  result.add "}"

macro forTuplefields(tup: typed, body: untyped): untyped =
  result = newStmtList()
  let tup =
    if tup.kind != nnkTupleConstr:
      tup.getTypeInst[^1]
    else:
      tup

  for x in tup:
    let body = body.copyNimTree()
    body.insert 0:
      genast(x):
        type Field {.inject.} = x
    result.add nnkIfStmt.newTree(nnkElifBranch.newTree(newLit(true), body))
  result = nnkBlockStmt.newTree(newEmptyNode(), result)

proc `=destroy`(arch: var typeof(Archetype()[])) =
  if arch.len > 0:
    for i in 0..<arch.data.len:
      let size = arch.sizes[i]
      for j in 0..<arch.len:
        arch.destroyHooks[i](arch.data[i][j * size].addr)
  reset arch.types
  reset arch.data
  reset arch.sizes
  reset arch.sinkHooks
  reset arch.destroyHooks


proc `hash`*(arch: Archetype): Hash = cast[Hash](arch)

proc getRequired(t: typedesc[ComponentTuple]): int =
  ## Returns required component count
  ## This is the count minus the `Not` count
  t.forTuplefields:
    when Field isnot Not:
      inc result

proc typeCount*(archetype: Archetype): int =
  # Amount of components this archetype has
  archeType.types.len

proc makeArchetype*[T](tup: typedesc[T]): Archetype =
  ## Generates an archetype based of a tuple.
  const tupLen = getRequired(T)
  result =
    Archetype(
      data: newSeqOfCap[AlignedSeq[byte]](tupLen),
      sizes: newSeqOfCap[int](tupLen)
    )

  forTuplefields(T):
    let fieldId = Field.getTheTypeInfo()
    result.typeToIndex[fieldId] = result.typeToIndex.len
    result.types.incl fieldId
    result.sinkHooks.add proc(a, b: pointer) {.nimcall.} =
      let
        a = cast[ptr Field](a)
        b = cast[ptr Field](b)
      a[] = move b[]

    result.destroyHooks.add proc(a: pointer) {.nimcall.} =
      let a = cast[ptr Field](a)
      reset a[]

    result.stringHooks.add proc(a: pointer): string {.nimcall.} =
      when compiles($cast[ptr Field](a)[]):
        $Field & $cast[ptr Field](a)[]
      else:
        $Field & "(...)"


    result.sizes.add sizeof(Field)
    result.data.add AlignedSeq[byte].init(alignOf(Field), cap = sizeof(Field) * 64)

  echo "Created Archetype: ", result.types, " from ", T

proc makeArchetype[T](typeInfo: sink TypeInfos, previous: Archetype, newType: T): Archetype =
  result =
    Archetype(
      types: typeInfo,
      typeToIndex: previous.typeToIndex,
      sizes: previous.sizes,
      data: newSeq[AlignedSeq[byte]](previous.data.len),
      sinkHooks: previous.sinkHooks,
      destroyHooks: previous.destroyHooks,
      stringHooks: previous.stringHooks
    )

  let id = T.getTheTypeInfo()
  echo "Created Archetype with: ", result.types, " from: ", previous.types
  result.typeToIndex[id] = previous.data.len

  for i, data in result.data.mpairs:
    data = AlignedSeq[byte].init(previous.data[i].alignment, cap = result.sizes[i] * 64)

  result.data.add AlignedSeq[byte].init(alignOf(T), cap = sizeof(T) * 64)
  result.sizes.add sizeof(newType)

  result.sinkHooks.add proc(a, b: pointer) =
    let
      a = cast[ptr T](a)
      b = cast[ptr T](b)
    a[] = move b[]

  result.destroyHooks.add proc(a: pointer) =
    let a = cast[ptr T](a)
    reset a[]

  result.stringHooks.add proc(a: pointer): string {.nimcall.} =
    when compiles($cast[ptr T](a)[]):
      $T & $cast[ptr T](a)[]
    else:
      $T & "(...)"

proc makeArchetypeRemoving(previous: Archetype, comp: TypeInfo): Archetype =
  result = Archetype(
    sizes: previous.sizes,
    types: previous.types,
    typeToIndex: previous.typeToIndex,
    sinkHooks: previous.sinkHooks,
    destroyHooks: previous.destroyHooks,
    stringHooks: previous.stringHooks
  )

  let toRemove = result.typeToIndex[comp]
  for i in result.typeToIndex.mvalues:
    if i > toRemove:
      dec i

  result.typeToIndex.del(comp)
  result.sizes.delete(toRemove)
  result.sinkHooks.delete(toRemove)
  result.destroyHooks.delete(toRemove)
  result.stringHooks.delete(toRemove)
  result.types.excl comp




proc `$`*(arch: Archetype): string =
  for ent in 0..<arch.len:
    result.add "Entity " & $ent & ": ("
    for comp in arch.types:
      let ind = arch.typeToIndex[comp]
      result.add arch.stringHooks[ind](arch.data[ind][ent * arch.sizes[ind]].addr)
      result.add ", "
    if result.endsWith ", ":
      result.setLen(result.high - 1)

    result.add "), "
  if result.endsWith ", ":
    result.setLen(result.high - 1)

iterator filterInd*[T](archetypes: openarray[Archetype], tup: typedesc[T]): (int, Archetype) =
  ## Iterates archetypes yielding all that can be converted to the tuple and the index.
  ## This means they share at least all components of `tup`
  const requiredCount = getRequired(T)
  for i, arch in archetypes:
    var found = 0
    block search:
      if arch.typeCount >= requiredCount:
        forTuplefields(tup):
          if Field.getTheTypeInfo in arch.types:
            when Field is Not:
              found = 0
              break search
            else:
              inc found

    if found == requiredCount:
      yield (i, arch)

iterator filter*(archetypes: openarray[Archetype], tup: typedesc[ComponentTuple]): Archetype =
  ## Iterates archetypes yielding all that can be converted to the tuple.
  ## This means they share at least all components of `tup`
  for _, arch in archetypes.filterInd(tup):
    yield arch

iterator filter*(archetypes: openarray[Archetype], typeInfo: TypeInfos): (int, Archetype) =
  ## Iterates archetypes yielding the position an archetype appears and the archetype that can be converted to the tuple.
  ## This means they share at least all components of `tup`.
  for i, arch in archetypes.pairs:
    if arch.types >= typeInfo:
      yield (i, arch)

proc filter*(archetypes: openarray[Archetype], tup: typedesc[ComponentTuple]): seq[Archetype] =
  for x in filter(archeTypes, tup):
    result.add x

proc add*[T](arch: Archetype, tup: sink T) =
  for field in tup.fields: # Iterate each component and copy it to `data`
    let id = field.typeof.getTheTypeInfo()
    assert id in arch.types
    let ind = arch.typeToIndex[id]

    let startLen = arch.data[ind].len
    arch.data[ind].setLen(arch.data[ind].len + sizeof(field))
    arch.sinkHooks[ind](arch.data[ind][startLen].addr, field.addr)
  inc arch.len

proc removeEntity(arch: Archetype, entityId: int) =
  # This logic handles moving data around the old buffer
  for i, comp in arch.data.mpairs:
    let size = arch.sizes[i]
    comp.delete(size * entityId .. size * (entityId + 1) - 1)
  dec arch.len

proc moveEntity(fromArch, toArch: Archetype, entityId: int) =
  for i in toArch.types * fromArch.types:
    let
      toInd = toArch.typeToIndex[i]
      fromInd = fromArch.typeToIndex[i]
      size = toArch.sizes[toInd]
      startLen = toArch.data[toInd].len

    assert toArch.data[toInd].alignment == fromArch.data[fromInd].alignment
    assert size == toArch.sizes[toInd]
    toArch.data[toInd].setLen(startLen + size)
    toArch.sinkHooks[toInd](toArch.data[toInd][startLen].addr, fromArch.data[fromInd][entityId * size].addr)

  inc toArch.len
  fromArch.removeEntity(entityId)

macro generateAccess(arch: Archetype, ind: int, indArray: array, tup: typed): untyped =
  result = nnkTupleConstr.newTree()
  for i, val in tup.getTypeInst[^1]:
    if val.kind != nnkBracketExpr or (val.kind == nnkBracketExpr and val[0] != bindSym"Not"):
      result.add:
        genast(val, ind, indArray, i):
          doAssert arch.data[indArray[i]].len >= 0
          let element = arch.data[indArray[i]][sizeof(val) * ind].addr
          cast[ptr val](element)[]

macro varTuple*(t: typedesc): untyped =
  result = t.getTypeInst[^1].copyNimTree

  for i in countDown(result.len - 1, 0):
    if result[i].kind == nnkBracketExpr and result[i][0] == bindSym"Not":
      result.del(i)

  for i, x in result:
    if x.kind != nnkBracketExpr:
      result[i] = nnkVarTy.newTree(x)
  result = newCall("typeof", result)

iterator foreach*[T](arch: Archetype, tup: typedesc[T]): tup.varTuple =
  var
    indices: array[getRequired(T), int] # calculate field index once instead of per entity, needed cause tuples ar order invariant
    found = 0

  tup.forTuplefields:
    when Field is Not:
      if Field.getTheTypeInfo in arch.types: # We hit on a `Not` exiting
        found = -1
        break
    else:
      if Field.getTheTypeInfo in arch.types: # We found a proper field
        indices[found] = arch.typeToIndex[Field.getTheTypeInfo()]
        inc found

  if found == indices.len:
    for i in 0..<arch.len:
      yield arch.generateAccess(i, indices, tup)

macro yieldIteratorImpl(call: untyped, typ: typed): untyped =
  let arg = ident"arg"
  result = nnkForStmt.newTree(arg)
  let yieldedTup = nnkTupleConstr.newTree()

  for i, _ in typ.getTypeInst[^1]:
    yieldedTup.add nnkBracketExpr.newTree(arg, newLit i)

  result.add call
  result.add nnkYieldStmt.newTree(yieldedTup)


template yieldIterator(call: untyped): untyped =
  yieldIteratorImpl(call, typeof(call))

iterator foreach*(archs: openarray[Archetype], tup: typedesc[ComponentTuple], filtered: static bool = true): tup.varTuple =
  when filtered:
    for arch in archs.filter(tup):
      yieldIterator arch.foreach(tup)
  else:
    for arch in archs:
      yieldIterator arch.foreach(tup)

iterator foreach*(world: World, t: typedesc[ComponentTuple]): t.varTuple =
  for arch in world.archetypes:
    yieldIterator arch.foreach(t)

proc isApartOf*[T: ComponentTuple](entity: typedesc[T], arch: Archetype): bool =
  ## Is this component tuple exactly this `arch`
  if tupleLen(T) == arch.typeCount:
    forTuplefields(T):
      if Field.getTheTypeInfo() notin arch.types:
        return false

    result = true

proc getArch*(world: World, T: typedesc[ComponentTuple]): tuple[arch: Archetype, id: int] =
  for id, arch in world.archetypes:
    if T.isApartOf(arch):
      return (arch, id)

proc getArch*(world: World, info: TypeInfos): tuple[arch: Archetype, id: int] =
  for id, arch in world.archetypes:
    if info == arch.types:
      return (arch, id)

proc addEntity*[T: ComponentTuple](world: var World, entity: sink T): Entity {.discardable.} =
  if (let (arch, id) = world.getArch(T); arch != nil):
    arch.add entity
    let ent = Entity(archIndex: id, entIndex: arch.len - 1)
    world.entityPointers.add ent
    return ent

  let newArch = makeArchetype T # No arch found, we need to make a new one then return an ent in it
  newArch.add entity
  let ent = Entity(archIndex: world.archeTypes.len, entIndex: 0)
  world.archetypes.add newArch
  world.entityPointers.add ent
  ent


iterator component*[T: not tuple](world: var World, entity: Entity, _: typedesc[T]): var T =
  ## Return a mutable reference to a component from the entity
  let 
    arch = world.archetypes[entity.archIndex]
    val = default(T)
    typInfo = val.getTheTypeInfo()
    ind = block:
      var val = -1
      for i, typ in arch.types:
        if typ == typInfo:
          val = i
          break
      val
    size = arch.data[ind].len div arch.len
    theAddr = arch.data[ind][size * entity.entIndex].addr
  yield cast[ptr T](theAddr)[]

iterator components*[T: tuple](world: var World, entity: Entity, tup: typedesc[T]): tup.varTuple =
  ## Access the mutagle fields directly on an entity
  var
    indices: array[getRequired(T), int]
    found = 0
    arch = world.archetypes[entity.archIndex]

  forTuplefields(tup.varTuple):
    when Field is Not:
      if Field.getTheTypeInfo() in arch.types[i]:
        found = -1
        break
    else:
      if Field.getTheTypeInfo() in arch.types:
        indices[found] = arch.typeToIndex[Field.getTheTypeInfo()]
        inc found

  if found == indices.len:
    for i in 0..<arch.len:
      yield arch.generateAccess(i, indices, tup.varTuple)

proc addComponent*[T](world: var World, entity: Entity, component: sink T) =
  let fromArch = world.archetypes[entity.archIndex]

  let thisCompTypeInfo = T.getTheTypeInfo
  var neededComponents = fromArch.types

  neededComponents.incl thisCompTypeInfo

  var
    arch: Archetype
    ind = 0

  (arch, ind) = world.getArch(neededComponents)

  if arch.isNil: # We dont have an arch that fits the type we need, make one
    arch = makeArchetype(neededComponents, world.archeTypes[entity.archIndex], component)
    ind = world.archetypes.len
    world.archetypes.add arch

  for i in world.entityPointers.high.countDown(0):
    let entPtr = world.entityPointers[i]
    if entPtr.archIndex == entity.archIndex and entPtr.entIndex > entity.entIndex:
      dec entptr.entIndex

  fromArch.moveEntity(arch, entity.entIndex)
  entity.archIndex = ind
  entity.entIndex = arch.len - 1
  
  const size = sizeof(component)
  let
    dataInd = arch.typeToIndex[thisCompTypeInfo]
    startLen = arch.data[dataInd].len
  arch.data[dataInd].setLen(startLen + size)
  arch.sinkHooks[dataInd](arch.data[dataInd][startLen].addr, component.addr)

proc removeComponent*[T](world: var World, entity: var Entity, comp: typedesc[T]) =
  let fromArch = world.archetypes[entity.archIndex]

  var
    thisCompTypeInfo = comp.getTheTypeInfo
    neededComponents = fromArch.types

  neededComponents.excl thisCompTypeInfo

  if neededComponents.len > 0: # We still have components, we need to move this entity
    var (arch, ind) = world.getArch(neededComponents)

    if arch.isNil: # We dont have an arch that fits the type we need, make one
      arch = fromArch.makeArchetypeRemoving(thisCompTypeInfo)
      ind = world.archetypes.len
      world.archetypes.add arch


    fromArch.moveEntity(arch, entity.entIndex)
    entity.archIndex = ind
    entity.entIndex = arch.len - 1

  else:
    fromArch.removeEntity(entity.entIndex)

iterator query*[T](world: var World, query: var QueryIndex[T]): T.varTuple =
  ## Query using a QueryIndex, this updates the generation pointer and is more efficient than `forEach` directly.
  ## As it only has to query if the pointer has decayed
  if world.archetypes.len != query.generation:
    for i, _ in filterInd(world.archetypes.toOpenArray(query.generation, world.archetypes.high), T):
      query.indices.add i + query.generation
    query.generation = world.archetypes.len

  for queryInd in query.indices:
    yieldIterator world.archetypes[queryInd].foreach(T)

when isMainModule:
  type
    Position = object of RootObj
      x, y, z: float32
    Health = object
      current, max: int32

  proc `=destroy`(h: var Health) =
    if h.current != 0 and h.max != 0:
      echo h

  proc `=copy`(a: var Health, b: Health) {.error.}
  proc `=dup`(a: Health): Health {.error.}
  proc main() =
    var world = World()
    world.addEntity (Position(x: 100, y: 10, z: 10), )
    world.addEntity (Position(x: 1, y: 10, z: 40), Health())
    world.addEntity (Health(current: 20, max: 300), )
    var myent = world.addEntity (Position(x: 1, y: 10, z: 40), )
    world.addComponent(myEnt, Health())


    for props in world.foreach (Position,):
      props[0].x = 300
      echo props[0]

    for (health, pos) in world.foreach (Health, Position):
      pos.x = 300
      health.current = 100
      health.max = 130


    for (health, pos) in world.foreach (Health, Position):
      assert pos == Position(x: 300, y: 10, z: 40)
      assert health == Health(current: 100, max: 130)

    for arch in world.archetypes:
      echo arch

  main()
