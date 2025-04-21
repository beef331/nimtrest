##[
This is a not a sincere attempt at an ECS, more a sincere attempt at making an ECS without an inane amount of macros.
That means yeacs uses a fair bit of runtime type information.
]##

import std/[typetraits, macros, genasts, strformat, enumerate, tables, hashes, sequtils]

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

type TypeInfo = int

type
  Entity* = ref object
    archIndex, entIndex: int

  ArchetypeBase* = ref object of RootObj
    generation: int # Every time we move an entity or do something to cause an index to be invalidate we increment this, only have 2^64 changes so be careful
    types: seq[TypeInfo] # For low level we use the nim provided type information to query, this is scraped inside data
    data: seq[seq[byte]] # type erased data collections, the RTTI is used to allocate/move these
    sinkHooks: seq[proc(a, b: pointer){.nimcall.}]
    destroyHooks: seq[proc(a: pointer){.nimcall.}]
    len: int

  Archetype*[T: ComponentTuple] = ref object of ArchetypeBase

  World* = object
    archetypes*: seq[ArchetypeBase]
    entityPointers: Table[ArchetypeBase, seq[Entity]]

  QueryIndex*[T: ComponentTuple] = object
    indices: seq[int]
    generation: int

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
        type Field{.inject.} = x
    result.add nnkIfStmt.newTree(nnkElifBranch.newTree(newLit(true), body))
  result = nnkBlockStmt.newTree(newEmptyNode(), result)

proc `=destroy`(arch: typeof(ArchetypeBase()[])) =
  if arch.len > 0:
    for i, coll in arch.data:
      let size = arch.data.len div arch.len
      for j in 0..<arch.len:
        arch.destroyHooks[i](coll[j * size].addr)
  `=destroy`(arch.types)
  `=destroy`(arch.data)
  `=destroy`(arch.sinkHooks)
  `=destroy`(arch.destroyHooks)


proc `hash`*(arch: ArchetypeBase): Hash = cast[Hash](arch)

proc getRequired(t: typedesc[ComponentTuple]): int =
  ## Returns required component count
  ## This is the count minus the `Not` count
  forTuplefields(t):
    when Field isnot Not:
      inc result

proc typeCount*(archetype: ArchetypeBase): int =
  # Amount of components this archetype has
  archeType.types.len

proc makeArchetype*[T](tup: typedesc[T]): Archetype[tup] =
  ## Generates an archetype based of a tuple.
  const tupLen = getRequired(T)
  result =
    Archetype[tup](
      types: newSeqOfCap[TypeInfo](tupLen),
      data: newSeq[seq[byte]](tupLen)
    )

  forTuplefields(T):
    result.types.add Field.getTheTypeInfo()
    result.sinkHooks.add proc(a, b: pointer) =
      let
        a = cast[ptr Field](a)
        b = cast[ptr Field](b)
      `=sink`(a[], b[])

    result.destroyHooks.add proc(a: pointer) =
      let a = cast[ptr Field](a)
      `=destroy`(a[])


proc makeArchetype[T](typeInfo: sink seq[TypeInfo], previous: ArchetypeBase, newType: T): ArchetypeBase =
  result =
    ArchetypeBase(
      types: typeInfo,
      data: newSeq[seq[byte]](previous.data.len + 1),
      sinkHooks: previous.sinkHooks,
      destroyHooks: previous.destroyHooks
    )
  result.sinkHooks.add proc(a, b: pointer) =
    let
      a = cast[ptr T](a)
      b = cast[ptr T](b)
    `=sink`(a[], b[])

  result.destroyHooks.add proc(a: pointer) =
    let a = cast[ptr T](a)
    `=destroy`(a[])


proc `$`*[T: ComponentTuple](arch: Archetype[T]): string =
  const
    typStr = $T
    tupLen = getRequired(T)
  result = fmt"Archetype[{typStr}]("
  let len = arch.len
  for i in 0..<len:
    var ind = 0
    forTuplefields(T):
      var field: Field
      copyMem(field.addr, arch.data[ind][i * Field.sizeof()].addr, sizeof(field))
      result.add $field
      if ind < tupLen - 1:
        result.add ", "
      inc ind

    if i < len - 1:
      result.add ", "

  result.add ")"


proc getTheTypeInfo*[T](t: typedesc[Not[T] | T]): TypeInfo =
  result = static: hash(T.getTypeInst.signatureHash()).int

iterator filterInd*[T](archetypes: openarray[ArchetypeBase], tup: typedesc[T]): (int, ArchetypeBase) =
  ## Iterates archetypes yielding all that can be converted to the tuple and the index.
  ## This means they share at least all components of `tup`
  const requiredCount = getRequired(T)
  for i, arch in archetypes:
    block search:
      if arch.typeCount >= requiredCount:
        var found = 0
        forTuplefields(tup):
          for i in 0..<arch.typeCount:
            if arch.types[i] == Field.getTheTypeInfo:
              when Field is Not:
                found = 0
                break search
              else:
                inc found

        if found == requiredCount:
          yield (i, arch)

iterator filter*(archetypes: openarray[ArchetypeBase], tup: typedesc[ComponentTuple]): ArchetypeBase =
  ## Iterates archetypes yielding all that can be converted to the tuple.
  ## This means they share at least all components of `tup`
  for _, arch in archetypes.filterInd(tup):
    yield arch



iterator filter*(archetypes: openarray[ArchetypeBase], typeInfo: openarray[TypeInfo]): (int, ArchetypeBase) =
  ## Iterates archetypes yielding the position an archetype appears and the archetype that can be converted to the tuple.
  ## This means they share at least all components of `tup`.
  for i, arch in archetypes.pairs:
    if arch.typeCount == typeInfo.len:
      var found = 0
      for x in typeInfo:
        block searchForTyp:
          for y in arch.types:
            if x == y:
              inc found
              break searchForTyp # We found the type break out to reduce iterations

      if found == typeInfo.len:
        yield (i, arch)

proc filter*(archetypes: openarray[ArchetypeBase], tup: typedesc[ComponentTuple]): seq[ArchetypeBase] =
  for x in filter(archeTypes, tup):
    result.add x

proc add*[T](arch: Archetype[T], tup: T) =
  for field in tup.fields: # Iterate each component and copy it to `data`
    let ind = block:
      var val = -1
      for i, typInfo in arch.types:
        if typInfo == field.typeof.getTheTypeInfo():
          val = i
      val
    assert ind >= 0

    let startLen = arch.data[ind].len
    arch.data[ind].setLen(arch.data[ind].len + sizeof(field))
    copyMem(arch.data[ind][startLen].addr, field.addr, sizeof(field))
  inc arch.len


proc removeEntity(arch: ArchetypeBase, entityId: int) =
  # This logic handles moving data around the old buffer
  inc arch.generation
  for comp in arch.data.mitems:
    if comp.len > 0:
      let size = comp.len div arch.len
      comp.delete(size * entityId .. size * (entityId + 1) - 1)
  dec arch.len

proc moveEntity(fromArch, toArch: ArchetypeBase, entityId: int) =
  var moved = 0
  for i, typA in toArch.types:
    for j, typB in fromArch.types:
      if typA == typB: # found a type we had that we need to copy over
        let size = fromArch.data[j].len div fromArch.len
        toArch.data[i].add fromArch.data[j].toOpenArray(entityId * size, (entityId + 1) * size - 1)
        inc moved
        break

    if moved == fromArch.typeCount:
      break # Object fully moved we can exit the loop

  assert moved == min(fromArch.typeCount, toArch.typeCount())

  inc toArch.len
  fromArch.removeEntity(entityId)

macro generateAccess(arch: ArchetypeBase, ind: int, indArray: array, tup: ComponentTuple): untyped =
  result = nnkTupleConstr.newTree()
  for i, val in tup.getTypeImpl:
    if val.kind != nnkBracketExpr or (val.kind == nnkBracketExpr and val[0] != bindSym"Not"):
      result.add:
        genast(val, ind, indArray, tup, i):
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

iterator foreach*[T](arch: ArchetypeBase, tup: typedesc[T]): tup.varTuple =
  var
    indices: array[getRequired(T), int]
    found = 0
    def: tup

  for field in def.fields: # Search for the indices in this object
    block searching:
      for i in 0..<arch.typeCount:
        when field is Not:
          if arch.types[i] == field.typeof.getTheTypeInfo: # We hit on a `Not` exiting
            found = -1
            break searching
        else:
          if arch.types[i] == field.typeof.getTheTypeInfo: # We found a proper field
            indices[found] = i
            inc found
            if found >= indices.len:
              break

  if found == indices.len:
    for i in 0..<arch.len:
      yield arch.generateAccess(i, indices, def)


iterator foreach*(archs: openarray[ArchetypeBase], tup: typedesc[ComponentTuple], filtered: static bool = true): auto =
  when filtered:
    for arch in archs.filter(tup):
      for ent in arch.foreach(tup):
        yield ent
  else:
    for arch in archs:
      for ent in arch.foreach(tup):
        yield ent

iterator foreach*(world: World, t: typedesc[ComponentTuple]): auto =
  for arch in world.archetypes:
    for ent in arch.foreach(t):
      yield ent

proc isApartOf*[T: ComponentTuple](entity: typedesc[T], arch: ArchetypeBase): bool =
  ## Is this component tuple exactly this `arch`
  if tupleLen(T) == arch.typeCount:
    var i = 0
    forTuplefields(T):
      if arch.types[i] != Field.getTheTypeInfo():
        return false
      inc i

    result = true

proc getArch*(world: World, T: typedesc[ComponentTuple]): Archetype[T] =
  for arch in world.archetypes:
    if T.isApartOf(arch):
      return Archetype[T](arch)

proc addEntity*[T: ComponentTuple](world: var World, entity: sink T): Entity {.discardable.} =
  for archId, arch in world.archetypes:
    if T.isApartOf(arch): # We found an arch, we can just add an entity and return it
      Archetype[T](arch).add entity
      let ent = Entity(archIndex: archId, entIndex: arch.len - 1)
      world.entityPointers[arch].add ent
      return ent

  let newArch = makeArchetype T # No arch found, we need to make a new one then return an ent in it
  newArch.add entity
  world.archetypes.add newArch
  let ent = Entity(archIndex: world.archeTypes.high, entIndex: newArch.len - 1)
  world.entityPointers[ArchetypeBase newArch] = @[ent]
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
    def: T
    arch = world.archetypes[entity.archIndex]

  for field in def.fields: # Our tuples our order invariant, we need to find the (int, float) -> (float, int) transformation

    if found >= indices.len:
      break

    for i in 0..<arch.typeCount:
      when field is Not:
        if arch.types[i] == field.typeof.getTheTypeInfo:
          found = -1
          break
      else:
        if arch.types[i] == field.typeof.getTheTypeInfo:
          indices[found] = i
          inc found
          if found >= indices.len:
            break

  if found == indices.len:
    for i in 0..<arch.len:
      yield arch.generateAccess(i, indices, def)

proc addComponent*[T](world: var World, entity: Entity, component: sink T) =
  let fromArch = world.archetypes[entity.archIndex]

  var
    thisCompTypeInfo = T.getTheTypeInfo
    neededComponents = fromArch.types

  if thisCompTypeInfo notin neededComponents: # Dont add more fields than required
    neededComponents.add thisCompTypeInfo

  var
    arch: ArchetypeBase
    ind = 0
    hasOldArch = false

  for i, filteredArch in world.archetypes.filter(neededComponents):
    arch = filteredArch
    hasOldArch = true
    ind = i

  if arch.isNil: # We dont have an arch that fits the type we need, make one
    arch = makeArchetype(neededComponents, world.archeTypes[entity.archIndex], component)
    ind = world.archetypes.len
  else:
    if arch in world.entityPointers:
      for ent in world.entityPointers[arch]:
        if ent.entIndex > entity.entIndex:
          dec ent.entIndex

  fromArch.moveEntity(arch, entity.entIndex)
  entity.archIndex = ind
  entity.entIndex = arch.len - 1
  
  for i, typ in arch.types: # We need to find where we have to copy this new component
    if thisCompTypeInfo == typ:
      const size = sizeof(component)
      let startLen = arch.data[i].len
      arch.data[i].setLen(startLen + size)
      copyMem(arch.data[i][startLen].addr, component.addr, size)
      break

  if not hasOldArch:
    world.archetypes.add arch

proc removeComponent*[T](world: var World, entity: var Entity, comp: typedesc[T]) =
  let fromArch = world.archetypes[entity.archIndex]

  var
    component = default(comp)
    thisCompTypeInfo = comp.getTheTypeInfo
    neededComponents = fromArch.types

  if (let foundIndex = neededComponents.find(thisCompTypeInfo); foundIndex > 0):
    neededComponents.delete(foundIndex)

  if neededComponents.len > 0: # We still have components, we need to move this entity
    var
      arch: ArchetypeBase
      ind = 0
      hasOldArch = false

    for i, filteredArch in world.archetypes.filter(neededComponents):
      arch = filteredArch
      hasOldArch = true
      ind = i

    if arch.isNil: # We dont have an arch that fits the type we need, make one
      arch = makeArchetype(neededComponents, world.archeTypes[entity.archIndex], component)
      ind = world.archetypes.len


    fromArch.moveEntity(arch, entity.entIndex)
    entity.archIndex = ind
    entity.entIndex = arch.len - 1

    if not hasOldArch:
      world.archetypes.add arch

  else:
    fromArch.removeEntity(entity.entIndex)

iterator query*[T](world: var World, query: var QueryIndex[T]): auto =
  ## Query using a QueryIndex, this updates the generation pointer and is more efficient than `forEach` directly.
  ## As it only has to query if the pointer has decayed
  if world.archetypes.len != query.generation:
    for i, _ in filterInd(world.archetypes.toOpenArray(query.generation, world.archetypes.high), T):
      query.indices.add i + query.generation
    query.generation = world.archetypes.len

  for queryInd in query.indices:
    for ent in world.archetypes[queryInd].foreach(T):
      yield ent

when isMainModule:
  type
    Position = object of RootObj
      x, y, z: float32
    Health = object of RootObj
      current, max: int32

  proc `=destroy`(h: Health) =
    if h.current != 0 and h.max != 0:
      echo h

  proc `=copy`(_: var Health, _: Health) {.error.}
  proc `=dup`(_: Health): Health {.error.}

  var world = World()
  world.addEntity (Position(x: 100, y: 10, z: 10), )
  world.addEntity (Position(x: 1, y: 10, z: 40), Health())
  world.addEntity (Health(current: 20, max: 300), )
  var myent = world.addEntity (Position(x: 1, y: 10, z: 40), )
  world.addComponent(myEnt, Health())


  for (pos,) in world.foreach (Position,):
    pos.x = 300
    echo pos

  for (health, pos) in world.foreach (Health, Position):
    pos.x = 300
    health.current = 100
    health.max = 130


  for (health, pos) in world.foreach (Health, Position):
    assert pos == Position(x: 300, y: 10, z: 40)
    assert health == Health(current: 100, max: 130)

  echo $world.getArch (Position, )
  echo world.getArch (Position, Health)
  echo world.getArch (Health, )
