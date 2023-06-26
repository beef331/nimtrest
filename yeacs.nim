##[
This is a not a sincere attempt at an ECS, more a sincere attempt at making an ECS without an inane amount of macros.
That means yeacs uses a fair bit of runtime type information.
]##

import std/[typetraits, macros, genasts, strformat, enumerate, tables, hashes]

const isLowLevel = not(defined(js) or defined(nimscript) or defined(useOOP))

type
  ComponentTuple* = concept ct, type CT
    onlyUniqueValues(CT)
  Not*[T] = distinct T

proc onlyUniqueValues*(t: typedesc[tuple]): bool =
  result = true
  let def = default(t)
  for nameA, fieldA in def.fieldPairs:
    for nameB, fieldB in def.fieldPairs:
      when nameA != nameB and fieldA is typeof(fieldB):
        return false

when isLowLevel:
  type TypeInfo = pointer
else:
  type TypeInfo = string

type
  Entity* = ref object
    archIndex, entIndex: int

  ArchetypeBase* = ref object of RootObj
    generation: int # Every time we move an entity or do something to cause an index to be invalidate we increment this, only have 2^64 changes so be careful
    when isLowLevel:
      componentOffset: seq[int] # offset on a component level to access the data stored there
      types: seq[TypeInfo] # For low level we use the nim provided type information to query, this is scraped inside data
      stride: int # the offset between entities
      data: seq[byte] # Sequential storage of components, the RTTI is used to allocate/move these
    else:
      types: seq[TypeInfo] # For highlevel we use the type name for querying, this isnt smart but it works, dont be a tosser and reuse names
      data*: seq[ref Component] # OOP solution for data, works even on JS!


  Archetype*[T: ComponentTuple] = ref object of ArchetypeBase

  World* = object
    archetypes*: seq[ArchetypeBase]
    entityPointers: Table[ArchetypeBase, seq[Entity]]

  QueryIndex*[T: ComponentTuple] = object
    indices: seq[int]
    generation: int

proc `hash`*(arch: ArchetypeBase): Hash = cast[Hash](arch)

template realCompSize*[T](val: T): int =
  when compiles(val of RootObj):
    max(sizeof(T) - sizeof(pointer), 1) # We do not need type information so this is used to remove it from size
  else:
    max(sizeof(T), 1)

template componentAddr*[T](t: T): pointer =
  ## Returns the address to the first field, or nothing if the object is field-less
  var thePtr: pointer
  when compiles((for x in t.fields: discard)):
    for field in t.fields:
      thePtr = field.unsafeaddr
      break
  else:
    thePtr = t.unsafeAddr
  thePtr

proc getRequired(t: typedesc[ComponentTuple]): int =
  var theTup = default(t)
  for field in theTup.fields:
    when field isnot Not:
      inc result

proc typeCount*(archetype: ArchetypeBase): int =
  # Amount of components this archetype has
  archeType.types.len

proc makeArchetype*[T](tup: typedesc[T]): Archetype[tup] =
  ## Generates an archetype based of a tuple.
  const tupLen = getRequired(T)
  result =
    when isLowLevel:
      Archetype[tup](
        types: newSeqOfCap[pointer](tupLen),
        componentOffset: newSeqOfCap[int](tupLen)
      )
    else:
      Archetype[tup](
        types: newSeqOfCap[string](tupLen)
      )

  let def = default(tup)
  var offset = 0
  for i, field in enumerate def.fields:
    result.types.add field.getTheTypeInfo
    when isLowLevel:
      result.componentOffset.add offset
      offset.inc realCompSize(field)
      result.stride = offset


proc makeArchetype[T](typeInfo: sink seq[TypeInfo], previous: ArchetypeBase, newType: T): ArchetypeBase =
  result =
    when isLowLevel:
      ArchetypeBase(
        types: typeInfo,
        componentOffset: previous.componentOffset,
        stride: previous.stride + realCompSize(newType)
      )
    else:
      ArchetypeBase(
        types: typeInfo
      )
  when isLowLevel:
    result.componentOffset.add  previous.stride

proc len*(arch: ArchetypeBase): int =
  ## Amount of entities in this archetype
  when isLowLevel:
    arch.data.len div arch.stride
  else:
    arch.data.len div arch.typeCount

proc `$`*[T: ComponentTuple](arch: Archetype[T]): string =
  const
    typStr = $T
    tupLen = getRequired(T)
  result = fmt"Archetype[{typStr}]("
  let len = arch.len
  for i in 0..<len:
    let def = default T
    for ind, field in enumerate def.fields:
      when isLowLevel:
        let
          startField = arch.data[i * arch.stride + arch.componentOffset[ind]].addr
          data = cast[ptr typeof(field)](cast[int](startField) - sizeof(pointer))
        copyMem(field.unsafeaddr, cast[pointer](cast[int](startField) - sizeof(pointer)), sizeof(field))
        result.add $field
      else:
        result.add $(ref typeof(field))(arch.data[i + ind])[]
      if ind < tupLen - 1:
        result.add ", "
    if i < len - 1:
      result.add ", "
  result.add ")"

proc getTheTypeInfo*(t: auto): TypeInfo =
  when isLowLevel:
    t.getTypeInfo()
  else:
    $typeof(t)

proc getTheTypeInfo*[T](n: Not[T]): TypeInfo =
  var a = default(T)
  getTheTypeInfo(a)

iterator filterInd*[T](archetypes: openarray[ArchetypeBase], tup: typedesc[T]): (int, ArchetypeBase) =
  ## Iterates archetypes yielding all that can be converted to the tuple and the index.
  ## This means they share at least all components of `tup`
  var def: tup
  const requiredCount = getRequired(T)
  for i, arch in archetypes:
    block search:
      if arch.typeCount >= requiredCount:
        var found = 0
        for field in def.fields:
          for i in 0..<arch.typeCount:
            if arch.types[i] == field.getTheTypeInfo:
              when field is Not:
                break search
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

when isLowLevel:
  proc add*[T](arch: Archetype[T], tup: sink T) =
    let startSize = arch.data.len
    arch.data.setLen(arch.data.len + sizeof(tup)) # grow the data slightly so we dont need to allocate per component
    arch.data.setLen(startSize) # decrease size
    for field in tup.fields: # Iterate each component and copy it to `data`
      const realSize = realCompSize(field) # Dont need type information, but need atleast 1 byte(ugh yes)
      let startLen = arch.data.len
      arch.data.setLen(arch.data.len + realSize)

      let compAddr = componentAddr(field)
      if compAddr != nil: # Only copy if we have a field
        arch.data[startLen].addr.copyMem(componentAddr(field), realSize)
else:
  proc add*[T](arch: Archetype[T], tup: sink T) =
    for field in tup.fields: # OOP is simpler we just add it
      let newComp = new typeof(field)
      newComp[] = field
      arch.data.add newComp

when isLowLevel:
  proc removeEntity(arch: ArchetypeBase, entityId: int) =
    # This logic handles moving data around the old buffer
    inc arch.generation # This invalidates any old Entity's, not the smartest but provents errors

    let
      fromIndex = entityId * arch.stride
      newSize = arch.data.len - arch.stride

    if fromIndex > 0:
      if entityId != arch.len - 1:
        moveMem(arch.data[fromIndex].unsafeAddr, arch.data[fromIndex + arch.stride].unsafeaddr, arch.data.len - fromIndex - arch.stride)
    elif arch.len > 1:
      moveMem(arch.data[0].unsafeAddr, arch.data[arch.stride].unsafeaddr, newSize) # copy from 0..movingEntity
    arch.data.setLen(newSize)

  proc moveEntity(fromArch, toArch: ArchetypeBase, entityId: int) =
    # Need to copy fromArch to toArch then shrink fromArch
    # This is tricky cause component order matters
    let toIndex = toArch.data.len
    toArch.data.setLen(toIndex + toArch.stride)
    let fromIndex = entityId * fromArch.stride
    var moved = 0
    for i, typA in toArch.types:
      for j, typB in fromArch.types:
        if typA == typB: # found a type we had that we need to copy over
          let
            compSize =
              if j == fromArch.types.high:
                fromArch.stride - fromArch.componentOffset[j]
              else:
                fromArch.componentOffset[j + 1] - fromArch.componentOffset[j]
            srcIndex = fromArch.componentOffset[j] + fromIndex
            destIndex = toArch.componentOffset[i] + toIndex

          moveMem(toArch.data[destIndex].addr, fromArch.data[srcIndex].addr, compSize)
          inc moved
          break

      if moved == fromArch.typeCount:
        break # Object fully moved we can exit the loop



    fromArch.removeEntity(entityId)

else:
  proc removeEntity(arch: ArchetypeBase, entityId: int) =
    # This logic handles moving data around the old buffer
    inc arch.generation
    let fromIndex = entityId * arch.typeCount

    for i in fromIndex .. arch.data.high - arch.typeCount:
      arch.data[i] = arch.data[i + arch.typeCount]
    arch.data.setLen(arch.data.len - arch.typeCount)

  proc moveEntity(fromArch, toArch: ArchetypeBase, entityId: int) =
    # Need to copy fromArch to toArch then shrink fromArch
    # This is tricky cause component order matters
    let toIndex = toArch.data.len
    toArch.data.setLen(toIndex + toArch.typeCount)
    let fromIndex = entityId * fromArch.typeCount
    var moved = 0

    for i, typA in toArch.types:
      block addTyp:
        for j, typB in fromArch.types:
          if typA == typB:
            toArch.data[toIndex + i] = fromArch.data[fromIndex + j]
            inc moved
            break addTyp

      if moved == fromArch.typeCount:
        break

    assert moved == fromArch.typeCount

    inc fromArch.generation

    for i in fromIndex .. fromArch.data.high - fromArch.typeCount:
      fromArch.data[i] = fromArch.data[i + fromArch.typeCount]
    fromArch.data.setLen(fromArch.data.len - fromArch.typeCount)


when isLowLevel:
  macro generateAccess(arch: ArchetypeBase, ind: int, indArray: array, tup: ComponentTuple): untyped =
    result = nnkTupleConstr.newTree()
    for i, val in tup.getTypeImpl:
      if val.kind != nnkBracketExpr or (val.kind == nnkBracketExpr and val[0] != bindSym"Not"):
        result.add:
          genast(val, ind, indArray, tup, i):
            let element = arch.data[ind * arch.stride + arch.componentOffset[indArray[i]]].addr
            when compiles(default(val) of RootObj):
              cast[ptr val](cast[int](element) - sizeof(pointer))
            else:
              cast[ptr val](element)

else:
  macro generateAccess(arch: ArchetypeBase, ind: int, indArray: array, tup: ComponentTuple): untyped =
    result = nnkTupleConstr.newTree()
    for i, val in tup.getTypeImpl:
      if val.kind != nnkBracketExpr or (val.kind == nnkBracketExpr and val[0] != bindSym"Not"):
        result.add:
          genast(val, ind, indArray, tup, i):
            (ref val)(arch.data[ind * arch.typeCount + indArray[i]])

iterator foreach*[T](arch: ArchetypeBase, tup: typedesc[T]): auto =
  var
    indices: array[getRequired(T), int]
    found = 0
    def: tup

  for field in def.fields:

    if found >= indices.len:
      break

    for i in 0..<arch.typeCount:
      when field is Not:
        if arch.types[i] == field.getTheTypeInfo:
          found = 0
          break
      else:
        if arch.types[i] == field.getTheTypeInfo:
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

proc isApartOf*[T: ComponentTuple](entity: T, arch: ArchetypeBase): bool =
  if tupleLen(T) == arch.typeCount:
    for i, field in enumerate entity.fields:
      if arch.types[i] != field.getTheTypeInfo():
        return false

    result = true

proc getArch*(world: World, T: typedesc[ComponentTuple]): Archetype[T] =
  let def = default(T)
  for arch in world.archetypes:
    if def.isApartOf(arch):
      return Archetype[T](arch)


proc addEntity*[T: ComponentTuple](world: var World, entity: sink T): Entity {.discardable.} =
  const requiredCount = getRequired(T)
  for archId, arch in world.archetypes:
    if entity.isApartOf(arch):
      Archetype[T](arch).add entity
      let ent = Entity(archIndex: archId, entIndex: arch.len - 1)
      world.entityPointers[arch].add ent
      return ent
  let newArch = makeArchetype typeof(entity)
  newArch.add entity
  world.archetypes.add newArch
  let ent = Entity(archIndex: world.archeTypes.high, entIndex: newArch.len - 1)
  world.entityPointers[ArchetypeBase newArch] = @[ent]
  ent


proc addComponent*[T](world: var World, entity: Entity, component: T) =
  let fromArch = world.archetypes[entity.archIndex]

  var
    thisCompTypeInfo = component.getTheTypeInfo
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
      when isLowLevel: # We need to copy the new component over to the type, the others already moved in `moveEntity`
        let compAddr = componentAddr(component)
        if compAddr != nil: # Fieldless objects are 1 bytes
          copyMem(arch.data[entity.entIndex * arch.stride + arch.componentOffset[i]].addr, compAddr, realCompSize(component))

      else:
        let newComp = (ref T)()
        newComp[] = component
        arch.data[entity.entIndex * arch.typeCount + i] = newComp
      break

  if not hasOldArch:
    world.archetypes.add arch

proc removeComponent*[T](world: var World, entity: var Entity, comp: typedesc[T]) =
  let fromArch = world.archetypes[entity.archIndex]

  var
    component = default(comp)
    thisCompTypeInfo = component.getTheTypeInfo
    neededComponents = fromArch.types

  if (let foundIndex = neededComponents.find(thisCompTypeInfo); foundIndex > 0):
    neededComponents.delete(foundIndex)

  if neededComponents.len > 0:
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
  if world.archetypes.len != query.generation:
    for i, _ in filterInd(world.archetypes.toOpenArray(query.generation, world.archetypes.high), T):
      query.indices.add i + query.generation
    query.generation = world.archetypes.len

  for queryInd in query.indices:
    for ent in world.archetypes[queryInd].foreach(T):
      yield ent




when isMainModule:
  type
    Position = object
      x, y, z: float32
    Health = object
      current, max: int32

  var world = World()
  world.addEntity (Position(x: 100, y: 10, z: 10), )
  world.addEntity (Position(x: 1, y: 10, z: 40), Health())
  world.addEntity (Health(current: 20, max: 300), )
  var myent = world.addEntity (Position(x: 1, y: 10, z: 40), )
  world.addComponent(myEnt, Health())


  for (pos,) in world.foreach (Position,):
    pos.x = 300
    echo pos[]

  for (health, pos) in world.foreach (Health, Position):
    pos.x = 300
    health.current = 100
    health.max = 130


  for (health, pos) in world.foreach (Health, Position):
    assert pos[] == Position(x: 300, y: 10, z: 40)
    assert health[] == Health(current: 100, max: 130)

  echo world.getArch (Position, )
  echo world.getArch (Position, Health)
  echo world.getArch (Health, )
