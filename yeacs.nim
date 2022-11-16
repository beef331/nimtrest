import std/[typetraits, macros, genasts, strformat, enumerate]

const isLowLevel = not(defined(js) or defined(nimscript))

type
  Component* = object of RootObj
  ComponentTuple* = concept ct, type CT
    onlyUniqueValues(CT)
    fromComponent(CT)

proc onlyUniqueValues*(t: typedesc[tuple]): bool =
  result = true
  for nameA, fieldA in default(t).fieldPairs:
    for nameB, fieldB in default(t).fieldPairs:
      when nameA != nameB and fieldA is typeof(fieldB):
        return false

proc fromComponent*(t: typedesc[tuple]): bool =
  result = true
  for x in default(t).fields:
    if x isnot Component:
      return false

when isLowLevel:
  type TypeInfo = pointer
else:
  type TypeInfo = string

type
  ArchetypeBase* = ref object of RootObj
    typeCount: int
    generation: int
    when isLowLevel:
      componentOffset: seq[int]
      types: seq[TypeInfo]
      stride: int
      data: seq[byte]
    else:
      types: seq[string]
      data: seq[ref Component]


  Archetype*[T: ComponentTuple] = ref object of ArchetypeBase

  World* = object
    archetypes*: seq[ArchetypeBase]

  Entity* = object
    archIndex, entIndex: int
    generation: int # Used to ensure we do not have an outdated pointer

template realCompSize*[T: Component](_: T): int = max(sizeof(T) - sizeof(pointer), 1)


proc makeArchetype*(tup: typedesc[ComponentTuple]): Archetype[tup] =
  const tupLen = tupleLen(tup)
  result =
    when isLowLevel:
      Archetype[tup](
        typeCount: tupLen,
        types: newSeqOfCap[pointer](tupLen),
        componentOffset: newSeqOfCap[int](tupLen)
      )
    else:
      Archetype[tup](
        typeCount: tupLen,
        types: newSeqOfCap[string](tupLen)
      )

  let def = default(tup)
  var offset = 0
  for i, field in enumerate def.fields:
    when isLowLevel:
      result.types.add field.getTypeInfo()
      result.componentOffset.add offset
      offset.inc realCompSize(field)
      result.stride = offset
    else:
      result.types.add $typeof(field)


proc makeArchetype[T: Component](typeInfo: sink seq[TypeInfo], previous: ArchetypeBase, newType: T): ArchetypeBase =
  result =
    when isLowLevel:
      ArchetypeBase(
        typeCount: typeInfo.len,
        types: typeInfo,
        componentOffset: previous.componentOffset,
        stride: previous.stride + realCompSize(newType)
      )
    else:
      ArchetypeBase(
        typeCount: typeInfo.len,
        types: typeInfo
      )
  when isLowLevel:
    result.componentOffset.add realCompSize(newType) + result.componentOffset[^1]

proc len*(arch: ArchetypeBase): int =
  when isLowLevel:
    arch.data.len div arch.stride
  else:
    arch.data.len div arch.typeCount

proc `$`*[T: ComponentTuple](arch: Archetype[T]): string =
  const
    typStr = $T
    tupLen = tupleLen(T)
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


iterator filter*(archetypes: openarray[ArchetypeBase], tup: typedesc[ComponentTuple]): ArchetypeBase =
  var def: tup
  const requiredCount = tupleLen(tup)
  for arch in archetypes:
    if arch.typeCount >= requiredCount:
      var found = 0
      for field in def.fields:
        for i in 0..<arch.typeCount:
          when isLowLevel:
            if arch.types[i] == field.getTypeInfo:
              inc found
          else:
            if arch.types[i] == $typeof(field):
              inc found


      if found == requiredCount:
        yield arch

iterator filter*(archetypes: openarray[ArchetypeBase], typeInfo: openarray[TypeInfo]): (int, ArchetypeBase) =
  for i, arch in archetypes.pairs:
    if arch.typeCount == typeInfo.len:
      var found = 0
      for x in typeInfo:
        for y in arch.types:
          if x == y:
            inc found

      if found == typeInfo.len:
        yield (i, arch)

proc filter*(archetypes: openarray[ArchetypeBase], tup: typedesc[ComponentTuple]): seq[ArchetypeBase] =
  for x in filter(archeTypes, tup):
    result.add x

when isLowLevel:
  proc add*[T](arch: Archetype[T], tup: sink T) =
    for field in tup.fields:
      const realSize =  realCompSize(field)# Dont need type information, but need atleast 1 byte(ugh yes)
      let startLen = arch.data.len
      arch.data.setLen(arch.data.len + realSize)
      let fieldStart = cast[pointer](cast[int](field.addr) + sizeof(pointer))
      arch.data[startLen].addr.copyMem(fieldStart, realSize)
else:
  proc add*[T](arch: Archetype[T], tup: sink T) =
    for field in tup.fields:
      let newComp = new typeof(field)
      newComp[] = field
      arch.data.add newComp

when isLowLevel:
  proc moveEntity(fromArch, toArch: ArchetypeBase, entityId: int) =
    # Need to copy fromArch to toArch then shrink fromArch
    # This is tricky cause component order matters
    let toIndex = toArch.data.len
    toArch.data.setLen(toIndex + toArch.stride)
    let fromIndex = entityId * fromArch.stride
    var moved = 0
    for i, typA in toArch.types:
      block addTyp:
        for j, typB in fromArch.types:
          if typA == typB:
            let
              compSize =
                if j == fromArch.typeCount - 1:
                  fromArch.stride - fromArch.componentOffset[j]
                else:
                  fromArch.componentOffset[j + 1] - fromArch.componentOffset[j]
              fromOffset = fromArch.componentOffset[j]
              toOffset = toArch.componentOffset[i]
            if fromArch.len > 0:
              copyMem(toArch.data[toOffset + toIndex].addr, fromArch.data[fromOffset + fromIndex].addr, compSize)
            inc moved
            break addTyp
    assert moved == fromArch.typeCount

    inc fromArch.generation
    if entityId == fromArch.len - 1:
      fromArch.data.setLen(max(fromArch.data.len - fromArch.stride, 0))
    else:
      if fromIndex > 0 and fromArch.data.len - fromArch.stride > 0:
        let buffer = newSeq[byte](fromArch.data.len - fromArch.stride)
        copyMem(buffer[0].unsafeAddr, fromArch.data[0].unsafeaddr, fromIndex)
        copyMem(buffer[fromIndex].unsafeAddr, fromArch.data[fromIndex + fromArch.stride].unsafeaddr, fromArch.len - 1 - fromIndex - fromArch.stride)
        fromArch.data = buffer
      else:
        fromArch.data.setLen(0)
else:
  proc moveEntity(fromArch, toArch: ArchetypeBase, entityId: int) =
    # Need to copy fromArch to toArch then shrink fromArch
    # This is tricky cause component order matters
    let toIndex = toArch.len
    toArch.data.setLen(toArch.len + toArch.typeCount)
    let fromIndex = entityId * fromArch.typeCount
    var moved = 0
    for i, typA in toArch.types:
      block addTyp:
        for j, typB in fromArch.types:
          if typA == typB:
            toArch.data[toIndex + i] = toArch.data[fromIndex + j]

            inc moved
            break addTyp
    assert moved == fromArch.typeCount

    inc fromArch.generation
    if entityId == fromArch.len:
      fromArch.data.setLen(fromArch.len - fromArch.typeCount)
    else:
      var buffer = newSeqOfCap[(ref Component)](fromArch.len - fromArch.typeCount)
      for i, x in fromArch.data:
        if i notin fromIndex .. fromIndex + fromArch.typecount:
          buffer.add x
      fromArch.data = buffer


when isLowLevel:
  macro generateAccess(arch: ArchetypeBase, ind: int, indArray: array, tup: ComponentTuple): untyped =
    result = nnkTupleConstr.newTree()
    for i, val in tup.getTypeImpl:
      result.add:
        genast(val, ind, indArray, tup, i):
          let element = arch.data[ind * arch.stride + arch.componentOffset[indArray[i]]].addr
          cast[ptr val](cast[int](element) - sizeof(pointer))
else:
  macro generateAccess(arch: ArchetypeBase, ind: int, indArray: array, tup: ComponentTuple): untyped =
    result = nnkTupleConstr.newTree()
    for i, val in tup.getTypeImpl:
      result.add:
        genast(val, ind, indArray, tup, i):
          (ref typeof(tup[i]))(arch.data[indArray[i] + ind * arch.typeCount])

iterator foreach*(arch: ArchetypeBase, tup: typedesc[ComponentTuple]): auto =
  var
    indices: array[tupleLen(tup), int]
    found = 0
    def: tup
  for field in def.fields:
    if found >= indices.len:
      break
    for i in 0..<arch.typeCount:
      template ifMatches() =
        indices[found] = i
        inc found
        if found >= indices.len:
          break

      when isLowLevel:
        if arch.types[i] == field.getTypeInfo():
          ifMatches()
      else:
        if arch.types[i] == $typeof(field):
          ifMatches()

  if found == tupleLen(tup):
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
      when isLowLevel:
        if arch.types[i] != field.getTypeInfo:
          return false
      else:
        if arch.types[i] != $typeof(field):
          return false
    result = true

proc getArch*(world: World, T: typedesc[ComponentTuple]): Archetype[T] =
  let def = default(T)
  for arch in world.archetypes:
    if def.isApartOf(arch):
      return Archetype[T](arch)


proc addEntity*[T: ComponentTuple](world: var World, entity: sink T): Entity {.discardable.} =
  const requiredCount = tupleLen(entity)
  for archId, arch in world.archetypes:
    if entity.isApartOf(arch):
      Archetype[T](arch).add entity
      return Entity(archIndex: archId, entIndex: arch.len - 1, generation: arch.generation)
  let newArch = makeArchetype typeof(entity)
  newArch.add entity
  world.archetypes.add newArch
  result = Entity(archIndex: world.archeTypes.high, entIndex: newArch.len - 1, generation: newArch.generation)


proc addComponent*[T: Component](world: var World, entity: var Entity, component: T) =
  ##assert entity.generation == world.archetypes[entity.archIndex].generation
  let
    thisCompTypeInfo =
      when isLowLevel:
        component.getTypeInfo()
      else:
        $typeof(component)
    neededComponents = block:
      var tmp = world.archetypes[entity.archIndex].types
      tmp.add thisCompTypeInfo
      tmp

  var
    arch: ArchetypeBase
    ind = -1
  for i, filteredArch in world.archetypes.filter(neededComponents):
    arch = filteredArch
    ind = i

  if arch.isNil:
    arch = makeArchetype(neededComponents, world.archeTypes[entity.archIndex], component)
    inc ind

  world.archetypes[entity.archIndex].moveEntity(arch, entity.entIndex)
  entity = Entity(archIndex: ind, entIndex: arch.len - 1, generation: arch.generation)
  let typInfo = neededComponents[^1]
  for i, typ in arch.types: # We need to find where we have to copy this new component
    if typInfo == typ:
      when isLowLevel:
        let realSize = realCompSize(component)
        copyMem(arch.data[entity.entIndex + arch.componentOffset[i]].addr, component.unsafeAddr, realSize)
      else:
        let newComp = new typeof(Component)
        newComp[] = component
        arch.data[entity.entIndex + i] = newComp
      break
  world.archetypes.add arch


when isMainModule:
  type
    Position = object of Component
      x, y, z: float32
    Health = object of Component
      current, max: int32

  var world = World()
  world.addEntity (Position(x: 100, y: 10, z: 10), )
  world.addEntity (Position(x: 1, y: 10, z: 40), Health())
  world.addEntity (Health(current: 20, max: 300), )


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
