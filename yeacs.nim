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

type
  ArchetypeBase* = ref object of RootObj
    typeCount: int
    when isLowLevel:
      componentOffset: seq[int]
      types: seq[pointer]
      stride: int
      data: seq[byte]
    else:
      types: seq[string]
      data: seq[ref Component]


  Archetype*[T: ComponentTuple] = ref object of ArchetypeBase


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
      offset.inc sizeof(field) - sizeof(pointer)
      result.stride = offset
    else:
      result.types.add $typeof(field)

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

proc filter*(archetypes: openarray[ArchetypeBase], tup: typedesc[ComponentTuple]): seq[ArchetypeBase] =
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
        result.add arch

when isLowLevel:
  proc add*[T](arch: Archetype[T], tup: sink T) =
    for field in tup.fields:
      const realSize = sizeof(field) - sizeof(pointer) # Dont need type information
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

  assert found == tupleLen(tup)
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

when isMainModule:
  type
    Position = object of Component
      x, y, z: float32
    Health = object of Component
      current, max: int32

  var
    posHealth = makeArchetype((Position, Health))
    pos = makeArchetype((Position,))
    health = makeArchetype((Health,))

  pos.add (Position(x: 100, y: 10, z: 10), )
  posHealth.add (Position(x: 1, y: 10, z: 40), Health())
  health.add (Health(current: 20, max: 300), )


  var myArchs = [ArchetypeBase pos, posHealth, health]

  for (pos,) in myArchs.foreach (Position,):
    pos.x = 300
    echo pos[]

  for (health, pos) in myArchs.foreach (Health, Position):
    pos.x = 300
    health.current = 100
    health.max = 130


  for (health, pos) in posHealth.foreach (Health, Position):
    assert pos[] == Position(x: 300, y: 10, z: 40)
    assert health[] == Health(current: 100, max: 130)

  echo $pos
  echo posHealth
  echo health

