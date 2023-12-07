import std/[os, parseutils, algorithm]


type 
  CardKind = enum
    Joker
    Two
    Three
    Four
    Five
    Six
    Seven
    Eight
    Nine
    Ten
    Jack
    Queen
    King
    Ace

  HandKind = enum
    HighCard
    OnePair
    TwoPair
    ThreeOfAKind
    FullHouse
    FourOfAKind
    FiveOfAKind

  Play = object
    cards: array[5, CardKind]
    bet: int
    handKind: HandKind

proc classify(count: array[CardKind, uint8]): HandKind =
  var 
    cardsInHand: set[CardKind]
    largestCount: uint8

  for card in CardKind:
    if count[card] > 0:
      cardsInHand.incl card
      largestCount = max(largestCount, count[card])

  let 
    uniqueCards = cardsInHand.len
    isJoking = count[Joker] > 0 

  case range[1..5](uniqueCards)
  of 5:
    if isJoking:
      OnePair
    else:
      HighCard
  of 4:
    if isJoking:
      ThreeOfAKind
    else:
      OnePair
  of 3:
    if largestCount == 3:
      if isJoking:
        FourOfAKind
      else:
        ThreeOfAKind
    else:
      case range[0..2](count[Joker])
      of 1:
        FullHouse
      of 2:
        FourOfAKind
      of 0:
        TwoPair
  of 2:
    if isJoking:
      FiveOfAKind
    elif largestCount == 4:
      FourOfAKind
    else:
      FullHouse
  of 1:
    FiveOfAKind

proc toCard(c: char): CardKind =
  case c
  of 'A':
    Ace
  of 'K':
    King
  of 'Q':
    Queen
  of 'J':
    Jack
  of 'T':
    Ten
  of '2'..'9':
    CardKind(Two.ord + (c.ord - '2'.ord))
  else:
    raiseAssert("Incorrect Conversion")

proc cmp(a, b: Play): int =
  if a.handKind > b.handKind:
    1
  elif a.handKind < b.handKind:
    -1
  else:
    for i, aCard in a.cards:
      if aCard > b.cards[i]:
        return 1
      if aCard < b.cards[i]:
        return -1
    0

proc solveIt(file: string): (int, int) =
  var hands, hands2 = newSeqOfCap[Play](128)
  for line in lines file:
    var 
      play = Play()
      count: array[CardKind, uint8]

    for i in 0..4:
      let card = line[i].toCard
      play.cards[i] = card 
      inc count[card]

    play.handKind = count.classify()
    discard line.toOpenArray(6, line.high).parseInt(play.bet)

    hands.add play
    hands2.add play

    for card in hands2[^1].cards.mitems:
      if card == Jack:
        card = Joker
    
    swap count[Joker], count[Jack]

    hands2[^1].handKind = count.classify()

  hands.sort(day7.cmp)
  hands2.sort(day7.cmp)

  for i, hand in hands:
    result[0] += (i + 1) * hand.bet
    result[1] += (i + 1) * hands2[i].bet

echo solveIt(paramStr(1))
