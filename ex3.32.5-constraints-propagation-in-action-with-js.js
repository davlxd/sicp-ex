// To try to apply constraints propagation in JS with simple callbacks and function calls
//




// First we need building blocks, nondirectional computation functions, a.k.a. constraints

const isConstant = x => typeof x === 'number'
const isReceiver = x => typeof x === 'function'

let adder = (a, b, c) => {
  if (isConstant(a) && isConstant(b) && isReceiver(c)) {
    c(a + b)
  }

  if (isConstant(b) && isConstant(c) && isReceiver(a)) {
    a(c - b)
  }

  if (isConstant(c) && isConstant(a) && isReceiver(b)) {
    b(c - a)
  }
}

adder(3, 4, sum => console.log(sum))
adder(3, b => console.log(b), 6)

let multiplier = (a, b, c) => {
  if (isConstant(a) && isConstant(b) && isReceiver(c)) {
    c(a * b)
  }

  if (isConstant(b) && isConstant(c) && isReceiver(a)) {
    a(c / b)
  }

  if (isConstant(c) && isConstant(a) && isReceiver(b)) {
    b(c / a)
  }
}

multiplier(3, 4, product => console.log(product))
multiplier(3, b => console.log(b), 6)






// Next, take 4F = 6C for a simplied instance

let converter = (F, C) => {
  const conduit = {} // conduit is the key here, based on above implemntation, conduit is a function whose param is result
  multiplier(3, F, conduit)
  multiplier(4, C, conduit) // but in here, it needs to provide value like constant
}


// So redesign the building block:

const isConduit = x => typeof x === 'object'
const conduitHasValue = x => x.value != null
const conduitHasNoValue = x => x.value == null

multiplier = (a, b, c) => {
  if (isConduit(a) && conduitHasValue(a)) {
    a = a.value
  }
  if (isConduit(b) && conduitHasValue(b)) {
    b = b.value
  }
  if (isConduit(c) && conduitHasValue(c)) {
    c = c.value
  }

  if (isConstant(a) && isConstant(b) && isConduit(c) && conduitHasNoValue(c)) {
    c.value = (a * b)
  }

  if (isConstant(b) && isConstant(c) && isConduit(a) && conduitHasNoValue(a)) {
    a.value = (c / b)
  }

  if (isConstant(c) && isConstant(a) && isConduit(b) && conduitHasNoValue(b)) {
    b.value = (c / a)
  }
}

let receiver = {}
multiplier(3, 4, receiver)
console.log(receiver)

receiver = {}
multiplier(3, receiver, 6)
console.log(receiver)

receiver = {}
multiplier(receiver, 2, { value: 10 })
console.log(receiver)



// Now forward direction can work
receiver = {}
converter(4, receiver)
console.log(receiver)



// But not backword direction
receiver = {}
converter(receiver, 10)
console.log(receiver)

// Because the first multiplier sentence inside converter won't wait for 2nd to finish
// We need to apply lazy eval






// Redesign again

multiplier = (a, b, c) => {
  if (isConduit(a) && conduitHasValue(a)) {
    a = a.value()
  }
  if (isConduit(b) && conduitHasValue(b)) {
    b = b.value()
  }
  if (isConduit(c) && conduitHasValue(c)) {
    c = c.value()
  }

  if (isConstant(a) && isConstant(b) && isConduit(c) && conduitHasNoValue(c)) {
    c.value = () => (a * b)
  }

  if (isConstant(b) && isConstant(c) && isConduit(a) && conduitHasNoValue(a)) {
    a.value = () => (c / b)
  }

  if (isConstant(c) && isConstant(a) && isConduit(b) && conduitHasNoValue(b)) {
    b.value = () => (c / a)
  }
}

receiver = {}
multiplier(3, 4, receiver)
console.log(receiver.value())

receiver = {}
multiplier(3, receiver, 6)
console.log(receiver.value())

receiver = {}
multiplier(receiver, 2, { value: () => 10 })
console.log(receiver.value())

converter = (F, C) => {
  const conduit = {}
  multiplier(3, F, conduit)
  multiplier(4, C, conduit)
}

receiver = {}
converter(4, receiver)
console.log(receiver.value())


receiver = {}
converter(receiver, 10)
console.log(receiver)

// Still cannot work because first multiplier cannot differentiate receiver and conduit
// We need to know which conduit to acquire data, which conduit to feed data, and that basically is propagation
// We can cheat here by set flags with receiver to differentiate, but problem may arise when we have
// constraints in the middle
// We can propagate this receiver flag, but when for backword direction computation, 
// this flag cannot propagate backword because inside converter the code get executed in order
// So it's bound to have one set code to construct the topology, another set of code trigger propagation
// So state become a necessity
//
// END - a failure



