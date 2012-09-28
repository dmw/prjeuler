
/*
 *
 *  Took 1644.339 seconds (27.4 minutes) in my MacBook Pro
 *  A 2.4 Ghz Intel Core i5 with 8GB RAM
 *  OS X 10.8.2
 *
 *
 */

var start = Date.now()

mainLoop();

function mainLoop () {
  for(var i = 0; i < 100000000000; i++) {
    var s = (i * (i + 1)) / 2;
    var divs = countDivisors(s);
    console.log(i + '\t' + s + '\t' + divs);
    if (divs > 500) end();
  }
}

function countDivisors (number) {
  var counter = 0;
  for (var i = number - 1; i > 0; i--) {
    if ((number % i) === 0 ) counter ++;
  }
  return counter;
}

function end() {
  var diff = Date.now() - start;
  console.log(diff)
  process.exit(0);
}
