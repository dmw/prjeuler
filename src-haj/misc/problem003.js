N = 600851475143

function isPrime(n) {

  r = Math.ceil(Math.sqrt(n)) + 1

  for(var i = 2; i < r; i++) {

    if(n % i == 0) {
      //console.log(' ' + n + ' not prime, breaks at ' + i)
      return false
    }
  }

  return true
}

function findLargestPrimeDivisor(n) {

  for (var i = 2; i < n; i++) {

    if (n % i == 0) {
      
      //console.log('found ' + i + ' ' + (n/i))

      if(isPrime(n/i)) {
        
        return ( n / i)
        process.exit(0)
      }
    } 
  }

  return n
}

var start_time = Date.now()
var result = findLargestPrimeDivisor(N)
var diff = (Date.now() - start_time) / 1000
console.log('Found ' + result + ' & took ' + diff + ' seconds')

// Find All Divisors

function findPrimeDivisors(n) {

  var divisors = []
  var still_divisors = true  

  do {

    divisor = findLargestPrimeDivisor(n)

    if (divisor != n) {
     
      n = n / divisor      
    }
    else {
      still_divisors = false
    }

    divisors.push(divisor)

  } while (still_divisors)

  return divisors
}

var start_time = Date.now()
var result = findPrimeDivisors(N)
var diff = (Date.now() - start_time) / 1000
console.log('Found ' + result + ' & took ' + diff + ' seconds')