#CipherSolver
Abhinav Mehta, 
Akshay Singhal,
Pankaj Gupta

#Description
This module serves in encrypting and decrypting english text via many cipher algorithms like Substitution, caeserian shift, RSA etc. We are also trying on adding cipher cracking to module by using various machine learning and AI techniques.

#Usage
For Substitution(both encode and decode)
```Haskell
import CipherSolver

main::IO()
main = do
  let message = "something written here"
  let key = "zxcvbnmasdfghjklqwertyuiop"
  let cipher = substitutionEncode key message
```

For frequency analysis of substitution cipher
```Haskell
let cipher = "something written here"
let letterfreq = freqtuple cipher
let wsorted = sortedwords cipher
let digrams = digramslist cipher
```

For Caeser cipher
  encoding:
  ```Haskell
  let cipher = caeserEncode key message
  ```
  
  decoding:
  ```Haskell
  let message = caeserDecode key cipher
  ```
  
  auto decrypt:
  ```Haskell
  let message = solveCaeser bigdata (words cipher)
  ```
  where bigdata represents dictionary words. you can read that from our dictionary folder.
  
For Affine cipher
  encoding:
  ```Haskell
  let cipher = affineEncode a b message
  ```
  
  decoding
  ```Haskell
  let message = affineDecode a b cipher
  ```
  
For Atbash cipher
  encoding:
  ```Haskell
  let cipher = atbash message
  ```
  
  decoding:
  ```Haskell
  let message = atbash cipher
  ```
