module primes 

open System

let primesUnder max =
   let rec filterPrimes lst =
      match lst with
      | [] -> []
      | h :: t -> h :: (filterPrimes [ for x in t do if x % h <> 0 then yield x])
   filterPrimes [2..max]

let printPrimeTable w =
   let printCnt max cnt n =
      let delim = if (cnt+1)%max=0 then '\n' else ' '
      printf "%3d%c" n delim
      cnt+1
   primesUnder 
   >> List.fold (w |> printCnt) 0 >> ignore 

let pause () =
   printfn "\nPress any key..."
   Console.ReadKey () |> ignore

[<EntryPoint>]
let main (args : string[]) =
   let len = Array.length args
   let limit = if len < 1 then 1000 else int args.[0]
   let width = if len < 2 then 15 else int args.[1]
   limit |> printPrimeTable width
   pause ()
   0
