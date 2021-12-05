# Advent Of Code (2021) 

| Day | Rust      | Haskell   | F#       | Python   | Kotlin    | TypeScript    | 
|-----|-----------|-----------|----------|----------|-----------|---------------|
| 1   | &#10003;  | &#10003;  | &#10003; | &#10003; | &#10003;  |               |
| 2   | &#10003;  | &#10003;  | &#10003; | &#10003; | &#10003;  |               |
| 3   | &#10003;  | &#10003;  |          |          |           |               |
| 4   | &#10003;  |           |          | &#10003; |           |               |
| 5   | &#10003;  |           |          | &#10003; |           |               |

Day 1:
* Kotlin: Builtin zipWithNext is handy. Rich iterator functionality.
* Python: zip works multiple iterators. zip3 is not required.

Day 2:
* F#'s Active Patterns
* Python's Structural Pattern Matching.

Day 3:
* Haskell operators are inbuilt and first class. Rust requires user defined lambda.

Day 4:
* Rust's encapsulation and explicit mutability simplifies the code complexity
* Python's else block is very handy
* Persistent data structures in Haskell is a deal breaker. Rust has the balance of FP and all the goodness of OOP. 

Day 5:
* Python's syntax can be verbose relatively compared to F#'s succinct code or Haskell's point free coding but nevertheless it is very productive. 
* Rust code took a while. As an after thought, I wish I first started this day's code with Python before Rust. Sometimes good functions like fold deceives developers and takes them in a wrong direction. I wish I could make use of a map function's output in Rust like how I would in C#.

Overall:

IMHO,

* Python is suitable for MVP/prototyping is my opinion unless there is a need for static/strong typing. Rust's good/useful features has considerable negative impact on productivity. For e.g., usize used for indexing, references/dereferences for comparison, ownership and life times, traits and its implementation in different modules, etc. 
        - Just to illustrate my point, write some rust code until you get to a solution and delete it completely and try to rewrite it. Most of the ceremonies involved in Rust will almost be the same during the second time as well. In case of Python, the second time will be a breeze. 
        - Another attempt to illustrate the point, write some code in Rust and rewrite it in Python. Do the opposite to understand the difference.
* Rust is defintely the best language I have come across in my experience and I wish I could recommend Rust for trivial and simple problems. With some experience fighting the Rust compiler, fixing the issues becomes easier but still the point is that it takes time to do so, to write every line of such code. Reading Rust code is just fine. I love Haskell as well, purely as a language but it is does not seem practical. I have written C# for almost years now. I have exposure to C, C++, PERL, Java, JavaScript, C#, F#, TypeScript, Scala, Rust, Kotlin, Go, V in the right order. 
* Python is the only language besides Rust, Haskell and F# that has lots of missing features like algebraic data types and generics. Kotlin has generics but lacks algebraic data type
* Horses for the courses. Right tool for the right job. 
