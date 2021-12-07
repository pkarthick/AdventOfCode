# Advent Of Code (2021) 

| Day | Rust      | Python    | Crystal  | Haskell  | F#       | Kotlin    | TypeScript    | 
|-----|-----------|-----------|----------|----------|----------|-----------|---------------|
| 1   | &#10003;  | &#10003;  | &#10003; | &#10003; | &#10003; | &#10003;  |               |
| 2   | &#10003;  | &#10003;  | &#10003; | &#10003; | &#10003; | &#10003;  |               |
| 3   | &#10003;  | &#10003;  | &#10003; | &#10003; |          |           |               |
| 4   | &#10003;  | &#10003;  |          |          |          |           |               |
| 5   | &#10003;  | &#10003;  |          |          |          |           |               |
| 6   | &#10003;  | &#10003;  |          |          |          |           |               |

Day 1:
* Kotlin: Builtin zipWithNext is handy. Rich iterator functionality.
* Python: Has itertools pairwise. zip works multiple iterators. zip3 is not required.

Day 2:
* F#'s Active Patterns
* Python's Structural Pattern Matching
* Crystal lacks tuple destructuring in the block arguments, match statement does not match string literals, variables can't be used in case  

Day 3:
* Haskell operators are inbuilt and first class. Rust requires user defined lambda.

Day 4:
* Rust's encapsulation and explicit mutability simplifies the code complexity
* Python's else block is very handy
* Persistent data structures in Haskell is a deal breaker. Rust has the balance of FP and all the goodness of OOP. 

Day 5:
* Python's syntax can be verbose relatively compared to F#'s succinct code or Haskell's point free coding but nevertheless it is very productive. 
* Rust code took a while. As an after thought, I wish I first started this day's code with Python before Rust. Sometimes good functions like fold deceives developers and takes them in a wrong direction. I wish I could make use of a map function's output in Rust like how I would in C#.

Day 6:
* Rust HashMap's entry and and_modify expresses the intent well
