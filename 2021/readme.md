# Advent Of Code (2021) 

| Day | Crystal   | Rust      | Python   | Haskell  | F#       | Kotlin    | TypeScript    | 
|-----|-----------|-----------|----------|----------|----------|-----------|---------------|
|  1  | &#10003;  | &#10003;  | &#10003; | &#10003; | &#10003; | &#10003;  |               |
|  2  | &#10003;  | &#10003;  | &#10003; | &#10003; | &#10003; | &#10003;  |               |
|  3  | &#10003;  | &#10003;  | &#10003; | &#10003; |          |           |               |
|  4  | &#10003;  | &#10003;  | &#10003; |          |          |           |               |
|  5  | &#10003;  | &#10003;  | &#10003; |          |          |           |               |
|  6  | &#10003;  | &#10003;  | &#10003; |          |          |           |               |
|  7  | &#10003;  | &#10003;  | &#10003; |          |          |           |               |
|  8  | &#10003;  | &#10003;  | &#10003; |          |          |           |               |
|  9  | &#10003;  |           |          |          |          |           |               |
| 10  | &#10003;  |           |          |          |          |           |               |

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
* Rust code took a while. As an after thought, I wish I first started this day's code with Python before Rust. Sometimes good functions like fold deceives me. It took me in a wrong direction today. I wish I could make use of a map function's output in Rust like how I would in C#.

Day 6:
* Rust HashMap's entry and and_modify expresses the intent well

Day 7:
* Crystal, Python and Rust use same algorithm but Crystal seems to be succinct solution of all due to its syntax brevity. Python is readable but reduce from functools, accumulate from itertools and operator.add from operator is too many imports to use the built-in functionality from the ceremonial perspective Modularity is definitely good from Python's own architecture stand point but now I was merely looking at it as a consumer, especially when comparing the languages. 
* Rust uses usize for indexing but Rust's abs_diff for subtracting for two usizes is still an experimental version. Absolute of two usize values cannot be negative. This is the strength (ensuring right behavior) and the weakness (by compromising the developer productivity) of Rust's type system. abs_diff is being worked on in itself makes a point.

Day 8:
* Python syntax was tedious to work with after the nested iterator calls go 4 level deep. Breaking down of calls and temporary assignments were required. Rust's chaining of calls were much better to work with
* Rust's life time hinders productivity on fold calls with HashMaps as initializers
* Crystal was much better to work with sets than Rust. It is not just sets, but in general, Rust's ownership and lifetime gets in the way of writing code and slows down

Day 9:
* Crystal seems like a productive language. It will my language of choice for the rest of the AOC 2021 unless there is a compelling feature from other language. Crystal's type system is flexible and prevents nil errors at compile time.
