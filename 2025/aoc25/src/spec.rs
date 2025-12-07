// use std::collections::HashMap;

// pub struct DataItem(Option<DataInfo>, DataKind, Repetition);

// type DataInfo = (&'static str, DataType);

// enum DataType {
//     Int,
//     Array(Box<DataType>)
// }

// pub struct Specification {
//     fragments: Vec<DataItem>
// }

// impl Specification {
//     fn new(fragments: Vec<DataItem>) -> Self {
//         Self {
//             fragments
//         }
//     }

//     fn read(&self, data: String) -> HashMap<String, DataType> {
//         HashMap::new()
//     }

//     fn read_integers() -> Specification {
//         let fragments = vec![
//             DataItem(Some(("numbers", DataType::Array(Box::new(DataType::Int)))), DataKind::Int64, Repetition::Many)
//         ];
//         Specification { fragments }
//     }

// }

// enum LineSpecification {
//     Integers,
//     Words,
//     Chars,
//     Custom(Specification)
// }

// enum DataKind {
//     Int32,
//     Int64,
//     Float32,
//     Float64,
//     Word,
//     Literal(&'static str),
//     Char,
//     Space(char),
//     Digit,
// }

// enum Repetition {
//     Many,
//     Atleast(usize),
//     Exact(usize)
// }