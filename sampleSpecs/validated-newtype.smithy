$version: "2"

namespace smithy4s.example

use smithy4s.meta#validateNewtype

@length(min: 1)
@pattern("[a-zA-Z0-9]+")
@validateNewtype
string ValidatedString

@length(min: 1)
@pattern("[a-zA-Z0-9]+")
string NonValidatedString

@length(max: 1)
@validateNewtype
list ValidatedList {
    member: String
}

list OtherList {
    @length(max: 1)
    member: String
}

// @validateNewtype
// list ValidatedMemberList {
//     @length(max: 1)
//     member: String
// }

@length(max: 1)
@validateNewtype
map ValidatedMap {
    key: String
    value: Integer
}

structure ValidatedFoo {
    name: ValidatedString = "abc"
}
