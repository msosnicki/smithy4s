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

structure ValidatedFoo {
    name: ValidatedString = "abc"
}

//new ones for collection constraints

//doesn't make sense (noop)
@nameFormat
// @validateNewtype
string ValidatedRefinedPrimitive

@length(max: 1)
@validateNewtype
list ValidatedConstrainedList {
    member: String
}

// // SHOULD BE SUPPORTED - not yet
// @validateNewtype
// list ValidatedListConstrainedMember {
//     @length(max: 1)
//     member: String
// }

// // SHOULD BE SUPPORTED - not yet
// @validateNewtype
// @length(max: 1)
// list ValidatedConstrainedListConstrainedMember {
//     @length(max: 1)
//     member: String
// }

// // SHOULD BE SUPPORTED - not yet
// @validateNewtype
// @length(max: 1)
// list ValidatedConstrainedListRefinedMember {
//     member: ValidatedRefinedPrimitive
// }

// SHOULD BE SUPPORTED - not yet
// @validateNewtype
// @length(max: 1)
// list ValidatedConstrainedListRefinedConstrainedMember {
//     @length(max:2)
//     member: ValidatedRefinedPrimitive
// }

// @validateNewtype
// @length(max: 1)
//SHOULD BE SUPPORTED - not yet
// list ValidatedConstrainedListConstrainedRefinedMember {
//     @length(max: 1)
//     member: ValidatedRefinedPrimitive
// }

// // doesn't make sense (noop)
// @nonEmptyListFormat
// @validateNewtype
// list ValidatedRefinedList {
//     member: String
// }

// @validateNewtype
// @nonEmptyListFormat
// list ValidatedRefinedListConstrained {
//     @length(max: 1)
//     member: String
// }

// @nonEmptyListFormat
// @validateNewtype
// list ValidatedRefinedListConstrainedMember {
//     @length(max: 1)
//     member: String
// }

// @nonEmptyListFormat
// // @validateNewtype
// list ValidatedRefinedList {
//     member: ValidatedName
// }

@length(max: 1)
@validateNewtype
map ValidatedMap {
    key: String
    value: Integer
}

