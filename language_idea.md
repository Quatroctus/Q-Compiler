```rust

object1 = struct {
    value1 value2 = i32;
}

object2 = struct {
    integers = object1;
    scale = f32;
}

obj1 = object2 ...;
obj2 = object2 obj1; // Valid: Single-Variable copy initialization. 

obj = object2 {0, 2}, .5f; // Valid: Single-Variable initialization to implicit constructed value.
obj = object2 {{0, 2}, .5f}; // Valid: Single-Variable initialization to explicit constructed value.
obj1 obj2 = object2 {0, 2}, .5f; // Valid: Multi-Variable initialization to implicit constructed value.
obj1 obj2 = object2 {0, 2}, {.5f}; // Invalid: Multi-Variable initialization to explicit constructed values.
obj1 obj2 = object2 {{0, 2}, {.5f}}; // Valid: Multi-Variable initialization to explicitly constructed value.

// This must be a single implicit or explicit constructor call.
//    var = type expressioncsl
//    var = type {expressioncsl}, ..., {expressioncsl}
//    var = type {expressioncsl} intermixed with expressioncsl

// This must be a series of explicit constructor calls.
//    multi-var = type {expressioncsl}, ..., {expressioncsl}

// When the number of vars = the number of expressions, must be a series of explicitly constructed values;
//   otherwise must be a single implicit constructor call.
//    multi-var = type expressioncsl

// This must be a single implicit constructor call.
//    multi-var = type {expressioncsl} intermixed with expressioncsl

// When the number of vars = the number of constructors, must be a series of explicitly constructed values;
//   otherwise must be a single implicit constructor call.
//    multi-var = type {expressioncsl}, ..., {expressioncsl}

main = args string[argc] i32 {

}
```