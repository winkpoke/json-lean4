# JSON parser in Lean 4

This Lean 4 code features a **recursive descent** parser for JSON, transforming JSON strings into representations native to Lean data types. It is self-contained, not depending on any external libraries, and consists of approximately 200 lines of code.


# Build and run
```
lake build
lake exe json test/test1.json
```

