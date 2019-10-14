# Wasm-subset

Sturdy Style generic interpreter for a subset language of WebAssembly

Is stack based and supports control flow

To run the short example programs in ```test/```
- run ```stack test``` in de root directory

Currently implemented
- Typeclass and concrete transformer for stack computations
- Value interface for addition, literals, equality and comparison
- Block, Loop and break control instructions
  
Not implemented yet
- Type checker implementation
- If statement