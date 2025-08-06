# OR Instruction Implementation Notes

## Overview
The OR instruction in IL (Instruction List) creates parallel branches in ladder logic. This is a fundamental concept in PLC programming where multiple conditions can activate the same output.

## How OR Works

### Stack-Based Processing
1. When an OR instruction is encountered, it:
   - Pops the current logic from the stack
   - Creates a new parallel branch with the OR operand
   - Combines them into a parallel structure
   - Pushes the parallel structure back onto the stack

### Visual Representation
The parallel branches are displayed with each branch on a separate line, all connecting to the same output:

```
001: |-[X0]-[X1]-(Y0)|
     |-[X2]-----(Y0)|
```

This means Y0 will be ON if either:
- X0 AND X1 are both ON
- OR X2 is ON

## Code Structure

### Key Functions

1. **process-instruction :OR** - Handles OR instruction processing
   - Creates parallel branch structure
   - Manages stack operations

2. **render-parallel-branches** - Renders parallel branches correctly
   - First branch includes rung number
   - Subsequent branches are indented
   - All branches show the same output

3. **render-rung** - Detects and handles parallel logic
   - Special case for parallel structures
   - Normal case for series logic

## Examples

### Simple OR
```
LD X0
OR X1
OUT Y0
```
Output: Two parallel paths to Y0

### Complex AND-OR
```
LD X0
AND X1
OR X2
OUT Y0
```
Output: (X0 AND X1) OR X2 activates Y0

### Multiple OR
```
LD X0
OR X1
OR X2
OR X3
OUT Y0
```
Output: Four parallel paths, any one activates Y0

## Testing
Run tests with:
```bash
lein run example
```

Or test specific IL code:
```clojure
(convert-il-to-ld "LD X0\nOR X1\nOUT Y0")
```