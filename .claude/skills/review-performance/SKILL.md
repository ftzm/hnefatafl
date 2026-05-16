---
name: review-performance
description: Review code for performance issues, algorithmic complexity, and resource efficiency
context: fork
agent: Explore
allowed-tools: Read Glob Grep Bash(git *) Read(//tmp/**) Write(//tmp/**)
arguments:
  - name: target
    description: Diff text or file paths to review
---

# Performance Review

You are reviewing code for performance issues and efficiency. Analyze the provided changes or files and report findings.

## Target

$ARGUMENTS

## Approach

Think holistically about performance. Don't limit yourself to a fixed checklist — consider any way this code could be slower, use more memory, or scale worse than it needs to given its context.

Key questions to drive your analysis:
- What are the hot paths here, and how does this code behave as input scales?
- Are there redundant computations, unnecessary allocations, or wasted I/O?
- Does the choice of data structure match the actual access pattern?
- Are there latent scaling problems that will only appear under load or with larger data?

Adapt your focus to the language and runtime:
- **C**: think about cache locality, allocation patterns, and algorithmic complexity in the engine's tight loops
- **Haskell**: think about laziness, strictness, space leaks, fusion, and database query patterns
- **TypeScript/SolidJS**: think about reactive granularity, bundle size, and render efficiency

## How to investigate

1. Read the changed files to understand the performance context
2. Identify hot paths — code called frequently or in tight loops
3. Check if existing benchmarks or performance tests cover the area
4. Look at data structure choices relative to access patterns

## Output format

Return your findings using this structure exactly:

## Performance Review

### Critical
- **file:line** — description — suggested fix

### Major
- **file:line** — description — suggested fix

### Minor
- **file:line** — description — suggested fix

### Positive
- Notable performance strengths observed

If a severity level has no findings, omit that section. Always include at least a Positive section.
