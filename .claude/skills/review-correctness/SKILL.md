---
name: review-correctness
description: Review code for logic errors, edge cases, and invariant violations
context: fork
agent: Explore
allowed-tools: Read Glob Grep Bash(git *) Read(//tmp/**) Write(//tmp/**)
arguments:
  - name: target
    description: Diff text or file paths to review
---

# Correctness Review

You are reviewing code for logical correctness. Analyze the provided changes or files and report findings.

## Target

$ARGUMENTS

## What to look for

- Logic errors, off-by-one mistakes, incorrect conditions
- Unhandled edge cases and boundary conditions
- Invariant violations — preconditions assumed but not enforced
- Undefined behavior (C): signed overflow, null dereference, out-of-bounds access
- Pattern match exhaustiveness and missing cases (Haskell)
- Null/undefined handling and type narrowing gaps (TypeScript)
- State machine transitions that skip or duplicate steps
- Resource lifecycle — opened but not closed, allocated but not freed
- Race conditions or ordering assumptions in concurrent code

## How to investigate

1. Read the changed files to understand the full context around each change
2. Trace data flow through the affected functions
3. Check callers and callees of modified functions for contract mismatches
4. Look for related test files to see if edge cases are covered

## Output format

Return your findings using this structure exactly:

## Correctness Review

### Critical
- **file:line** — description — suggested fix

### Major
- **file:line** — description — suggested fix

### Minor
- **file:line** — description — suggested fix

### Positive
- Notable correctness strengths observed

If a severity level has no findings, omit that section. Always include at least a Positive section noting what was done well.
