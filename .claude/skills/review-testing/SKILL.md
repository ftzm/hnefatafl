---
name: review-testing
description: Review code for thorough, well-structured test coverage
context: fork
agent: Explore
allowed-tools: Read Glob Grep Bash(git *) Read(//tmp/**) Write(//tmp/**)
arguments:
  - name: target
    description: Diff text or file paths to review
---

# Testing Review

You are reviewing code for thorough, well-structured test coverage. Analyze the provided changes or files and report findings.

## Target

$ARGUMENTS

## What to look for

### Coverage
- All non-tautological code should be tested. Simple line
  coverage is not enough.
- For any non-trivial logic, ask: how could this break? Under
  what usage patterns could it break? Are those scenarios
  exercised in tests?
- Edge cases, boundary conditions, error paths, and state
  transitions should all have test cases.
- For concurrent or event-driven code: what happens if events
  arrive out of order? If one operation fires before another
  completes? If two operations race? These timing scenarios
  need tests.
- Untested code paths are findings, proportional to their risk.

### Test quality
- Test cases should be simple and focused on what is being
  tested, not on setup ceremony.
- When test setup becomes cumbersome, look for opportunities to
  extract principled, generic test helpers. Helpers should reduce
  accidental complexity without hiding what the test verifies.
- Tests should be readable as specifications: a reader should
  understand the expected behavior from the test alone.
- Avoid testing implementation details — test behavior and
  contracts.

## How to investigate

1. Read the changed code and identify all branches, edge cases,
   and error conditions.
2. Find the corresponding test files and check which scenarios
   are covered.
3. Look at existing test helpers and patterns in the codebase —
   could they be reused or extended?
4. Check if new test helpers would reduce duplication across
   test cases.

## Output format

Return your findings using this structure exactly:

## Testing Review

### Critical
- **file:line** — description — suggested fix

### Major
- **file:line** — description — suggested fix

### Minor
- **file:line** — description — suggested fix

### Positive
- Notable testing strengths observed

If a severity level has no findings, omit that section. Always include at least a Positive section.
