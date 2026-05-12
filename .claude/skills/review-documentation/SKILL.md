---
name: review-documentation
description: Review code for appropriate documentation — present where needed, absent where redundant
context: fork
agent: Explore
allowed-tools: Read Glob Grep Bash(git *)
arguments:
  - name: target
    description: Diff text or file paths to review
---

# Documentation Review

You are reviewing code for appropriate documentation. Analyze the provided changes or files and report findings.

## Target

$ARGUMENTS

## What to look for

- Code whose purpose is not immediately evident from usage
  should be documented. Emphasize the **why**, not the **what**.
- Only document **how** the code works if the implementation is
  surprising or non-obvious — and then also explain why it works
  that way.
- Undocumented non-obvious code is a finding.
- Redundant comments on self-evident code are also a finding.

## How to investigate

1. Read the changed files and consider: would a competent
   developer new to this codebase understand this code without
   the comment? If yes, the comment is noise. If no, a comment
   is missing.
2. Check existing documentation style in the codebase for
   consistency.
3. Look for magic numbers, non-obvious algorithms, subtle
   invariants, or surprising design choices that lack explanation.

## Output format

Return your findings using this structure exactly:

## Documentation Review

### Critical
- **file:line** — description — suggested fix

### Major
- **file:line** — description — suggested fix

### Minor
- **file:line** — description — suggested fix

### Positive
- Notable documentation strengths observed

If a severity level has no findings, omit that section. Always include at least a Positive section.
