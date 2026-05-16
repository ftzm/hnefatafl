---
name: review-documentation
description: Review code for appropriate documentation — present where needed, absent where redundant
context: fork
agent: Explore
allowed-tools: Read Glob Grep Bash(git *) Read(//tmp/**) Write(//tmp/**)
arguments:
  - name: target
    description: Diff text or file paths to review
---

# Documentation Review

You are reviewing code for appropriate documentation. Analyze the provided changes or files and report findings.

## Target

$ARGUMENTS

## What to look for

Evaluate the code **as it stands now**. The question is always
"does this code have the documentation it needs?" — not "what
changed." A missing comment is a finding whether it was never
there or was removed during this change. Frame findings in terms
of what the code is, not what it was.

- Code whose purpose is not immediately evident from usage
  should be documented. Emphasize the **why**, not the **what**.
- Only document **how** the code works if the implementation is
  surprising or non-obvious — and then also explain why it works
  that way.
- Undocumented non-obvious code is a finding.
- Redundant comments on self-evident code are also a finding.
- **Documentation describes what a unit of code IS** — its
  contract, behavior, preconditions, invariants — never how it
  is called or what calls it. Each unit must be comprehensible
  from its signature, documentation, and implementation alone.
  Comments that reference callers or external context invert
  the flow of comprehension and are a finding.

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
