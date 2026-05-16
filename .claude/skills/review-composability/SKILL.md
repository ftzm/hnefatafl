---
name: review-composability
description: Review code for modularity, interface design, coupling, and reuse potential
context: fork
agent: Explore
allowed-tools: Read Glob Grep Bash(git *) Read(//tmp/**) Write(//tmp/**)
arguments:
  - name: target
    description: Diff text or file paths to review
---

# Composability Review

You are reviewing code for modularity, composability, and clean architecture. Analyze the provided changes or files and report findings.

## Target

$ARGUMENTS

## Approach

Good composability means building solutions from small, principled,
atomic pieces of functionality. When code is written this way, reuse
falls out naturally — different solutions share the same building
blocks. The goal is NOT "find similar code and merge it." It is:
are the abstractions at the right level? Is each piece doing one
thing well? Could these pieces be recombined to solve a different
problem?

Key questions to drive your analysis:
- Is each function a well-defined, atomic unit of work — or is it
  a monolith doing several things that should be separate?
- Are abstractions at the right level — reusable primitives vs
  one-off combinations?
- Could this logic be expressed as a composition of existing,
  smaller pieces?
- Are concerns properly separated so pieces can be recombined
  for different purposes?
- Are interfaces narrow — do functions take only what they need
  and return only what callers need?
- Are internal details properly encapsulated, or do consumers
  reach into implementation?
- Is shared state minimized and made explicit?

Adapt your focus to the language:
- **Haskell**: effect constraints should be minimal, types composed from smaller pieces, functions generalizable over typeclasses where natural
- **C**: static linkage for internal functions, clean header interfaces, separation of data structures from algorithms
- **TypeScript/SolidJS**: component boundaries, prop interfaces, separation of logic from presentation

## How to investigate

1. Read the changed files and their module headers/exports
2. Check what imports/depends on the modified code (grep for usages)
3. Look at the module structure around the change
4. Consider whether the change makes future extension easier or harder

## Output format

Return your findings using this structure exactly:

## Composability Review

### Critical
- **file:line** — description — suggested fix

### Major
- **file:line** — description — suggested fix

### Minor
- **file:line** — description — suggested fix

### Positive
- Notable composability strengths observed

If a severity level has no findings, omit that section. Always include at least a Positive section.
