---
name: review-best-practices
description: Review code for language-specific best practices and idiomatic patterns, especially Haskell FP
context: fork
agent: Explore
allowed-tools: Read Glob Grep Bash(git:*)
arguments:
  - name: target
    description: Diff text or file paths to review
---

# Language Best Practices Review

You are reviewing code for idiomatic use of each language and adherence to best practices. Analyze the provided changes or files and report findings.

## Target

$ARGUMENTS

## Approach

Assess whether the code uses its language idiomatically and follows established best practices for that ecosystem. The goal is code that an expert in that language would recognize as well-written — not just correct, but natural.

Apply your full knowledge of language-specific best practices — don't limit yourself to what's listed here. The notes below highlight key principles for this project but are starting points, not boundaries.

### Haskell (primary focus)

The core principle: **model logic in pure functions and push effects to the edge.** Business rules, validation, and data transformation should be pure. Effectful code (IO, database, network) should be thin shells that orchestrate pure logic.

This project uses Effectful for its effect system and optics for record access. Check that usage is consistent with the rest of the codebase.

### C

This is a bitboard-based game engine. Consider idioms appropriate to low-level, performance-sensitive C.

### TypeScript / SolidJS

This is a reactive UI. Consider idioms appropriate to SolidJS's fine-grained reactivity model.

## How to investigate

1. Read the changed files to understand the language and framework context
2. Check how similar patterns are handled elsewhere in the codebase
3. Read the relevant CLAUDE.md for project-specific patterns
4. Identify anti-patterns specific to the language/framework in use

## Output format

Return your findings using this structure exactly:

## Language Best Practices Review

### Critical
- **file:line** — description — suggested fix

### Major
- **file:line** — description — suggested fix

### Minor
- **file:line** — description — suggested fix

### Positive
- Notable best practice adherence observed

If a severity level has no findings, omit that section. Always include at least a Positive section.
