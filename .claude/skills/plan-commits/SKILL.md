---
name: plan-commits
description: Break a task into atomic, commit-sized steps with acceptance criteria. Invoke before starting ANY implementation work.
allowed-tools: Read Glob Grep Bash(git *) Agent
---

# Plan Atomic Commits

You are planning implementation work. Break the task into a sequence of
atomic commits, each coherent and comprehensible as a single unit.

This skill is invoked for ALL implementation work — even tasks that
look like a single commit. Scope is not always obvious upfront, and
the planning step ensures you think through what changes, what to
test, and how to verify before writing code.

## Commit size limits

- **Target**: ≤200 lines of substantive, novel code changes per commit.
- **Hard limit**: 400 lines. Never exceed this for non-trivial code.
- **Exempt**: purely mechanical/repetitive changes (renames, reformats,
  migrations that follow a pattern) and lockfiles may exceed these limits.

## Atomicity

Each commit represents one coherent idea: adding a set of DB queries,
implementing a module of pure logic, wiring up an endpoint, etc. A
reviewer should be able to understand the commit in isolation without
needing context from uncommitted future work.

Incremental building blocks are fine — a commit can add code that is
unused until the following commit. The unit of coherence is the idea,
not "is it called yet."

## Scope discipline

A commit contains only what is in its scope. If during implementation
you notice something unrelated that should change — a nearby cleanup,
an unrelated bug, a "while I'm here" improvement — do not include it.
Note it for a separate commit.

## Codebase must stay green

Every commit must leave the codebase in a state where it compiles and
the full test suite passes. No "this commit breaks the build but the
next one fixes it."

## Required fields per commit

Every commit in the plan must include all of these:

- **Scope**: the coherent idea this commit represents, in one sentence.
- **Changes**: specific files and what changes in each. Read the
  codebase to identify the right files and understand existing patterns
  before specifying changes.
- **Builds on**: which prior commit(s) this extends, and what concrete
  outputs it uses (e.g., "uses `SetClockState` command from 2.1").
  Omit for standalone changes or the first commit in a sequence.
- **Enables**: what this unlocks — a) the immediate technical next
  step that will build on this, and b) progress toward the overall
  feature goal.
- **Acceptance criteria**: the specific tests to be written. Name what
  they cover and which parts of the change they exercise. Tests are
  **required** unless technically infeasible — not merely inconvenient.
  **Coverage must account for every substantive part of the change** —
  if a commit touches three files, the tests must exercise all three,
  not just the easiest one to test. All new automatable verification
  must be part of the project's test suite, not ad-hoc commands.
- **Est. size**: line count estimate.

## Plan structure

For multi-commit work, use this document structure:

```markdown
# [Feature] — Implementation Plan

## Context
[What we're building and why, in a few sentences.]

## Progress
| # | Commit | Status |
|---|--------|--------|
| 1 | `feat(scope): summary` | not started |
| ... | ... | ... |

---

### 1 — `feat(scope): summary`

**Scope:** ...
**Builds on:** ...
**Enables:** ...

**Changes:**
- `path/to/file.hs` — description of change
- ...

**Acceptance criteria:**
- Tests: ...

**Est. size:** ~N lines.
```

For single-commit work, the same fields apply but no document is
needed — present them inline in the conversation.

Update the Progress table as commits are completed.

## Process

1. **Read the codebase first.** Understand existing patterns, types, and
   file structure before proposing changes. Never guess at file paths
   or function signatures.
2. **Estimate sizes.** If a commit looks like it will exceed 200 lines,
   consider splitting. If it would exceed 400 lines, it must be split.
3. **Verify coherence.** Each commit should be understandable in
   isolation. Ask: "Could a reviewer make sense of this diff without
   knowing what comes next?"
4. **Present the plan for review** before starting implementation.
