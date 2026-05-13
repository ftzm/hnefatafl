---
name: review
description: Run a multi-dimensional code review using parallel sub-agents
allowed-tools: Read Glob Grep Bash(git *) Agent Skill
arguments:
  - name: target
    description: What to review — file paths, a git ref like HEAD~3, a branch name, etc.
---

# Code Review Orchestrator

You are orchestrating a comprehensive code review. Your job is to resolve the target, dispatch parallel sub-reviewers, and synthesize their findings.

## Target

$ARGUMENTS

## Step 1: Resolve the target

Determine what code to review:
- If the target looks like a git ref (HEAD~3, a branch name, a commit hash): run `git diff <target>` to get the diff
- If the target is file paths: read those files
- If ambiguous, use your best judgment

Capture the diff or file contents — you'll pass this to each sub-reviewer.

## Step 2: Dispatch sub-reviewers

Spawn **all 8** of these sub-reviewer skills in parallel using the Skill tool, passing the diff/file content as the argument to each:

1. `/review-correctness`
2. `/review-security`
3. `/review-performance`
4. `/review-style`
5. `/review-composability`
6. `/review-best-practices`
7. `/review-documentation`
8. `/review-testing`

Each runs in its own isolated sub-agent and returns structured findings.

## Step 3: Synthesize

Once all sub-reviewers return, produce a unified review:

1. **Deduplicate**: if multiple reviewers flagged the same issue, merge into one finding and note which dimensions it touches
2. **Include every finding**: every issue from every sub-reviewer must appear in the synthesis. Deduplication merges overlapping findings — it never drops them. If a finding is unique to one reviewer, it still appears.
3. **Rank by severity**: Critical > Major > Minor, across all dimensions
4. **Present the unified report** in this format:

---

## Code Review: [target]

### Critical
- **file:line** — [dimensions] description — suggested fix

### Major
- **file:line** — [dimensions] description — suggested fix

### Minor
- **file:line** — [dimensions] description — suggested fix

### Positive
- Strengths noted across reviews

---

The `[dimensions]` tag shows which review(s) flagged it, e.g. `[security, correctness]`.

Keep each finding concise — one or two sentences. Don't pad — if the code is clean, say so briefly. But never omit a finding for brevity.
