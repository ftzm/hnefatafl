---
name: review
description: Run a multi-dimensional code review using parallel sub-agents
allowed-tools: Read Glob Grep Bash(git *) Agent
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

Spawn **all 8** sub-reviewers **in parallel using the Agent tool** — all 8 Agent calls in a **single message**. This is critical for parallelism; the Agent tool runs concurrently when multiple calls are in one message.

For each agent:
- Read the corresponding skill file from `.claude/skills/review-*/SKILL.md` to get its full prompt
- Pass the diff/file content as the `$ARGUMENTS` target
- Use `subagent_type: "Explore"` (they only need read access)

The 8 skill files to read and dispatch:
1. `.claude/skills/review-correctness/SKILL.md`
2. `.claude/skills/review-security/SKILL.md`
3. `.claude/skills/review-performance/SKILL.md`
4. `.claude/skills/review-style/SKILL.md`
5. `.claude/skills/review-composability/SKILL.md`
6. `.claude/skills/review-best-practices/SKILL.md`
7. `.claude/skills/review-documentation/SKILL.md`
8. `.claude/skills/review-testing/SKILL.md`

Each agent's prompt should be the full content of the skill file (everything after the frontmatter `---`) with `$ARGUMENTS` replaced by the actual diff or description of what to review. Tell the agent to run `git diff --cached` (or whatever command resolves the target) to see the code.

## Step 3: Synthesize

Once all sub-reviewers return, produce a unified review:

1. **Deduplicate**: if multiple reviewers flagged the same issue, merge into one finding and note which dimensions it touches
2. **Include every finding**: every issue from every sub-reviewer must appear in the synthesis. Deduplication merges overlapping findings — it never drops them. If a finding is unique to one reviewer, it still appears.
3. **Preserve severity**: use the highest severity any sub-reviewer assigned to a finding. Never downgrade a sub-reviewer's severity — if a reviewer calls something Critical, it stays Critical in the synthesis unless merged with another finding that changes the substance
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
