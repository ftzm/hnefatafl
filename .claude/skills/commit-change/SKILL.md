---
name: commit-change
description: Create an atomic commit with verified acceptance criteria and a well-structured message. Invoke when ready to commit a change.
allowed-tools: Read Glob Grep Bash(git *) Edit
---

# Commit an Atomic Change

You are creating a commit. Actively verify that the change meets all
requirements below — run the commands, read the output, do not
self-report.

## Pre-commit checklist

### 1. Scope check

- [ ] The change contains only what is in scope for this commit.
- [ ] No unrelated cleanups, fixes, or "while I'm here" additions
      have crept in.
- [ ] The change represents one coherent idea.
- [ ] A reviewer can understand the diff in isolation.

### 2. Acceptance criteria are met

- [ ] New or updated tests exercise the change.
- [ ] Tests cover **every substantive part** of the change — not just
      the easiest path. If the commit touches N files/concerns, the
      tests must exercise all N, not a subset.
- [ ] All new verification is part of the project's test suite, not
      ad-hoc commands.
- [ ] If manual verification was used instead of tests: confirm it is
      because tests are **technically infeasible**, not merely
      inconvenient.

### 3. Size check

Run `git diff --cached --stat` after staging and check the output:

- [ ] ≤200 lines of substantive, novel code: good.
- [ ] 200–400 lines: acceptable but note it in the commit message.
- [ ] >400 lines of non-mechanical code: do not commit. Split first.

### 4. Codebase is green

Run the full test suite for every subproject touched by this commit:

- C library: `cd libhnefatafl && make test`
- Backend: `cd backend && cabal test`
- Frontend: `cd frontend && npm run check:all`

Read the output. Do not assume success. The codebase must compile and
all tests must pass — not just the new ones.

## Commit message format

Follow conventional commit format with labeled sections for
scannability. Every section after the summary line uses a
`Section:` label prefix.

```
<type>(<scope>): <summary line, under 72 chars>

Context: <what this builds on and what it enables — omit for
standalone changes>

Changes: <what was done, as a bulleted list>

Tests: <specific tests added and what they cover — or why manual
verification was used instead>
```

### Rules

- **Summary line**: one sentence, under 72 characters. Present tense.
- **Context**: only include if this commit extends prior work or
  enables a specific next step. Must be self-contained — a reader
  seeing only `git log` must understand it without access to any
  plan document, ticket, or conversation. Never reference plan
  slice numbers, step IDs, or internal jargon. Describe what
  this builds on and enables in plain terms. Omit entirely for
  standalone changes.
- **Changes**: bulleted list of what changed. One bullet per
  concern/file-group. Terse — a reviewer reading `git log` should
  parse this in seconds.
- **Tests**: name the test file(s) and what they exercise. If manual
  verification was used, explain why tests were infeasible.
- Keep lines under 72 characters for `git log` readability.
- No filler, no restating the summary in the body.

## Process

1. Stage the relevant files (`git add <specific files>`). Never use
   `git add -A` or `git add .`.
2. Run `git diff --cached --stat` and check the size.
3. Run the full test suite (step 4 of the checklist). Read the output.
4. If any check fails, fix the issue before proceeding.
5. Write the commit message following the format above.
6. **Present the commit message to the user for review before
   committing.** Do not run `git commit` until the user approves.
7. Create the commit. Use a HEREDOC for the message:
   ```bash
   git commit -m "$(cat <<'EOF'
   <type>(<scope>): <summary>

   Context: ...

   Changes:
   - ...
   - ...

   Tests: ...

   Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
   EOF
   )"
   ```
7. Verify the commit succeeded (`git log -1`). If a pre-commit hook
   fails, fix the issue and create a **new** commit — do not amend.
