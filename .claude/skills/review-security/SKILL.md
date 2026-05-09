---
name: review-security
description: Review code for security vulnerabilities and safety issues
context: fork
agent: Explore
allowed-tools: Read Glob Grep Bash(git:*)
arguments:
  - name: target
    description: Diff text or file paths to review
---

# Security Review

You are reviewing code for security vulnerabilities and safety issues. Analyze the provided changes or files and report findings.

## Target

$ARGUMENTS

## Approach

Think broadly about security. Don't limit yourself to a fixed checklist — consider any way the code could be exploited, misused, or fail unsafely given its context. Consider the OWASP top 10, CWE categories, and language-specific vulnerability classes, but treat those as starting points, not boundaries.

Key questions to drive your analysis:
- Where does untrusted input enter and how far does it travel before validation?
- What trust boundaries does this code sit on or cross?
- What happens if any assumption this code makes turns out to be false?
- What can an attacker control, and what can they reach from there?
- Are there failure modes that degrade security rather than denying access?

Adapt your focus to the language:
- **C**: memory safety is paramount — but also consider logic-level vulnerabilities
- **Haskell**: type safety handles many classes of bugs, so focus on boundaries — FFI, IO, serialization, network-facing code, auth logic
- **TypeScript**: client-side code is attacker-controlled — focus on what it trusts and what the server validates

## How to investigate

1. Read the changed files and understand the security context
2. Identify trust boundaries the code touches
3. Trace data from untrusted sources through to sensitive operations
4. Check that the code fails closed rather than open
5. Look for related security tests or validation logic

## Output format

Return your findings using this structure exactly:

## Security Review

### Critical
- **file:line** — description — suggested fix

### Major
- **file:line** — description — suggested fix

### Minor
- **file:line** — description — suggested fix

### Positive
- Notable security strengths observed

If a severity level has no findings, omit that section. Always include at least a Positive section.
