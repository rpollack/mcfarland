# Codex Workflow Rules

## Branches and PRs

Before creating any new PR branch, always start from a freshly updated `master`:

```bash
git checkout master
git fetch origin --prune
git merge --ff-only origin/master
git checkout -b <new-branch-name>
```

All pull requests must be real, non-draft PRs.

PR titles and descriptions should be customer-value focused. Lead with what the user or customer gets from the change, then include implementation notes and test coverage as supporting detail.

Before creating a PR, review the full branch diff and make sure client and server tests still match the changed behavior. A branch may go through several implementation iterations, so tests do not need to be updated after every prompt, but they must be updated before PR creation. If tests need changes, make those changes and commit them before opening the PR. The PR description should include the relevant test commands that were run, or clearly state any test that could not be run.
