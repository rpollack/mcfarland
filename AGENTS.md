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
