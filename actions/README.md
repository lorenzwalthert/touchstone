# Github Actions for {touchstone}

This folder contains the [Github Actions](https://github.com/features/actions) used when benchmarking a package with {touchstone}. For [security reasons](https://securitylab.github.com/research/github-actions-preventing-pwn-requests/) the workflow is split into twp separate actions:

* [lorenzwalthert/touchstone/actions/receive](https://github.com/lorenzwalthert/touchstone/tree/main/actions/receive)
  * Reads `config.json` to prepare and run the benchmark job.
  * Does not have read & write access.
  * Started via PR/push to PR (on main branch).
* [lorenzwalthert/touchstone/actions/comment](https://github.com/lorenzwalthert/touchstone/tree/main/actions/comment)
  * Comments the results on the PR that originated the workflow run. 
  * Has read & write access.
  * Started automatically when receive job finishes.
  
The actions will always be compatible with the version of {touchstone} of the same git ref, so we recommend using the same tag for both {touchstone} and the actions, e.g. with {touchstone} v0.0.1:
```yaml
- uses lorenzwalthert/touchstone/actions/receive@v0.0.1
```
