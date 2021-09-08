# can initialize with cli [plain]

    Code
      use_touchstone()
    Message <cliMessage>
      v Populated file 'script.R' in 'touchstone/'.
      v Populated file 'config.json' in 'touchstone/'.
      v Populated file '.gitignore' in 'touchstone/'.
      v Populated file 'touchstone-receive.yaml' in '.github/workflows/'.
      v Populated file 'touchstone-comment.yaml' in '.github/workflows/'.
      v Populated file 'cancel.yaml' in '.github/workflows/'.
      v Added a cancelling action for the touchstone workflow.
      i A new push to a branch will stop the current benchmarking run and start
        benchmarking your latest push (instead of queuing it until the previous
        completed). You can manually list other Github Actions workflows in the
        cancel workflow to stop running outdated actions to save compute resources
        and time.
      * Replace the mtcars sample code in `touchstone/script.R` with code from your
      package you want to benchmark.
      * Commit and push to GitHub to the default branch to activate the workflow,
      then make a pull request to trigger your first benchmark run.

# can initialize with cli [ansi]

    Code
      use_touchstone()
    Message <cliMessage>
      [32mv[39m Populated file [34m[34mscript.R[34m[39m in [34m[34m[34mtouchstone[34m/[34m[39m.
      [32mv[39m Populated file [34m[34mconfig.json[34m[39m in [34m[34m[34mtouchstone[34m/[34m[39m.
      [32mv[39m Populated file [34m[34m.gitignore[34m[39m in [34m[34m[34mtouchstone[34m/[34m[39m.
      [32mv[39m Populated file [34m[34mtouchstone-receive.yaml[34m[39m in [34m[34m[34m.github/workflows[34m/[34m[39m.
      [32mv[39m Populated file [34m[34mtouchstone-comment.yaml[34m[39m in [34m[34m[34m.github/workflows[34m/[34m[39m.
      [32mv[39m Populated file [34m[34mcancel.yaml[34m[39m in [34m[34m[34m.github/workflows[34m/[34m[39m.
      [32mv[39m Added a cancelling action for the touchstone workflow.
      [36mi[39m A new push to a branch will stop the current benchmarking run and start
        benchmarking your latest push (instead of queuing it until the previous
        completed). You can manually list other Github Actions workflows in the
        cancel workflow to stop running outdated actions to save compute resources
        and time.
      * Replace the mtcars sample code in `touchstone/script.R` with code from your
      package you want to benchmark.
      * Commit and push to GitHub to the default branch to activate the workflow,
      then make a pull request to trigger your first benchmark run.

