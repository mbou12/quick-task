# Quick Task -- DO NOT USE THIS YET, NEED TO FIX BUILD AND CONFIG 

Quick task is a simple CLI tool to help manage tasks one at a time.

Currently only supports unix environments.

---

### Installation

In the env directory there is a build bash program that will compile the source
file.

```bash
git clone git@github.com:mbou12/quick-task.git
cd quick-task
./env/config
./env/build
```

---

### Use

The binary is named `qt` and the following options are allowed.

When passing in options, the program does not process arguments to the options; 
when the program processes options that require input it is interactive.

- `qt --help`
- `qt -c` : complete the current task
- `qt -dc` : display the current task
- `qt -da` : display all tasks
- `qt -n` : add a task
- `qt -r` : remove a task
