# Quick Task

Quick task is a simple CLI tool to help manage tasks one at a time.

---

### Installation

In the env directory there is a build bash program that will compile the source
file.

```bash
git clone git@github.com:mbou12/quick-task.git
cd quick-task
./env/build
```

---

### Set Up

Push the path of the binary into the `$PATH` enviromental variable. Set up
the doc files that store your tasks.

```bash
echo -e "\nexport "'$PATH'":$PWD/env/bin" >> ~/.bash_profile
source ~/.bash_profile
touch ./doc/task.txt
touch ./doc/completed.txt
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
