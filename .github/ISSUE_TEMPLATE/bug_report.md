---
name: Bug report
about: Create a report to help us improve
---

**Description**
A clear and concise description of what the issue is about.

**Expected behaviour**
What you expected to happen, and what is happening instead.

**Context**
Please paste the `GHDL Bug occurred` log block here. Also, provide the following information:

- OS:
- Origin:
  - [ ] Package manager. Repo:
  - [ ] Released binaries. Tarball:
  - [ ] Built from sources. Commit SHA:

**Additional context**
Add any other context about the problem here. If applicable, add screenshots to help explain your problem.

**How to reproduce?**
Tell us how to reproduce this issue. Please provide a Minimal Working Example (MWE), that is compatible with [issue-runner](https://github.com/1138-4EB/issue-runner). With sample code it's easier to reproduce the bug and it's much faster to fix it. For example:

```
#>> ent.vhd
entity ent is
end entity;

architecture a of ent is
begin
  process begin
    report "Hello world" severity note;
    wait;
  end process;
end;

#>> sim.sh
ghdl -a ent.vhd
ghdl --elab-run ent

#>> run.sh
docker run --rm -tv /$(pwd):/src:z -w //src ghdl/ghdl:buster-mcode sh -c ./sim.sh

#>> end
```

Note that `run.sh` is used to execute `sim.sh` inside a docker container. Please, put your commands in `sim.sh` and just copy `run.sh` from the example. Using `ghdl/ghdl:*` docker images to run the MWEs ensures that the latest available GHDL is used.

**Files**
A list of relevant files for this issue. Large files can be uploaded one-by-one or in a tarball/zipfile. See [1138-4EB/issue-runner#parser](https://github.com/1138-4EB/issue-runner#parser).

**Checklist**
Before submitting your issue, please review the following checklist:

- [ ] Add `GHDL Bug occurred` log block
- [ ] Add a MWE
- [ ] Try the latest version
