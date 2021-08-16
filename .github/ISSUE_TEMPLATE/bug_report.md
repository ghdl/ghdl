---
name: Bug report
about: Create a report to help us improve
---

**Description**
A clear and concise description of what the issue is about.

**Expected behaviour**
What you expected to happen, and what is happening instead.

**How to reproduce?**
Tell us how to reproduce this issue. Please provide a Minimal Working Example (MWE). With sample code it's easier to reproduce the bug and it's much faster to fix it. For example:

```vhd :file: ent.vhd
entity ent is
end entity;

architecture a of ent is
begin
  process begin
    report "Hello world" severity note;
    wait;
  end process;
end;
```

```sh :image: ghdl/ghdl:bullseye-mcode
ghdl -a ent.vhd
ghdl --elab-run ent
```

> NOTE: `:file:` and `:image:` identifiers are specific to [issue-runner](https://github.com/1138-4EB/issue-runner). We suggest to use these, since it allows continuous integration workflows to automatically test the MWE. Using `ghdl/ghdl:*` docker images to run the MWEs ensures that the latest available GHDL is used.

> NOTE: Large files can be uploaded one-by-one or in a tarball/zipfile.

**Context**
Please, provide the following information:

- OS:
- Origin:
  - [ ] Package manager: `version`
  - [ ] Released binaries: `tarball_url`
  - [ ] Built from sources: `commit SHA`

If a `GHDL Bug occurred` block is shown in the log, please paste it here:

```
******************** GHDL Bug occurred ***************************
Please report this bug on https://github.com/ghdl/ghdl/issues
...
******************************************************************
```

**Additional context**
Add any other context about the problem here. If applicable, add screenshots to help explain your problem.