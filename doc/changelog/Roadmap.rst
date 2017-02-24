.. _CHANGE:Roadmap:

Roadmap | Future improvements
############

I have several axes for `GHDL` improvements:

* Documentation.
* Better diagnostics messages (warning and error).
* Full support of VHDL-2008.
* Optimization (simulation speed).
* Graphical tools (to see waves and to debug)
* Style checks
* VITAL acceleration

TODOs
=================

- RTD builds fail if EPUB is activated.
- Convert VendorPrimitives Markdown to RST
- SVG images are not shown in the PDF. That's because LaTeX is used. Can any package be added to allow so?

Options shown in the command line help, but not found in the doc:

* :samp:`--expect-failure      invert exit status`
* :samp:`--has-feature=X       test presence of feature X`
* :samp:`--list-features       display the list of features`
* :samp:`--vpi=FILENAME        load VPI module`
* :samp:`--vpi-trace[=FILE]    trace vpi calls to FILE`
* :samp:`--dump-rti            dump Run Time Information`
* :samp:`--trace-signals       disp signals after each cycle`
* :samp:`--trace-processes     disp process name before each cycle`
* :samp:`--stats               display run-time statistics`
* :samp:`--disp-order          disp signals order`
* :samp:`--disp-sources        disp sources while displaying signals`
* :samp:`--disp-sig-types      disp signal types`
* :samp:`--disp-signals-map    disp map bw declared sigs and internal sigs`
* :samp:`--disp-signals-table  disp internal signals`
* :samp:`--checks              do internal checks after each process run`
* :samp:`--activity=LEVEL      watch activity of LEVEL signals: LEVEL is all, min (default) or none (unsafe)`
* :samp:`-C  --mb-comments     allow multi-bytes chars in a comment`

	* https://github.com/tgingold/ghdl/issues/132
	
* :samp:`--bootstrap           allow --work=std`
* :samp:`--syn-binding         use synthesis default binding rule`

.. todolist::