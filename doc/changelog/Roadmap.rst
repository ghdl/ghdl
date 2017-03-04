.. _CHANGE:Roadmap:

Roadmap | Future Improvements
#############################

I have several axes for `GHDL` improvements:

* Documentation.
* Better diagnostics messages (warning and error).
* Full support of VHDL-2008.
* Optimization (simulation speed).
* Graphical tools (to see waves and to debug)
* Style checks
* VITAL acceleration

TODOs
=====

- RTD builds fail if EPUB is activated.
- Convert VendorPrimitives Markdown to RST
- SVG images are not shown in the PDF. That's because LaTeX is used. Can any package be added to allow so?

Options shown in the command line help, but not found in the doc:

* :samp:`--syn-binding         use synthesis default binding rule`

* VPI Commands

	* :samp:`--vpi=FILENAME        load VPI module`
	* :samp:`--vpi-trace[=FILE]    trace vpi calls to FILE`
	
.. todolist::