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

* RTD builds fail if EPUB is activated.
* Convert VendorPrimitives Markdown to RST
* SVG images are not shown in the PDF. That's because LaTeX is used. Can any package be added to allow so?
* Create a fancy Shield with a ZIP/TGZ icon, that points to the source code zip and tar.gz/tgz:

   * https://github.com/tgingold/ghdl/archive/2017-03-01.zip
   * https://github.com/tgingold/ghdl/archive/2017-03-01.tar.gz

* The URL of some sections does not match the position in the hierarchy shown in the side menu. We have several options:

   * Leave it as it is now.
   * Create a new chapter in the side menu, named 'Building GHDL' and move 'Builing' and 'Precompile Vendor Primitives' there.
   * Move 'Releases' to ch Introduction, renamed to 'Getting'. And keep 'Building', 'Vendor...' and 'Docker' in the same chapter, renamed to 'Building'.
   
   We are going to write more content first, and see how much we get. We'll reorder afterwards.
   
   [@Paebbels] I have no problem if a chapter has different URLs for sections. Users use either the web-gui / navigation bar or remember an url in the browser history, the will not see the discrepancy because the use only one access way.
   
Options shown in the command line help, but not found in the doc:

* ``--syn-binding         use synthesis default binding rule``

* VPI Commands

	* ``--vpi=FILENAME        load VPI module``
	* ``--vpi-trace[=FILE]    trace vpi calls to FILE``
	
.. todolist::