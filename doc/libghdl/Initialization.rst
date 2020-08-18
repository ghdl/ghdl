.. _LIB:Initialization:

Initialization
##############

To be able to use `libghdl`, it first must be initialized.

* Why does it must be initialized?
* What does the initialization do?
* Is there any use case where someone would want to use libghdl without initialization?
* If no, then why it does not initialize itself automatically?
* What functions need to be called during initialization? In Python bindings 3 functions are called:

.. code-block:: python

   libghdl.libghdl_init()
   libghdl.libghdl__set_hooks_for_analysis()

   # Set the prefix in order to locate the vhdl libraries.
   libghdl.libghdl__set_exec_prefix(
       *_to_char_p(dirname(dirname(_libghdl_path)).encode('utf-8')))
