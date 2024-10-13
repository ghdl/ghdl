.. # Load pre-defined aliases and graphical characters like © from docutils
   # <file> is used to denote the special path
   # <Python>\Lib\site-packages\docutils\parsers\rst\include
.. include:: <isonum.txt>
.. include:: <mmlalias.txt>

.. # define a hard line break for HTML
.. |br| raw:: html

   <br />

.. # define horizontal line for HTML
.. |hr| raw:: html

   <hr />

.. # define additional CSS based styles and ReST roles for HTML
.. raw:: html

   <style type="text/css">
     span.bolditalic {font-weight: bold; font-style: italic; }
     span.underline {text-decoration: underline; }
     span.strike {text-decoration: line-through; }
     span.xlarge {font-size: x-large; }
     span.colorred {color: #CC0000; }
     span.colorgreen {color: #009933; }
     span.colorblue {color: #0066FF; }
     span.colorpurple {color: #9900CC; }
   </style>

.. role:: bolditalic
   :class: bolditalic

.. role:: underline
   :class: underline

.. role:: strike
   :class: strike

.. role:: xlarge
   :class: xlarge

.. role:: red
   :class: colorred
.. role:: green
   :class: colorgreen
.. role:: blue
   :class: colorblue
.. role:: purple
   :class: colorpurple

.. role:: deletion
   :class: colorred strike
.. role:: addition
   :class: colorgreen

.. role:: pycode(code)
   :language: python
   :class: highlight
