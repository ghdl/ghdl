.. program:: ghdl
.. _REF:ImplVITAL:

****************************
Implementation of VITAL
****************************

.. index:: VITAL

.. index:: IEEE 1076.4

.. index:: 1076.4

This chapter describes how VITAL is implemented in GHDL. Support of VITAL is
really in a preliminary stage. Do not expect too much of it as of right now.

.. _vital_packages:

VITAL packages
==============

The VITAL standard or IEEE 1076.4 was first published in 1995, and revised in
2000.

The version of the VITAL packages depends on the VHDL standard. VITAL
1995 packages are used with the VHDL 1987 standard, while VITAL 2000
packages are used with other standards. This choice is based on the
requirements of VITAL: VITAL 1995 requires the models follow the VHDL
1987 standard, while VITAL 2000 requires the models follow VHDL 1993.

The VITAL 2000 packages were slightly modified so that they conform to
the VHDL 1993 standard (a few functions are made pure and a few
impure).

.. _vhdl_restrictions_for_vital:

VHDL restrictions for VITAL
===========================

The VITAL standard (partially) implemented is the IEEE 1076.4 standard
published in 1995.

This standard defines restriction of the VHDL language usage on VITAL
model. A :dfn:`VITAL model` is a design unit (entity or architecture)
decorated by the `VITAL_Level0` or `VITAL_Level1` attribute.
These attributes are defined in the `ieee.VITAL_Timing` package.

Currently, only VITAL level 0 checks are implemented. VITAL level 1 models
can be analyzed, but GHDL doesn't check they comply with the VITAL standard.

Moreover, GHDL doesn't check (yet) that timing generics are not read inside
a VITAL level 0 model prior the VITAL annotation.

The analysis of a non-conformant VITAL model fails. You can disable the
checks of VITAL restrictions with the *--no-vital-checks*. Even when
restrictions are not checked, SDF annotation can be performed.

.. _backannotation:

Backannotation
==============

.. index:: SDF

:dfn:`Backannotation` is the process of setting VITAL generics with timing
information provided by an external files.

The external files must be SDF (Standard Delay Format) files. GHDL
supports a tiny subset of SDF version 2.1. Other version numbers can be
used, provided no features added by later versions are used.

Hierarchical instance names are not supported. However you can use a list of
instances. If there is no instance, the top entity will be annotated and
the celltype must be the name of the top entity. If there is at least one
instance, the last instance name must be a component instantiation label, and
the celltype must be the name of the component declaration instantiated.

Instances being annotated are not required to be VITAL compliant. However
generics being annotated must follow rules of VITAL (e.g., type must be a
suitable vital delay type).

Currently, only timing constraints applying on a timing generic of type
`VitalDelayType01` has been implemented. This SDF annotator is
just a proof of concept. Features will be added with the following GHDL
release.

Negative constraint calculation
===============================

Negative constraint delay adjustments are necessary to handle negative
constraints such as a negative setup time. This step is defined in the VITAL
standard and should occur after backannotation.

GHDL does not do negative constraint calculation. It fails to handle models
with negative constraint. I hope to be able to add this phase soon.
