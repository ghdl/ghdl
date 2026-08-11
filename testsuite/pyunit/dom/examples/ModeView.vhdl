-- Author: Patrick Lehmann
--
-- A VHDL-2019 mode view. Only parseable under --std=19.
package ModeViewPkg is
	type RecordType is record
		a : bit;
		b : bit;
	end record;

	view MasterView of RecordType is
		a : out;
		b : in;
	end view;
end package ModeViewPkg;
