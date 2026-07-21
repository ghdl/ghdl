-- Author: Patrick Lehmann
--
-- VHDL-2019 mode views: simple/composite elements, multiple identifiers sharing one mode,
-- 'converse, and view-typed ports/parameters.
package ModeViews is
	type InnerRecord is record
		x : bit;
		y : bit;
	end record;

	type OuterRecord is record
		a : bit;
		b : bit;
		c : InnerRecord;
	end record;

	view InnerView of InnerRecord is
		x : out;
		y : in;
	end view;

	view OuterView of OuterRecord is
		a, b : out;
		c    : view InnerView;
	end view;
end package ModeViews;

use work.ModeViews.all;

entity Consumer is
	port (
		p1 : view OuterView;
		p2 : view OuterView'converse
	);
end entity Consumer;

architecture rtl of Consumer is
	procedure proc(signal s : view OuterView) is
	begin
	end procedure;
begin
end architecture rtl;
