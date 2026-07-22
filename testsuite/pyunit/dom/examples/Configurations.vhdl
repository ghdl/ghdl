-- Author: Patrick Lehmann
--
-- Configurations: block/component configurations, configuration specifications, and all three
-- entity aspect forms (entity, configuration, open), plus the all/others instantiation list forms.
entity Sub is
end entity Sub;

architecture rtl of Sub is
begin
end architecture rtl;

configuration BaseCfg of Sub is
	for rtl
	end for;
end configuration BaseCfg;

entity Consumer is
end entity Consumer;

architecture rtl of Consumer is
	component SubComp is
	end component SubComp;

	for U4 : SubComp use entity work.Sub(rtl);
begin
	U1 : SubComp;
	U2 : SubComp;
	U3 : SubComp;
	U4 : SubComp;
end architecture rtl;

configuration Cfg of Consumer is
	for rtl
		for U1 : SubComp
			use configuration work.BaseCfg;
		end for;
		for U2 : SubComp
			use open;
		end for;
		for others : SubComp
			use entity work.Sub(rtl);
		end for;
	end for;
end configuration Cfg;
