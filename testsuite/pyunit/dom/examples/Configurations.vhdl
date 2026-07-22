-- Author: Patrick Lehmann
--
-- Configurations: block/component configurations, configuration specifications, and all three
-- entity aspect forms (entity, configuration, open), plus the all/others instantiation list forms.
entity Sub is
end entity Sub;

architecture Behav of Sub is
begin
end architecture Behav;

configuration BaseCfg of Sub is
	for Behav
	end for;
end configuration BaseCfg;

entity Consumer is
end entity Consumer;

architecture Rtl of Consumer is
	component SubComp is
	end component SubComp;

	for U4 : SubComp use entity work.Sub(Behav);
begin
	U1 : SubComp;
	U2 : SubComp;
	U3 : SubComp;
	U4 : SubComp;
end architecture Rtl;

configuration Cfg of Consumer is
	for Rtl
		for U1 : SubComp
			use configuration work.BaseCfg;
		end for;
		for U2 : SubComp
			use open;
		end for;
		for others : SubComp
			use entity work.Sub(Behav);
		end for;
	end for;
end configuration Cfg;
