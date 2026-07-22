-- Author: Patrick Lehmann
--
-- Generate statement alternative labels: previously always read as an empty string, regardless of
-- whether the source gave one or not.
entity GenerateLabels is
end entity GenerateLabels;

architecture rtl of GenerateLabels is
begin
	gen1: if label_a : true generate
	begin
	end generate;

	gen2: if false generate
	elsif label_b : true generate
	begin
	else label_c : generate
	begin
	end generate;

	gen3: case 1 generate
	when case_label : 1 =>
	begin
	when others_label : others =>
	begin
	end generate;
end architecture rtl;
