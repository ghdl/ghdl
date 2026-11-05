package DeclarationDocumentation is
	--! Constant before.
	constant C1 : natural := 1;
	constant C2 : natural := 2;  --! Constant same line.

	--! Function before.
	function F(
		--! Parameter before.
		X : bit;
		Y : bit;  --! Parameter same line.
		Z : bit
	) return bit;

	--! Procedure before.
	procedure P;
	procedure Q;  --! Procedure same line, no parameters.

	type R is record
		--! Element before.
		A : bit;
		B : bit;  --! Element same line.
	end record;
end package;

package body DeclarationDocumentation is
	--! Body of F.
	function F(X : bit; Y : bit; Z : bit) return bit is
	begin
		return X;
	end function;

	procedure P is
	begin
	end procedure;

	procedure Q is
	begin
	end procedure;
end package body;

architecture rtl of InterfaceDocumentation is
	--! Architecture inside.

	--! Signal before.
	signal S1 : bit;
	signal S2 : bit;  --! Signal same line.

	--! Signal list.
	signal S3, S4 : bit;
begin
	--! Process before.
	Proc : process
		--! Variable before.
		variable V : bit;
	begin
		wait;
	end process;
end architecture;
