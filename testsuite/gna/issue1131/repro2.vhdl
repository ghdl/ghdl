library IEEE;
use IEEE.Std_Logic_1164.all;

entity repro2 is
	generic (
		BUS_WIDTH : integer := 8
	);
end entity;

architecture Behav of repro2 is
	type BusT is record
		A : std_logic_vector;
		F : std_logic_vector;
	end record;

	signal BusInst : BusT(
		A(BUS_WIDTH-1 downto 0),
		F(3 downto 0)
                );

        type bust_Arr is array (natural range <>) of bust;

        subtype my_bust_arr is bust_arr (0 to 1)(a(3 downto 0), f(2 downto 0));

        signal barr1 : my_bust_arr;
        signal barr2 : bust_arr (1 downto 0)(a(3 downto 0), f(3 downto 0));
	signal s : bit;
begin
  s <= '1' after 1 ns, '0' after 2 ns;
  businst.f(0) <= '1' after 200 ps;
  barr1(1).a(2) <= '1' after 200 ps;
end architecture;
