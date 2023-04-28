package pkg2 is
	type rec is record
		elem : bit_vector;
	end record;

	type arr is array(natural range <>) of rec;
end package;

use work.pkg2.all;

entity e2 is
	port (
		p : in rec
	);
end entity;

architecture a of e2 is
	signal sig : p'subtype;
begin

end architecture;

entity top2 is
end top2;

use work.pkg2.all;

architecture behav of top2 is
  signal s : rec (elem(7 downto 0));
begin
  dut : entity work.e2 port map (p => s);
end behav;
