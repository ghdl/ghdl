package pkg is
	type rec is record
		elem : bit_vector;
	end record;

	type arr is array(natural range <>) of rec;
end package;

use work.pkg.all;

entity e is
	port (
		p : in arr
	);
end entity;

architecture a of e is
	signal sig : p'subtype;
begin

end architecture;

entity top is
end top;

use work.pkg.all;

architecture behav of top is
  signal s : arr (1 to 4)(elem(7 downto 0));
begin
  dut : entity work.e port map (p => s);
end behav;
