library ieee;
use ieee.std_logic_1164.all;

package ram_pkg is
    generic (
        N : positive := 8;
        A : positive := 3
    );

	constant CONST_A: natural := A;
	constant CONST_N: natural := N;

    subtype ram_word is std_logic_vector(N-1 downto 0);
	subtype addr_word is std_logic_vector(A-1 downto 0);
    type memory is array(0 to (2**A) - 1) of ram_word;

end package ram_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram is
generic(
	N: natural := 8;
	A: natural := 3;
	LOG_EN: boolean := true;
	package local_pkg is new work.ram_pkg
		generic map (N=>N, A=>A));
port(
	i_clk, i_rst: in std_logic;
	i_we: in std_logic;
	i_din: in local_pkg.ram_word := (others=>'0');
	o_dout: out local_pkg.ram_word := (others=>'0');
	i_addr: in local_pkg.addr_word := (others=>'0'));
end ram;

architecture behav of ram is
	signal ram256: local_pkg.memory;

begin
	process (i_clk, i_rst)
		variable ram_addr_in: natural range 0 to local_pkg.memory'length - 1;
	begin
		if i_rst = '1' then
			ram256 <= (others=>(others=>'0'));
			o_dout <= (others=>'0');
		elsif rising_edge(i_clk) then
			ram_addr_in := to_integer(unsigned(i_addr));

			if i_we = '1' then
				ram256 (ram_addr_in) <= i_din ;
			end if;

			o_dout <= ram256 (ram_addr_in);

		end if;

	end process;

end architecture;
