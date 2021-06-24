library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

entity entity_1 is
	generic (
		FREQ : real     := 100.0;
		BITS : positive := 8
	);
	port (
	  Clock: in  std_logic;
	  Reset: in  std_logic := '0';
	  Q:     out std_logic_vector(BITS - 1 downto 0)
	);

	constant fire : boolean := True;
begin
	wood <= fire;
end entity entity_1;

architecture behav of entity_1 is
	constant MAX : positive := -25;
	signal rst : std_logic := foo('U');
	signal vec : bit_vector(pack(3 to 2).signaal'range'value);

	type newInt is range -4 to 3;
	type newFp is range 4.3 downto -3.9;
	type arr is array(natural range <>, enum range <>) of integer(3 downto 0);
	type rec is record
		elem1 : bit;
		elem2 : boolean;
		elem3 : integer_vector(3 downto 0);
		elem4 : natural range 7 to 8;
	end record;
	type enum is (e1, e2, e3);
	type acc is access bar;
	subtype uint8 is integer range 0 to 255;

--	file f : text;

	function func (a : integer; b : boolean) return bit is
	begin

	end function;

  shared variable pt_var : lib.pack.prot;

	procedure proc(spam : egg) is
	begin

	end procedure;

	alias bar is boolean;
begin
	process(Clock)
	begin
		if rising_edge(Clock) then
			if Reset = '1' then
				Q <= (others => '0');
			else
				Q <= std_logic_vector(unsigned(Q) + 1);
			end if;
		end if;
	end process;
end architecture behav;

package package_1 is
	constant ghdl : float := (3, 5, 0 to 2 => 5, 3 => 4, name => 10); -- 2.3;

	component comp is
		port (
			clk : std
		);
	end component;
end package;

package body package_1 is
	constant ghdl : float := (1); -- => 2, 4 => 5, others => 10); -- .5;
end package body;
