library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity phonybench is
	generic (
		GENSTR   : string := "adrien";
		GENSTDLV : std_logic_vector(5 downto 0) := "111000";
		GENSTDL  : std_logic := '1';
		GENNAT   : natural := 22
	);
end phonybench;

architecture bench of phonybench is

	type char2std_t is array(character) of std_ulogic;

	constant char2std_c : char2std_t := (
		'U' => 'U',
		'X' => 'X',
		'0' => '0',
		'1' => '1',
		'Z' => 'Z',
		'W' => 'W',
		'L' => 'L',
		'H' => 'H',
		'-' => '-',
		others => 'X'
	);

	function str2std(arg : string) return std_logic_vector is
		variable result : std_logic_vector(arg'length - 1 downto 0);
		variable j : integer;
	begin
		j := arg'length - 1;
		for i in arg'range loop
			result(j) := char2std_c(arg(i));
			j := j - 1;
		end loop;
		return result;
	end function;

	signal sigvec1  : std_logic_vector(5 downto 0) := str2std(GENSTR);
	signal sigvec2  : std_logic_vector(5 downto 0) := GENSTDLV;
	signal siglog   : std_logic := GENSTDL;
	signal signat   : natural := GENNAT;

	signal clk : std_logic := '0';

begin

	clk <= not clk after 5 ms;

	sigvec1 <= str2std(GENSTR);
	sigvec2 <= GENSTDLV;
	siglog  <= GENSTDL;
	signat  <= GENNAT;

end architecture;
