library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

entity bug is
	generic(
		LEN : positive := 32;
        POS : natural := 10
	);
	port(
		output  : out signed(LEN-1 downto 0)
	);

    function GET_NUM_FUNC(n : natural) return integer is
	variable result	:integer;
	begin
		case n is
			when 0 => result := 16#010020#;
			when 1 => result := 16#020D30#;
			when 2 => result := 16#00FC1#;
			when 3 => result := 16#05010#;
			when 4 => result := 16#02800#;
			when 5 => result := 16#01400#;
			when 6 => result := 16#002F#;
			when 7 => result := 16#0508#;
			when 8 => result := 16#0200#;
			when 9 => result := 16#0100#;
			when 10 => result := 16#0A0#;
			when others => result := 16#0#;
		end case;
		return result;
	end GET_NUM_FUNC;

end bug;

architecture behav of bug is
begin
	output <= conv_signed(GET_NUM_FUNC(POS), LEN);
end architecture;
