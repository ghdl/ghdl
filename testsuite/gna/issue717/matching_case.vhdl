library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity CounterWithReset is
	generic (gWidth : natural := 16);
	port(
		iClk : in std_ulogic;
		inRstAsync : in std_ulogic;
		iCtrl : in std_ulogic_vector (2 downto 0);
		oData : out unsigned(gWidth-1 downto 0)
	);
end entity CounterWithReset;

architecture RTL of CounterWithReset is
	signal CountStateM, CountStateNextM : unsigned(oData'RANGE);
	signal CountState, CountStateNext : unsigned(oData'RANGE);
	signal Ctrl : std_ulogic_vector(iCtrl'RANGE);
    signal a, b : std_ulogic ;
begin
	count : process (iClk, inRstAsync) is
	begin
		if inRstAsync = '0' then
			CountState <= (others=>'0');
			Ctrl <= (others => '0');
		elsif rising_edge(iClk) then
			CountState <= CountStateNext;
			Ctrl <= iCtrl;
		end if;
	end process;
	
	counter : process(Ctrl, CountState) is
	begin
		CountStateNext <= CountState;
		--our counter has 3 control bits
		--the first(left) bit is enable
		--the other two bits control the mode
		case ?(ctrl) is
			when "0--" => --enable bit is not set. we don't count
			when "100" => CountStateNextM <= CountStateM + 1; --count up
			when "101" => CountStateNextM <= CountStateM + 2; --count up 2
			when "110" => CountStateNextM <= CountStateM - 1; --count down
			when "111" => CountStateNextM <= CountStateM - 2; --count down 2
			when others => CountStateNextM <= (others=>'X');
		end case?;
		case ctrl is
			when "0--" => --enable bit is not set. we don't count
			when "100" => CountStateNext <= CountState + 1; --count up
			when "101" => CountStateNext <= CountState + 2; --count up 2
			when "110" => CountStateNext <= CountState - 1; --count down
			when "111" => CountStateNext <= CountState - 2; --count down 2
			when others => CountStateNext <= (others=>'X');
		end case;
	end process;
	
	oData <= CountState;
end architecture RTL;
