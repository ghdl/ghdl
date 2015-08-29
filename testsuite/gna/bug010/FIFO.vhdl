library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity FIFO is

	generic(Depth : integer := 3);

	port(
		iClk	: in std_logic;
		iReset	: in std_logic;
		-- write port
		iWrEn		: in std_logic;
		iData		: in std_logic_vector(7 downto 0);
		oHasSpace	: out std_logic;
		-- read port
		iRdEn		: in std_logic;
		oData		: out std_logic_vector(7 downto 0);
		oHasData	: out std_logic
	);
end FIFO;

architecture behaviour of FIFO is

	constant DMSB	: integer := Depth - 1;
	constant Size	: integer := 2 ** DEPTH;

	type regArrayT is array(0 to Size-1) of std_logic_vector(7 downto 0);
	
	signal free					: unsigned(Depth downto 0) := (others => '0');
	signal rIdx, wIdx			: unsigned(DMSB downto 0) := (others => '0');
	signal regArray				: regArrayT;
	signal rdEn, wrEn			: std_logic;
	signal hasData, hasSpace	: std_logic;
	
begin

	oData <= regArray(to_integer(rIdx));
	hasData <= '0' when free = Size else '1';
	oHasData <= hasData;

	hasSpace <= '0' when free = to_unsigned(0, Depth) else '1';
	oHasSpace <= hasSpace;
	
	rdEn <= iRdEn and hasData;
	wrEn <= iWrEn and hasSpace;

	main: process(iClk) begin
		if iClk'event and iClk = '1' then
			if iReset = '1' then
				free <= to_unsigned(Size, Depth + 1);
				rIdx <= (others => '0');
				wIdx <= (others => '0');
			elsif wrEn = '1' and rdEn = '1' then
				rIdx <= rIdx + 1;
				regArray(to_integer(wIdx)) <= iData;
				wIdx <= wIdx + 1;
			elsif rdEn = '1' then
				rIdx <= rIdx + 1;
				free <= free + 1;
			elsif wrEn = '1' then
				regArray(to_integer(wIdx)) <= iData;
				wIdx <= wIdx + 1;
				free <= free - 1;
			end if;
		end if;
	end process;
	
end behaviour;
