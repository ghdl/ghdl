library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

-- equal speed writer and reader
-- faster writer
-- faster reader

entity TestFIFO is
end TestFIFO;


architecture behaviour of TestFIFO is

	component FIFO is
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
	end component;
	
	type ramType is array(0 to 255) of std_logic_vector(7 downto 0);
	
	signal clk, reset			: std_logic;
	signal wrEn, rdEn			: std_logic;
	signal inData, outData		: std_logic_vector(7 downto 0);
	signal hasSpace, hasData	: std_logic;
	
	signal rawInData		: std_logic_vector(7 downto 0);
	signal rawWrEn, rawRdEn	: std_logic;

	signal dstRAM	: ramType;
	
	signal writerData		: unsigned(8 downto 0) := (others => '0');
	signal writerWrEn		: std_logic;
	signal writeDone		: std_logic;
	signal writerClkCntInit, readerClkCntInit	: unsigned(1 downto 0) := "00";
	signal writerClkCnt, readerClkCnt			: unsigned(1 downto 0) := "00";

	signal readerCnt		: unsigned(8 downto 0) :=  (others => '0');
	signal readerRdEn		: std_logic;
	signal readDone			: std_logic;
	
	signal rawMode	: std_logic;
	

begin

	FIFO0: FIFO port map(
		iClk => clk, iReset => reset,
		iWrEn => wrEn, iData => inData, oHasSpace => hasSpace,
		iRdEn => rdEn, oData => outData, oHasData => hasData
	);
	
	wrEn <= rawWrEn when rawMode = '1' else writerWrEn;
	rdEn <= rawRdEn when rawMode = '1' else readerRdEn; 
	
	inData <= rawInData when rawMode = '1' else std_logic_vector(writerData(7 downto 0));
	writeDone <= writerData(7);	-- done when writerData == 0x100
	writerWrEn <= '1' when (writerClkCnt = "00") and (hasSpace = '1') and (writeDone = '0') else '0';
	
	fifoWriter: process(clk)
	begin
		if clk'event and clk = '1' and reset = '0' then
			if writerClkCnt = "00" then
				if hasSpace = '1' and writeDone = '0' then
					writerData <= writerData + 1;
				end if;
				report "writerClkCnt <= writerClkCntInit;";
				writerClkCnt <= writerClkCntInit;
			else
				report "writerClkCnt <= writerClkCnt - 1;";
				writerClkCnt <= writerClkCnt - 1;
			end if;
		end if;
	end process;
	
	readDone <= readerCnt(7);	-- done when readerCnt == 0x100
	readerRdEn <= '1' when readerClkCnt = "00" and hasData = '1' and readDone = '0' else '0';
	
	fifoReader: process(clk)
	begin
		if clk'event and clk = '1' and reset = '0' then
			if readerClkCnt = "00" then
				if hasData = '1' and readDone = '0' then
					dstRAM(to_integer(readerCnt)) <= outData;
					readerCnt <= readerCnt + 1;
				end if;
				readerClkCnt <= readerClkCntInit;
			else
				readerClkCnt <= readerClkCnt - 1;
			end if;
		end if;
	end process;
	
	main: process
		
		procedure nextCycle is begin
			wait for 500 ns;
			report "rise";
			clk <= '1';
			wait for 500 ns;
			clk <= '0';
		end procedure;
		
		procedure doReset is begin
			reset <= '1';
			clk <= '0';
			nextCycle;
			reset <= '0';
		end procedure;
		
		procedure testSimple is
		
			procedure write(data : std_logic_vector(7 downto 0); expHasData, expHasSpace: std_logic) is begin
				rawWrEn <= '1';
				rawInData <= data;
				nextCycle;
				assert hasData = expHasData and hasSpace = expHasSpace;
			end procedure;

		begin
			rawMode <= '1';
			rawWrEn <= '0';
			rawRdEn <= '0';
			doReset;
			assert hasData = '0' and hasSpace = '1';
			write(X"70", '1', '1');
			write(X"61", '1', '1');
			write(X"52", '1', '1');
			write(X"43", '1', '1');
			write(X"34", '1', '1');
			write(X"25", '1', '1');
			write(X"16", '1', '1');
			write(X"07", '1', '0');
			write(X"5a", '1', '0');	-- attempt to write to the full FIFO, should be ignored
		end procedure;
		
		procedure clearDst is
			variable i : integer;
		begin
			for i in 0 to 255 loop
				dstRAM(i) <= X"00";
			end loop;
		end procedure;
		
		procedure checkDst is
			variable i : integer;
		begin
			for i in 0 to 255 loop
				assert dstRAM(i) = std_logic_vector(to_unsigned(i, 8));
			end loop;
		end procedure;

		-- 0 means fastest
		procedure setWriterSpeed(div: integer) is begin
			writerClkCntInit <=  to_unsigned(div, 2);
			writerClkCnt <= to_unsigned(div, 2);
			writerData <= (others => '0');
		end procedure;
		
		procedure setReaderSpeed(div: integer) is begin
			readerClkCntInit <= to_unsigned(div, 2);
			readerClkCnt <= to_unsigned(div, 2);
		end procedure;
	
		procedure testEqualFastSpeed is begin
			setWriterSpeed(0);
			setReaderSpeed(0);
			clearDst;
			doReset;
			while readDone = '0' loop
				nextCycle;
			end loop;
			checkDst;
		end procedure;

	begin
		testSimple;
		-- testEqualFastSpeed;
		wait; -- wait forever, end simulation
	end process;
	
end behaviour;
