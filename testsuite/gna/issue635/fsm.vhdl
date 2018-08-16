library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.float_pkg.all;
--   %3 = alloca [3 x [4 x i32]], align 16
entity Ram0 is
	generic
	(
		addressWidth : in positive;
		busWidth : in positive;
		size : in positive
	);
	port
	(
		clk : in std_logic;
		address : in unsigned(addressWidth - 1 downto 0);
		writeEnable : in std_logic;
		dataIn : in std_logic_vector(busWidth - 1 downto 0);
		dataOut : out std_logic_vector(busWidth - 1 downto 0)
	);
end Ram0;

architecture Behavioral of Ram0 is
	constant alignment : positive := busWidth / 8;
	constant ramSize : positive := size / alignment;

	type RamType is array(natural range <>) of std_logic_vector(busWidth - 1 downto 0);
	subtype RamRange is natural range 0 to ramSize;

	signal ram : RamType(RamRange) := (
		0 => "00000000000000000000000000000001",
		1 => "00000000000000000000000000000010",
		2 => "00000000000000000000000000000011",
		3 => "00000000000000000000000000000100",
		4 => "00000000000000000000000000000010",
		5 => "00000000000000000000000000000011",
		6 => "00000000000000000000000000000100",
		7 => "00000000000000000000000000000101",
		8 => "00000000000000000000000000000011",
		9 => "00000000000000000000000000000100",
		10 => "00000000000000000000000000000101",
		11 => "00000000000000000000000000000110",
		others => "00000000000000000000000000000000");
begin
	process(clk)
		variable index : RamRange;
	begin
		if (rising_edge(clk))
		then
			index := to_integer(address) / alignment;

			if (writeEnable = '1')
			then
				ram(index) <= dataIn;
			end if;

			dataOut <= ram(index);
		end if;
	end process;
end Behavioral;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.float_pkg.all;
--   %4 = alloca [2 x [2 x [2 x i32]]], align 16
entity Ram1 is
	generic
	(
		addressWidth : in positive;
		busWidth : in positive;
		size : in positive
	);
	port
	(
		clk : in std_logic;
		address : in unsigned(addressWidth - 1 downto 0);
		writeEnable : in std_logic;
		dataIn : in std_logic_vector(busWidth - 1 downto 0);
		dataOut : out std_logic_vector(busWidth - 1 downto 0)
	);
end Ram1;

architecture Behavioral of Ram1 is
	constant alignment : positive := busWidth / 8;
	constant ramSize : positive := size / alignment;

	type RamType is array(natural range <>) of std_logic_vector(busWidth - 1 downto 0);
	subtype RamRange is natural range 0 to ramSize;

	signal ram : RamType(RamRange) := (
		0 => "00000000000000000000000000000010",
		1 => "00000000000000000000000000000010",
		2 => "00000000000000000000000000000011",
		3 => "00000000000000000000000000000100",
		4 => "00000000000000000000000000000101",
		5 => "00000000000000000000000000000110",
		6 => "00000000000000000000000000000111",
		7 => "00000000000000000000000000001000",
		others => "00000000000000000000000000000000");
begin
	process(clk)
		variable index : RamRange;
	begin
		if (rising_edge(clk))
		then
			index := to_integer(address) / alignment;

			if (writeEnable = '1')
			then
				ram(index) <= dataIn;
			end if;

			dataOut <= ram(index);
		end if;
	end process;
end Behavioral;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.float_pkg.all;
--   %5 = alloca [2 x [4 x double]], align 16
entity Ram2 is
	generic
	(
		addressWidth : in positive;
		busWidth : in positive;
		size : in positive
	);
	port
	(
		clk : in std_logic;
		address : in unsigned(addressWidth - 1 downto 0);
		writeEnable : in std_logic;
		dataIn : in std_logic_vector(busWidth - 1 downto 0);
		dataOut : out std_logic_vector(busWidth - 1 downto 0)
	);
end Ram2;

architecture Behavioral of Ram2 is
	constant alignment : positive := busWidth / 8;
	constant ramSize : positive := size / alignment;

	type RamType is array(natural range <>) of std_logic_vector(busWidth - 1 downto 0);
	subtype RamRange is natural range 0 to ramSize;

	signal ram : RamType(RamRange) := (
		0 => "10011001100110011001100110011010",
		1 => "00111111101110011001100110011001",
		2 => "10011001100110011001100110011010",
		3 => "00111111110010011001100110011001",
		4 => "00110011001100110011001100110011",
		5 => "00111111110100110011001100110011",
		6 => "10011001100110011001100110011010",
		7 => "00111111110110011001100110011001",
		8 => "10011001100110011001100110011010",
		9 => "00111111110010011001100110011001",
		10 => "00110011001100110011001100110011",
		11 => "00111111110100110011001100110011",
		12 => "10011001100110011001100110011010",
		13 => "00111111110110011001100110011001",
		14 => "00000000000000000000000000000000",
		15 => "00111111111000000000000000000000",
		others => "00000000000000000000000000000000");
begin
	process(clk)
		variable index : RamRange;
	begin
		if (rising_edge(clk))
		then
			index := to_integer(address) / alignment;

			if (writeEnable = '1')
			then
				ram(index) <= dataIn;
			end if;

			dataOut <= ram(index);
		end if;
	end process;
end Behavioral;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.float_pkg.all;

entity Function_Z3fooi is
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		input1 : in signed(31 downto 0);
		output : out signed(31 downto 0);
		ready : out std_logic
	);
end Function_Z3fooi;

architecture behavioral of Function_Z3fooi is
	signal stackPointer : unsigned(7 downto 0);

	signal ramAddress : unsigned(7 downto 0);
	signal ramWriteEnable : std_logic;
	signal ramDataIn : std_logic_vector(31 downto 0);
	signal ramDataOut : std_logic_vector(31 downto 0);
	-- signals for Ram0
	signal ramAddress0 : unsigned(7 downto 0);
	signal ramWriteEnable0 : std_logic;
	signal ramDataIn0 : std_logic_vector(31 downto 0);
	signal ramDataOut0 : std_logic_vector(31 downto 0);
	-- signals for Ram1
	signal ramAddress1 : unsigned(7 downto 0);
	signal ramWriteEnable1 : std_logic;
	signal ramDataIn1 : std_logic_vector(31 downto 0);
	signal ramDataOut1 : std_logic_vector(31 downto 0);
	-- signals for Ram2
	signal ramAddress2 : unsigned(7 downto 0);
	signal ramWriteEnable2 : std_logic;
	signal ramDataIn2 : std_logic_vector(31 downto 0);
	signal ramDataOut2 : std_logic_vector(31 downto 0);
begin
	ramInstance : entity work.Ram
	generic map
	(
		addressWidth => 8,
		busWidth => 32,
		size => 256
	)
	port map
	(
		clk => clk,
		address => ramAddress,
		writeEnable => ramWriteEnable,
		dataIn => ramDataIn,
		dataOut => ramDataOut
	);
	ramInstance0 : entity work.Ram0
	generic map
	(
		addressWidth => 8,
		busWidth => 32,
		size => 256
	)
	port map
	(
		clk => clk,
		address => ramAddress0,
		writeEnable => ramWriteEnable0,
		dataIn => ramDataIn0,
		dataOut => ramDataOut0
	);
	ramInstance1 : entity work.Ram1
	generic map
	(
		addressWidth => 8,
		busWidth => 32,
		size => 256
	)
	port map
	(
		clk => clk,
		address => ramAddress1,
		writeEnable => ramWriteEnable1,
		dataIn => ramDataIn1,
		dataOut => ramDataOut1
	);
	ramInstance2 : entity work.Ram2
	generic map
	(
		addressWidth => 8,
		busWidth => 32,
		size => 256
	)
	port map
	(
		clk => clk,
		address => ramAddress2,
		writeEnable => ramWriteEnable2,
		dataIn => ramDataIn2,
		dataOut => ramDataOut2
	);

	process(clk, reset)
		type State is (block1_cycle1, block1_cycle2, block1_cycle3, block1_cycle4, block1_cycle5, block1_cycle6, block1_cycle7, block1_cycle8, block1_cycle9, block1_cycle10, block1_cycle11, block2_cycle1, block2_cycle2, block2_cycle3, block4_cycle1, block5_cycle1, block5_cycle2, block5_cycle3, block7_cycle1, block8_cycle1, block8_cycle2, block8_cycle3, block10_cycle1, block10_cycle2, block10_cycle3, block10_cycle4, block10_cycle5, block10_cycle6, block10_cycle7, block10_cycle8, block11_cycle1, block11_cycle2, block11_cycle3, block9_cycle1, block12_cycle1, block12_cycle2, block12_cycle3, block6_cycle1, block13_cycle1, block13_cycle2, block13_cycle3, block3_cycle1, block14_cycle1, block14_cycle2, block14_cycle3, block16_cycle1, block17_cycle1, block17_cycle2, block17_cycle3, block19_cycle1, block19_cycle2, block19_cycle3, block19_cycle4, block19_cycle5, block19_cycle6, block19_cycle7, block19_cycle8, block19_cycle9, block19_cycle10, block19_cycle11, block19_cycle12, block19_cycle13, block19_cycle14, block19_cycle15, block20_cycle1, block20_cycle2, block20_cycle3, block18_cycle1, block21_cycle1, block21_cycle2, block21_cycle3, block15_cycle1, block22_cycle1, block22_cycle2, block22_cycle3, block24_cycle1, block24_cycle2, block24_cycle3, block24_cycle4, block24_cycle5, block24_cycle6, block25_cycle1, block25_cycle2, block25_cycle3, block23_cycle1, block23_cycle2, block23_cycle3);

		variable lastState, currentState, nextState : State;

		variable retValWritten : std_logic := '0';
		variable variable84 : signed(31 downto 0);
		variable variable82 : signed(7 downto 0);
		variable variable81 : signed(7 downto 0);
		variable variable79 : signed(31 downto 0);
		variable variable15 : signed(31 downto 0);
		variable variable32 : signed(31 downto 0);
		variable variable29 : signed(31 downto 0);
		variable variable52 : signed(7 downto 0);
		variable variable76 : signed(31 downto 0);
		variable variable27 : signed(7 downto 0);
		variable variable38 : signed(7 downto 0);
		variable variable80 : signed(7 downto 0);
		variable variable26 : signed(63 downto 0);
		variable variable22 : signed(0 downto 0);
		variable variable21 : signed(31 downto 0);
		variable variable20 : signed(7 downto 0);
		variable variable18 : signed(31 downto 0);
		variable variable30 : signed(7 downto 0);
		variable variable53 : signed(31 downto 0);
		variable variable19 : signed(0 downto 0);
		variable variable40 : signed(7 downto 0);
		variable variable55 : signed(7 downto 0);
		variable variable25 : signed(7 downto 0);
		variable variable64 : signed(7 downto 0);
		variable variable17 : signed(7 downto 0);
		variable variable6 : signed(7 downto 0);
		variable variable4 : unsigned(7 downto 0);
		variable variable28 : signed(63 downto 0);
		variable variable1 : unsigned(7 downto 0);
		variable variable5 : signed(31 downto 0);
		variable variable75 : signed(63 downto 0);
		variable variable3 : unsigned(7 downto 0);
		variable variable2 : unsigned(7 downto 0);
		variable variable34 : signed(7 downto 0);
		variable variable7 : unsigned(7 downto 0);
		variable variable31 : signed(31 downto 0);
		variable variable16 : signed(0 downto 0);
		variable variable13 : unsigned(7 downto 0);
		variable variable11 : unsigned(7 downto 0);
		variable variable39 : signed(7 downto 0);
		variable variable12 : unsigned(7 downto 0);
		variable variable56 : signed(7 downto 0);
		variable variable37 : signed(7 downto 0);
		variable variable42 : signed(0 downto 0);
		variable variable83 : signed(7 downto 0);
		variable variable67 : signed(7 downto 0);
		variable variable24 : signed(63 downto 0);
		variable variable43 : signed(7 downto 0);
		variable variable59 : signed(63 downto 0);
		variable variable36 : signed(7 downto 0);
		variable variable46 : signed(31 downto 0);
		variable variable10 : unsigned(7 downto 0);
		variable variable68 : signed(7 downto 0);
		variable variable70 : signed(31 downto 0);
		variable variable47 : signed(7 downto 0);
		variable variable33 : signed(7 downto 0);
		variable variable74 : signed(31 downto 0);
		variable variable14 : signed(7 downto 0);
		variable variable48 : signed(31 downto 0);
		variable variable23 : signed(7 downto 0);
		variable variable49 : signed(31 downto 0);
		variable variable50 : signed(7 downto 0);
		variable variable57 : signed(63 downto 0);
		variable variable65 : signed(7 downto 0);
		variable variable51 : signed(31 downto 0);
		variable variable8 : unsigned(7 downto 0);
		variable variable72 : signed(7 downto 0);
		variable variable54 : signed(31 downto 0);
		variable variable45 : signed(0 downto 0);
		variable variable63 : signed(31 downto 0);
		variable variable35 : signed(7 downto 0);
		variable variable58 : signed(7 downto 0);
		variable variable78 : signed(31 downto 0);
		variable variable41 : signed(31 downto 0);
		variable variable60 : signed(31 downto 0);
		variable variable61 : signed(7 downto 0);
		variable variable66 : signed(7 downto 0);
		variable variable69 : signed(7 downto 0);
		variable variable71 : signed(0 downto 0);
		variable variable9 : unsigned(7 downto 0);
		variable variable73 : signed(31 downto 0);
		variable variable62 : signed(31 downto 0);
		variable variable44 : signed(31 downto 0);
		variable variable77 : signed(7 downto 0);
	begin
		if (reset = '1')
		then
			currentState := block1_cycle1;
			stackPointer <= (others => '0');
			ready <= '0';
		elsif (rising_edge(clk))
		then
			-- default values
			ready <= '0';
			ramAddress <= (others => '0');
			ramWriteEnable <= '0';
			ramDataIn <= (others => '0');
			ramAddress0 <= (others => '0');
			ramWriteEnable0 <= '0';
			ramDataIn0 <= (others => '0');
			ramAddress1 <= (others => '0');
			ramWriteEnable1 <= '0';
			ramDataIn1 <= (others => '0');
			ramAddress2 <= (others => '0');
			ramWriteEnable2 <= '0';
			ramDataIn2 <= (others => '0');

			case currentState is
				-- block1
				when block1_cycle1 =>
					-- alloca
					variable1 := stackPointer;
					stackPointer <= stackPointer + 4;
					-- store cycle 0
					ramAddress <= variable1;
					ramDataIn <= std_logic_vector(input1);
					ramWriteEnable <= '1';
					nextState := block1_cycle2;
				when block1_cycle2 =>
					-- alloca
					variable2 := stackPointer;
					stackPointer <= stackPointer + 16;
					-- load cycle 0
					ramAddress <= variable1;
					nextState := block1_cycle3;
				when block1_cycle3 =>
					-- alloca
					variable3 := stackPointer;
					stackPointer <= stackPointer + 16;
					-- load cycle 1
					nextState := block1_cycle4;
				when block1_cycle4 =>
					-- alloca
					variable4 := stackPointer;
					stackPointer <= stackPointer + 16;
					-- load cycle 2
					variable5 := signed(ramDataOut);
					variable6 := resize(variable5, 8);
					nextState := block1_cycle5;
				when block1_cycle5 =>
					-- alloca
					variable7 := stackPointer;
					stackPointer <= stackPointer + 4;
					-- store cycle 0
					ramAddress <= variable7;
					ramDataIn(7 downto 0) <= std_logic_vector(variable6);
					ramWriteEnable <= '1';
					nextState := block1_cycle6;
				when block1_cycle6 =>
					-- alloca
					variable8 := stackPointer;
					stackPointer <= stackPointer + 4;
					-- store cycle 0
					ramAddress <= variable8;
					ramDataIn(7 downto 0) <= std_logic_vector(to_signed(0, 8));
					ramWriteEnable <= '1';
					nextState := block1_cycle7;
				when block1_cycle7 =>
					-- alloca
					variable9 := stackPointer;
					stackPointer <= stackPointer + 4;
					nextState := block1_cycle8;
				when block1_cycle8 =>
					-- alloca
					variable10 := stackPointer;
					stackPointer <= stackPointer + 4;
					nextState := block1_cycle9;
				when block1_cycle9 =>
					-- alloca
					variable11 := stackPointer;
					stackPointer <= stackPointer + 4;
					nextState := block1_cycle10;
				when block1_cycle10 =>
					-- alloca
					variable12 := stackPointer;
					stackPointer <= stackPointer + 4;
					nextState := block1_cycle11;
				when block1_cycle11 =>
					-- alloca
					variable13 := stackPointer;
					stackPointer <= stackPointer + 4;
					nextState := block2_cycle1;
				-- block2
				when block2_cycle1 =>
					-- load cycle 0
					ramAddress <= variable8;
					nextState := block2_cycle2;
				when block2_cycle2 =>
					-- load cycle 1
					nextState := block2_cycle3;
				when block2_cycle3 =>
					-- load cycle 2
					variable14 := signed(ramDataOut(7 downto 0));
					variable15 := signed(resize(unsigned(variable14), 32));
					if (variable15 < to_signed(2, 32)) then
						variable16 := "1";
					else
						variable16 := "0";
					end if;
					if(variable16(0) = '1')then
						nextState := block4_cycle1;
					else
						nextState := block3_cycle1;
					end if;
				-- block4
				when block4_cycle1 =>
					-- store cycle 0
					ramAddress <= variable9;
					ramDataIn(7 downto 0) <= std_logic_vector(to_signed(0, 8));
					ramWriteEnable <= '1';
					nextState := block5_cycle1;
				-- block5
				when block5_cycle1 =>
					-- load cycle 0
					ramAddress <= variable9;
					nextState := block5_cycle2;
				when block5_cycle2 =>
					-- load cycle 1
					nextState := block5_cycle3;
				when block5_cycle3 =>
					-- load cycle 2
					variable17 := signed(ramDataOut(7 downto 0));
					variable18 := signed(resize(unsigned(variable17), 32));
					if (variable18 < to_signed(2, 32)) then
						variable19 := "1";
					else
						variable19 := "0";
					end if;
					if(variable19(0) = '1')then
						nextState := block7_cycle1;
					else
						nextState := block6_cycle1;
					end if;
				-- block7
				when block7_cycle1 =>
					-- store cycle 0
					ramAddress <= variable10;
					ramDataIn(7 downto 0) <= std_logic_vector(to_signed(0, 8));
					ramWriteEnable <= '1';
					nextState := block8_cycle1;
				-- block8
				when block8_cycle1 =>
					-- load cycle 0
					ramAddress <= variable10;
					nextState := block8_cycle2;
				when block8_cycle2 =>
					-- load cycle 1
					nextState := block8_cycle3;
				when block8_cycle3 =>
					-- load cycle 2
					variable20 := signed(ramDataOut(7 downto 0));
					variable21 := signed(resize(unsigned(variable20), 32));
					if (variable21 < to_signed(2, 32)) then
						variable22 := "1";
					else
						variable22 := "0";
					end if;
					if(variable22(0) = '1')then
						nextState := block10_cycle1;
					else
						nextState := block9_cycle1;
					end if;
				-- block10
				when block10_cycle1 =>
					-- load cycle 0
					ramAddress <= variable8;
					nextState := block10_cycle2;
				when block10_cycle2 =>
					-- load cycle 1
					-- load cycle 0
					ramAddress <= variable9;
					nextState := block10_cycle3;
				when block10_cycle3 =>
					-- load cycle 2
					variable23 := signed(ramDataOut(7 downto 0));
					variable24 := signed(resize(unsigned(variable23), 64));
					--   %35 = getelementptr inbounds [2 x [2 x [2 x i32]]], [2 x [2 x [2 x i32]]]* %4, i64 0, i64 %34
					variable3 := unsigned(resize(unsigned(variable24 * 4), 8));
					-- load cycle 1
					-- load cycle 0
					ramAddress <= variable10;
					nextState := block10_cycle4;
				when block10_cycle4 =>
					-- load cycle 2
					variable25 := signed(ramDataOut(7 downto 0));
					variable26 := signed(resize(unsigned(variable25), 64));
					--   %38 = getelementptr inbounds [2 x [2 x i32]], [2 x [2 x i32]]* %35, i64 0, i64 %37
					variable3 := variable3 + unsigned(resize(unsigned(variable26 * 2), 8));
					-- load cycle 1
					nextState := block10_cycle5;
				when block10_cycle5 =>
					-- load cycle 2
					variable27 := signed(ramDataOut(7 downto 0));
					variable28 := signed(resize(unsigned(variable27), 64));
					--   %41 = getelementptr inbounds [2 x i32], [2 x i32]* %38, i64 0, i64 %40
					variable3 := variable3 + unsigned(resize(unsigned(variable28 * 1), 8));
					variable3 := unsigned(resize(unsigned(variable3 * 4), 8));
					-- load cycle 0
					ramAddress1 <= variable3;
					nextState := block10_cycle6;
				when block10_cycle6 =>
					-- load cycle 1
					-- load cycle 0
					ramAddress <= variable7;
					nextState := block10_cycle7;
				when block10_cycle7 =>
					-- load cycle 2
					variable29 := signed(ramDataOut1);
					-- load cycle 1
					nextState := block10_cycle8;
				when block10_cycle8 =>
					-- load cycle 2
					variable30 := signed(ramDataOut(7 downto 0));
					variable31 := signed(resize(unsigned(variable30), 32));
					variable32 := variable31 + variable29;
					variable33 := resize(variable32, 8);
					-- store cycle 0
					ramAddress <= variable7;
					ramDataIn(7 downto 0) <= std_logic_vector(variable33);
					ramWriteEnable <= '1';
					nextState := block11_cycle1;
				-- block11
				when block11_cycle1 =>
					-- load cycle 0
					ramAddress <= variable10;
					nextState := block11_cycle2;
				when block11_cycle2 =>
					-- load cycle 1
					nextState := block11_cycle3;
				when block11_cycle3 =>
					-- load cycle 2
					variable34 := signed(ramDataOut(7 downto 0));
					variable35 := variable34 + to_signed(1, 8);
					-- store cycle 0
					ramAddress <= variable10;
					ramDataIn(7 downto 0) <= std_logic_vector(variable35);
					ramWriteEnable <= '1';
					nextState := block8_cycle1;
				-- block9
				when block9_cycle1 =>
					nextState := block12_cycle1;
				-- block12
				when block12_cycle1 =>
					-- load cycle 0
					ramAddress <= variable9;
					nextState := block12_cycle2;
				when block12_cycle2 =>
					-- load cycle 1
					nextState := block12_cycle3;
				when block12_cycle3 =>
					-- load cycle 2
					variable36 := signed(ramDataOut(7 downto 0));
					variable37 := variable36 + to_signed(1, 8);
					-- store cycle 0
					ramAddress <= variable9;
					ramDataIn(7 downto 0) <= std_logic_vector(variable37);
					ramWriteEnable <= '1';
					nextState := block5_cycle1;
				-- block6
				when block6_cycle1 =>
					nextState := block13_cycle1;
				-- block13
				when block13_cycle1 =>
					-- load cycle 0
					ramAddress <= variable8;
					nextState := block13_cycle2;
				when block13_cycle2 =>
					-- load cycle 1
					nextState := block13_cycle3;
				when block13_cycle3 =>
					-- load cycle 2
					variable38 := signed(ramDataOut(7 downto 0));
					variable39 := variable38 + to_signed(1, 8);
					-- store cycle 0
					ramAddress <= variable8;
					ramDataIn(7 downto 0) <= std_logic_vector(variable39);
					ramWriteEnable <= '1';
					nextState := block2_cycle1;
				-- block3
				when block3_cycle1 =>
					-- store cycle 0
					ramAddress <= variable11;
					ramDataIn(7 downto 0) <= std_logic_vector(to_signed(0, 8));
					ramWriteEnable <= '1';
					nextState := block14_cycle1;
				-- block14
				when block14_cycle1 =>
					-- load cycle 0
					ramAddress <= variable11;
					nextState := block14_cycle2;
				when block14_cycle2 =>
					-- load cycle 1
					nextState := block14_cycle3;
				when block14_cycle3 =>
					-- load cycle 2
					variable40 := signed(ramDataOut(7 downto 0));
					variable41 := resize(variable40, 32);
					if (variable41 < to_signed(1, 32)) then
						variable42 := "1";
					else
						variable42 := "0";
					end if;
					if(variable42(0) = '1')then
						nextState := block16_cycle1;
					else
						nextState := block15_cycle1;
					end if;
				-- block16
				when block16_cycle1 =>
					-- store cycle 0
					ramAddress <= variable12;
					ramDataIn(7 downto 0) <= std_logic_vector(to_signed(0, 8));
					ramWriteEnable <= '1';
					nextState := block17_cycle1;
				-- block17
				when block17_cycle1 =>
					-- load cycle 0
					ramAddress <= variable12;
					nextState := block17_cycle2;
				when block17_cycle2 =>
					-- load cycle 1
					nextState := block17_cycle3;
				when block17_cycle3 =>
					-- load cycle 2
					variable43 := signed(ramDataOut(7 downto 0));
					variable44 := resize(variable43, 32);
					if (variable44 < to_signed(2, 32)) then
						variable45 := "1";
					else
						variable45 := "0";
					end if;
					if(variable45(0) = '1')then
						nextState := block19_cycle1;
					else
						nextState := block18_cycle1;
					end if;
				-- block19
				when block19_cycle1 =>
					--   %69 = getelementptr inbounds [2 x [2 x [2 x i32]]], [2 x [2 x [2 x i32]]]* %4, i64 0, i64 1
					variable3 := resize(unsigned(to_unsigned(4, 64)), 8);
					--   %70 = getelementptr inbounds [2 x [2 x i32]], [2 x [2 x i32]]* %69, i64 0, i64 1
					variable3 := variable3 + resize(unsigned(to_unsigned(2, 64)), 8);
					--   %71 = getelementptr inbounds [2 x i32], [2 x i32]* %70, i64 0, i64 1
					variable3 := variable3 + resize(unsigned(to_unsigned(1, 64)), 8);
					variable3 := unsigned(resize(unsigned(variable3 * 4), 8));
					-- load cycle 0
					ramAddress1 <= variable3;
					--   %77 = getelementptr inbounds [3 x [4 x i32]], [3 x [4 x i32]]* %3, i64 0, i64 2
					variable2 := resize(unsigned(to_unsigned(6, 64)), 8);
					--   %78 = getelementptr inbounds [4 x i32], [4 x i32]* %77, i64 0, i64 3
					variable2 := variable2 + resize(unsigned(to_unsigned(3, 64)), 8);
					variable2 := unsigned(resize(unsigned(variable2 * 4), 8));
					nextState := block19_cycle2;
				when block19_cycle2 =>
					-- load cycle 1
					-- load cycle 0
					ramAddress <= variable7;
					nextState := block19_cycle3;
				when block19_cycle3 =>
					-- load cycle 2
					variable46 := signed(ramDataOut1);
					-- load cycle 1
					nextState := block19_cycle4;
				when block19_cycle4 =>
					-- load cycle 2
					variable47 := signed(ramDataOut(7 downto 0));
					variable48 := signed(resize(unsigned(variable47), 32));
					variable49 := variable48 + variable46;
					variable50 := resize(variable49, 8);
					-- store cycle 0
					ramAddress <= variable7;
					ramDataIn(7 downto 0) <= std_logic_vector(variable50);
					ramWriteEnable <= '1';
					nextState := block19_cycle5;
				when block19_cycle5 =>
					-- load cycle 0
					ramAddress0 <= variable2;
					nextState := block19_cycle6;
				when block19_cycle6 =>
					-- load cycle 1
					-- load cycle 0
					ramAddress <= variable7;
					nextState := block19_cycle7;
				when block19_cycle7 =>
					-- load cycle 2
					variable51 := signed(ramDataOut0);
					-- load cycle 1
					nextState := block19_cycle8;
				when block19_cycle8 =>
					-- load cycle 2
					variable52 := signed(ramDataOut(7 downto 0));
					variable53 := signed(resize(unsigned(variable52), 32));
					variable54 := variable53 + variable51;
					variable55 := resize(variable54, 8);
					-- store cycle 0
					ramAddress <= variable7;
					ramDataIn(7 downto 0) <= std_logic_vector(variable55);
					ramWriteEnable <= '1';
					nextState := block19_cycle9;
				when block19_cycle9 =>
					-- load cycle 0
					ramAddress <= variable11;
					nextState := block19_cycle10;
				when block19_cycle10 =>
					-- load cycle 1
					-- load cycle 0
					ramAddress <= variable12;
					nextState := block19_cycle11;
				when block19_cycle11 =>
					-- load cycle 2
					variable56 := signed(ramDataOut(7 downto 0));
					variable57 := resize(variable56, 64);
					--   %86 = getelementptr inbounds [3 x [4 x i32]], [3 x [4 x i32]]* %3, i64 0, i64 %85
					variable2 := unsigned(resize(unsigned(variable57 * 3), 8));
					-- load cycle 1
					nextState := block19_cycle12;
				when block19_cycle12 =>
					-- load cycle 2
					variable58 := signed(ramDataOut(7 downto 0));
					variable59 := resize(variable58, 64);
					--   %89 = getelementptr inbounds [4 x i32], [4 x i32]* %86, i64 0, i64 %88
					variable2 := variable2 + unsigned(resize(unsigned(variable59 * 1), 8));
					variable2 := unsigned(resize(unsigned(variable2 * 4), 8));
					-- load cycle 0
					ramAddress0 <= variable2;
					nextState := block19_cycle13;
				when block19_cycle13 =>
					-- load cycle 1
					-- load cycle 0
					ramAddress <= variable7;
					nextState := block19_cycle14;
				when block19_cycle14 =>
					-- load cycle 2
					variable60 := signed(ramDataOut0);
					-- load cycle 1
					nextState := block19_cycle15;
				when block19_cycle15 =>
					-- load cycle 2
					variable61 := signed(ramDataOut(7 downto 0));
					variable62 := signed(resize(unsigned(variable61), 32));
					variable63 := variable62 + variable60;
					variable64 := resize(variable63, 8);
					-- store cycle 0
					ramAddress <= variable7;
					ramDataIn(7 downto 0) <= std_logic_vector(variable64);
					ramWriteEnable <= '1';
					nextState := block20_cycle1;
				-- block20
				when block20_cycle1 =>
					-- load cycle 0
					ramAddress <= variable12;
					nextState := block20_cycle2;
				when block20_cycle2 =>
					-- load cycle 1
					nextState := block20_cycle3;
				when block20_cycle3 =>
					-- load cycle 2
					variable65 := signed(ramDataOut(7 downto 0));
					variable66 := variable65 + to_signed(1, 8);
					-- store cycle 0
					ramAddress <= variable12;
					ramDataIn(7 downto 0) <= std_logic_vector(variable66);
					ramWriteEnable <= '1';
					nextState := block17_cycle1;
				-- block18
				when block18_cycle1 =>
					nextState := block21_cycle1;
				-- block21
				when block21_cycle1 =>
					-- load cycle 0
					ramAddress <= variable11;
					nextState := block21_cycle2;
				when block21_cycle2 =>
					-- load cycle 1
					nextState := block21_cycle3;
				when block21_cycle3 =>
					-- load cycle 2
					variable67 := signed(ramDataOut(7 downto 0));
					variable68 := variable67 + to_signed(1, 8);
					-- store cycle 0
					ramAddress <= variable11;
					ramDataIn(7 downto 0) <= std_logic_vector(variable68);
					ramWriteEnable <= '1';
					nextState := block14_cycle1;
				-- block15
				when block15_cycle1 =>
					-- store cycle 0
					ramAddress <= variable13;
					ramDataIn(7 downto 0) <= std_logic_vector(to_signed(0, 8));
					ramWriteEnable <= '1';
					nextState := block22_cycle1;
				-- block22
				when block22_cycle1 =>
					-- load cycle 0
					ramAddress <= variable13;
					nextState := block22_cycle2;
				when block22_cycle2 =>
					-- load cycle 1
					nextState := block22_cycle3;
				when block22_cycle3 =>
					-- load cycle 2
					variable69 := signed(ramDataOut(7 downto 0));
					variable70 := resize(variable69, 32);
					if (variable70 < to_signed(2, 32)) then
						variable71 := "1";
					else
						variable71 := "0";
					end if;
					if(variable71(0) = '1')then
						nextState := block24_cycle1;
					else
						nextState := block23_cycle1;
					end if;
				-- block24
				when block24_cycle1 =>
					--   %108 = getelementptr inbounds [3 x [4 x i32]], [3 x [4 x i32]]* %3, i64 0, i64 0
					variable2 := resize(unsigned(to_unsigned(0, 64)), 8);
					-- load cycle 0
					ramAddress <= variable13;
					nextState := block24_cycle2;
				when block24_cycle2 =>
					-- load cycle 1
					nextState := block24_cycle3;
				when block24_cycle3 =>
					-- load cycle 2
					variable72 := signed(ramDataOut(7 downto 0));
					variable73 := resize(variable72, 32);
					variable74 := variable73 + to_signed(2, 32);
					variable75 := resize(variable74, 64);
					--   %113 = getelementptr inbounds [4 x i32], [4 x i32]* %108, i64 0, i64 %112
					variable2 := variable2 + unsigned(resize(unsigned(variable75 * 1), 8));
					variable2 := unsigned(resize(unsigned(variable2 * 4), 8));
					-- load cycle 0
					ramAddress0 <= variable2;
					nextState := block24_cycle4;
				when block24_cycle4 =>
					-- load cycle 1
					-- load cycle 0
					ramAddress <= variable7;
					nextState := block24_cycle5;
				when block24_cycle5 =>
					-- load cycle 2
					variable76 := signed(ramDataOut0);
					-- load cycle 1
					nextState := block24_cycle6;
				when block24_cycle6 =>
					-- load cycle 2
					variable77 := signed(ramDataOut(7 downto 0));
					variable78 := signed(resize(unsigned(variable77), 32));
					variable79 := variable78 + variable76;
					variable80 := resize(variable79, 8);
					-- store cycle 0
					ramAddress <= variable7;
					ramDataIn(7 downto 0) <= std_logic_vector(variable80);
					ramWriteEnable <= '1';
					nextState := block25_cycle1;
				-- block25
				when block25_cycle1 =>
					-- load cycle 0
					ramAddress <= variable13;
					nextState := block25_cycle2;
				when block25_cycle2 =>
					-- load cycle 1
					nextState := block25_cycle3;
				when block25_cycle3 =>
					-- load cycle 2
					variable81 := signed(ramDataOut(7 downto 0));
					variable82 := variable81 + to_signed(1, 8);
					-- store cycle 0
					ramAddress <= variable13;
					ramDataIn(7 downto 0) <= std_logic_vector(variable82);
					ramWriteEnable <= '1';
					nextState := block22_cycle1;
				-- block23
				when block23_cycle1 =>
					-- load cycle 0
					ramAddress <= variable7;
					nextState := block23_cycle2;
				when block23_cycle2 =>
					-- load cycle 1
					nextState := block23_cycle3;
				when block23_cycle3 =>
					-- load cycle 2
					variable83 := signed(ramDataOut(7 downto 0));
					variable84 := signed(resize(unsigned(variable83), 32));
					ready <= '1';
					if (retValWritten = '0') then
						output <= variable84;
						retValWritten := '1';
					end if;
			end case;

			lastState := currentState;
			currentState := nextState;
		end if;
	end process;
end behavioral;

