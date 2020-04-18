library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library STD;
use IEEE.NUMERIC_STD.ALL;

entity DSPn is
	port(
		CLK			: in std_logic;
		CE				: in std_logic;
		RST_N			: in std_logic;
		ENABLE		: in std_logic;
		A0   			: in std_logic;
		DI				: in std_logic_vector(7 downto 0);
		DO				: out std_logic_vector(7 downto 0);
		CS_N			: in std_logic;
		RD_N			: in std_logic;
		WR_N			: in std_logic;
		
		DP_ADDR		: in std_logic_vector(11 downto 0);
		DP_SEL      : in std_logic;

		VER			: in std_logic_vector(2 downto 0);--00-DSP1B, 01-DSP2, 10-DSP3, 11-DSP4
		
		BRK_OUT		: out std_logic;
		DBG_REG		: in std_logic_vector(7 downto 0);
		DBG_DAT_IN	: in std_logic_vector(7 downto 0);
		DBG_DAT_OUT	: out std_logic_vector(7 downto 0);
		DBG_DAT_WR	: in std_logic
	);
end DSPn;

architecture rtl of DSPn is

	constant ACC_A		: integer range 0 to 1 := 0;
	constant ACC_B		: integer range 0 to 1 := 1;
	constant FLAG_OV0	: integer range 0 to 5 := 0;
	constant FLAG_OV1	: integer range 0 to 5 := 1;
	constant FLAG_Z	: integer range 0 to 5 := 2;
	constant FLAG_C	: integer range 0 to 5 := 3;
	constant FLAG_S0	: integer range 0 to 5 := 4;
	constant FLAG_S1	: integer range 0 to 5 := 5;
	
	constant INSTR_OP: std_logic_vector(1 downto 0) := "00";
	constant INSTR_RT: std_logic_vector(1 downto 0) := "01";
	constant INSTR_JP: std_logic_vector(1 downto 0) := "10";
	constant INSTR_LD: std_logic_vector(1 downto 0) := "11";

	-- IO Registers
	signal DR	: std_logic_vector(15 downto 0);
	signal SR	: std_logic_vector(15 downto 0);
	signal DP	: std_logic_vector(10 downto 0);
	signal RP	: std_logic_vector(10 downto 0);
	signal PC	: std_logic_vector(10 downto 0);
	type StackRam_t is array (0 to 7) of std_logic_vector(10 downto 0);
	signal STACK_RAM	: StackRam_t;
	signal SP	: unsigned(2 downto 0);
	signal K, L, M, N	: std_logic_vector(15 downto 0);
	signal P, Q	: std_logic_vector(15 downto 0);
	type Acc_t is array (0 to 1) of std_logic_vector(15 downto 0);
	signal ACC	: Acc_t;
	type Flags_t is array (0 to 1) of std_logic_vector(5 downto 0);
	signal FLAGS : Flags_t;
	signal TR, TRB	: std_logic_vector(15 downto 0);
	signal SI, SO	: std_logic_vector(15 downto 0);
	signal SGN	: std_logic_vector(15 downto 0);
	signal RQM	: std_logic;
	signal DRS, DRC	: std_logic;
	signal USF0, USF1	: std_logic;
	signal P0, P1	: std_logic;
	signal EI, DMA	: std_logic;
		
	signal OP_DST		: std_logic_vector(3 downto 0);
	signal OP_SRC		: std_logic_vector(3 downto 0);
	signal OP_RP		: std_logic;
	signal OP_DPH		: std_logic_vector(3 downto 0);
	signal OP_DPL		: std_logic_vector(1 downto 0);
	signal OP_A			: unsigned(0 downto 0);
	signal OP_ALU		: std_logic_vector(3 downto 0);
	signal OP_P			: std_logic_vector(1 downto 0);
	signal OP_ID		: std_logic_vector(15 downto 0);
	signal OP_NA		: std_logic_vector(10 downto 0);
	signal OP_BRCH		: std_logic_vector(8 downto 0);
	signal OP_INSTR	: std_logic_vector(1 downto 0);
	
	signal IDB		: std_logic_vector(15 downto 0);
	signal ALU_R	: std_logic_vector(15 downto 0);
	
	signal PROG_ROM_ADDR : std_logic_vector(12 downto 0);
	signal PROG_ROM_Q	: std_logic_vector(23 downto 0);
	signal DATA_ROM_ADDR : std_logic_vector(12 downto 0);
	signal DATA_ROM_Q	: std_logic_vector(15 downto 0);
	signal DATA_RAM_ADDR_A, DATA_RAM_ADDR_B	: std_logic_vector(10 downto 0);
	signal DATA_RAM_Q_A, DATA_RAM_Q_B : std_logic_vector(15 downto 0);
	signal DATA_RAM_WE : std_logic;
	
	signal EN : std_logic;
	signal RD_Nr, WR_Nr : std_logic_vector(2 downto 0);
	signal PORT_ACTIVE : std_logic;
	
	--debug
	signal DBG_RUN_LAST : std_logic;
	signal DBG_DAT_WRr : std_logic;
	signal DBG_BRK_ADDR : std_logic_vector(10 downto 0) := (others => '1');
	signal DBG_CTRL : std_logic_vector(7 downto 0) := (others => '0');

	component dp16k_wrapper_8bit
	generic (
		addr_width : natural := 11
	);
	port (
		clock           : in  STD_LOGIC;

		address_a       : in  STD_LOGIC_VECTOR (addr_width-1 DOWNTO 0);
		data_a          : in  STD_LOGIC_VECTOR (7 DOWNTO 0) := (others => '0');
		enable_a                : in  STD_LOGIC := '1';
		wren_a          : in  STD_LOGIC := '0';
		q_a                     : out STD_LOGIC_VECTOR (7 DOWNTO 0);
		cs_a            : in  std_logic := '1';

		address_b       : in  STD_LOGIC_VECTOR (addr_width-1 DOWNTO 0) := (others => '0');
		data_b          : in  STD_LOGIC_VECTOR (7 DOWNTO 0) := (others => '0');
		enable_b                : in  STD_LOGIC := '1';
		wren_b          : in  STD_LOGIC := '0';
		q_b                     : out STD_LOGIC_VECTOR (7 DOWNTO 0);
		cs_b        : in  std_logic := '1'
	);
	end component;
	component sprom_verilog is
		generic (
			addr_width    : integer := 8;
			data_width    : integer := 8;
			length        : integer := 8;
			hex_file      : string := ""
		);
		port
		(
			clock           : in  STD_LOGIC;
			address       : in  STD_LOGIC_VECTOR (addr_width-1 DOWNTO 0);
			q             : out STD_LOGIC_VECTOR (data_width-1 DOWNTO 0)
		);
	end component;

begin

	EN <= ENABLE and CE;
		
	OP_INSTR <= PROG_ROM_Q(23 downto 22);
	OP_P <= PROG_ROM_Q(21 downto 20);
	OP_ALU <= PROG_ROM_Q(19 downto 16);
	OP_A <= unsigned(PROG_ROM_Q(15 downto 15));
	OP_DPL <= PROG_ROM_Q(14 downto 13);
	OP_DPH <= PROG_ROM_Q(12 downto 9);
	OP_RP <= PROG_ROM_Q(8);
	OP_SRC <= PROG_ROM_Q(7 downto 4);
	OP_DST <= PROG_ROM_Q(3 downto 0);
	OP_ID <= PROG_ROM_Q(21 downto 6) when OP_INSTR = INSTR_LD else IDB;
	OP_NA <= PROG_ROM_Q(12 downto 2);
	OP_BRCH <= PROG_ROM_Q(21 downto 13);
	
	
	SGN <= x"8000" xor (0 to 15 => FLAGS(ACC_A)(FLAG_S1));
	SR <= RQM & USF1 & USF0 & DRS & DMA & DRC & "00" & EI & "00000" & P1 & P0;
	SI <= (others => '0');
	IDB <= TRB 	         when OP_SRC = x"0" else
			 ACC(ACC_A)    when OP_SRC = x"1" else
			 ACC(ACC_B)    when OP_SRC = x"2" else
			 TR            when OP_SRC = x"3" else
			 "00000" & DP  when OP_SRC = x"4" and VER(2) = '1' else
			 "00000" & RP  when OP_SRC = x"5" and VER(2) = '1' else
			 x"00" & DP(7 downto 0) when OP_SRC = x"4" else
			 "000000" & RP(9 downto 0)  when OP_SRC = x"5" else
			 DATA_ROM_Q    when OP_SRC = x"6" else
			 SGN           when OP_SRC = x"7" else
			 DR            when OP_SRC = x"8" else
			 DR            when OP_SRC = x"9" else
			 SR            when OP_SRC = x"A" else
			 SI            when OP_SRC = x"B" else
			 SI            when OP_SRC = x"C" else
			 K             when OP_SRC = x"D" else
			 L             when OP_SRC = x"E" else
			 DATA_RAM_Q_A  when OP_SRC = x"F" else
			 x"0000";
	
	--ALU
	Q <= ACC(to_integer(OP_A));
	P <= x"0001"      when OP_ALU(3 downto 1) = "100" else
		  DATA_RAM_Q_A when OP_P = "00" else
		  IDB          when OP_P = "01" else
		  M            when OP_P = "10" else
		  N;
		  
	process( OP_ALU, P, Q, ALU_R, FLAGS, OP_A)
		variable FC : std_logic;
		variable CARRY : unsigned(15 downto 0);
	begin
		FC := FLAGS(to_integer(not OP_A))(FLAG_C);
		CARRY := (0 => FC, others => '0');
		case OP_ALU is
			when x"1" =>
				ALU_R <= Q or P;
			when x"2" =>
				ALU_R <= Q and P;
			when x"3" =>
				ALU_R <= Q xor P;
			when x"4" =>
				ALU_R <= std_logic_vector(unsigned(Q) - unsigned(P));
			when x"5" =>
				ALU_R <= std_logic_vector(unsigned(Q) + unsigned(P));
			when x"6" =>
				ALU_R <= std_logic_vector(unsigned(Q) - unsigned(P) - CARRY);
			when x"7" =>
				ALU_R <= std_logic_vector(unsigned(Q) + unsigned(P) + CARRY);
			when x"8" =>
				ALU_R <= std_logic_vector(unsigned(Q) - unsigned(P));
			when x"9" =>
				ALU_R <= std_logic_vector(unsigned(Q) + unsigned(P));
			when x"A" =>
				ALU_R <= not Q;
			when x"B" =>
				ALU_R <= Q(15) & Q(15 downto 1);
			when x"C" =>
				ALU_R <= Q(14 downto 0) & FC;
			when x"D" =>
				ALU_R <= Q(13 downto 0) & "11";
			when x"E" =>
				ALU_R <= Q(11 downto 0) & "1111";
			when x"F" =>
				ALU_R <= Q(7 downto 0) & Q(15 downto 8);
			when others =>
				ALU_R <= Q;
		end case;
	end process;

	--Flags
	process(CLK, RST_N)
		variable OV0 : std_logic;
	begin
		if RST_N = '0' then
			FLAGS <= (others => (others => '0'));
		elsif rising_edge(CLK) then
			if EN = '1' then
				if (OP_INSTR = INSTR_OP or OP_INSTR = INSTR_RT) and OP_ALU /= x"0" then
					FLAGS(to_integer(OP_A))(FLAG_S0) <= ALU_R(15);
					
					if ALU_R = x"0000" then
						FLAGS(to_integer(OP_A))(FLAG_Z) <= '1';
					else
						FLAGS(to_integer(OP_A))(FLAG_Z) <= '0';
					end if;
					
					case OP_ALU is
						when x"1" | x"2" | x"3" | x"A" | x"D" | x"E" | x"F" =>
							FLAGS(to_integer(OP_A))(FLAG_C) <= '0';
						when x"4" | x"6" | x"8" =>
							if ALU_R > Q then
								FLAGS(to_integer(OP_A))(FLAG_C) <= '1';
							else
								FLAGS(to_integer(OP_A))(FLAG_C) <= '0';
							end if;
						when x"5" | x"7" | x"9" =>
							if ALU_R < Q then
								FLAGS(to_integer(OP_A))(FLAG_C) <= '1';
							else
								FLAGS(to_integer(OP_A))(FLAG_C) <= '0';
							end if;
						when x"B" =>
							FLAGS(to_integer(OP_A))(FLAG_C) <= Q(0);
						when x"C" =>
							FLAGS(to_integer(OP_A))(FLAG_C) <= Q(15);
						when others => null;
					end case;
					
					OV0 := (Q(15) xor ALU_R(15)) and ((Q(15) xor P(15)) xor OP_ALU(0));
					case OP_ALU is
						when x"1" | x"2" | x"3" | x"A" | x"B" | x"C" | x"D" | x"E" | x"F" =>
							FLAGS(to_integer(OP_A))(FLAG_OV0) <= '0';
							FLAGS(to_integer(OP_A))(FLAG_OV1) <= '0';
						when x"4" | x"5" | x"6" | x"7" | x"8" | x"9" =>
							FLAGS(to_integer(OP_A))(FLAG_OV0) <= OV0;
							if OV0 = '1' then
								FLAGS(to_integer(OP_A))(FLAG_S1) <= FLAGS(to_integer(OP_A))(FLAG_OV1) xor (not ALU_R(15));
								FLAGS(to_integer(OP_A))(FLAG_OV1) <= not FLAGS(to_integer(OP_A))(FLAG_OV1);
							end if;
						when others => null;
					end case;
				end if;
			end if;
		end if;
	end process; 
	
	--Multiplier
	process(K, L)
		variable TEMP : signed(30 downto 0);
	begin
		TEMP := resize((signed(K) * signed(L)),TEMP'length);
		M <= std_logic_vector(TEMP(30 downto 15));
		N <= std_logic_vector(TEMP(14 downto 0)) & "0";
	end process; 
	
	--Registers
	process(CLK, RST_N)
		variable DAT : std_logic_vector(15 downto 0);
	begin
		if RST_N = '0' then
			ACC <= (others => (others => '0'));
			TR <= (others => '0');
			DP <= (others => '0');
			RP <= (others => '1');
			DRC <= '0';
			USF1 <= '0';
			USF0 <= '0';
			DMA <= '0';
			EI <= '0';
			P1 <= '0';
			P0 <= '0';
			TRB <= (others => '0');
			SO <= (others => '0');
			K <= (others => '0');
			L <= (others => '0');
		elsif rising_edge(CLK) then
			if EN = '1' then
				if (OP_INSTR = INSTR_OP or OP_INSTR = INSTR_RT) then 
					if OP_ALU /= x"0" then
						ACC(to_integer(OP_A)) <= ALU_R;
					end if;
					
					case OP_DPL is
						when "01" =>
							DP(3 downto 0) <= std_logic_vector(unsigned(DP(3 downto 0)) + 1);
						when "10" =>
							DP(3 downto 0) <= std_logic_vector(unsigned(DP(3 downto 0)) - 1);
						when "11" =>
							DP(3 downto 0) <= (others => '0');
						when others => null;
					end case;
					DP(7 downto 4) <= DP(7 downto 4) xor OP_DPH;
					
					if OP_RP = '1' then
						RP <= std_logic_vector(unsigned(RP) - 1);
					end if;
				end if;
				
				if OP_INSTR /= INSTR_JP then
					case OP_DST is
						when x"1" =>
							ACC(ACC_A) <= OP_ID;
						when x"2" =>
							ACC(ACC_B) <= OP_ID;
						when x"3" =>
							TR <= OP_ID;
						when x"4" =>
							DP <= OP_ID(10 downto 0);
						when x"5" =>
							RP <= OP_ID(10 downto 0);
						when x"7" =>
							USF1 <= OP_ID(14);
							USF0 <= OP_ID(13);
							DMA <= OP_ID(11);
							DRC <= OP_ID(10);
							EI <= OP_ID(7);
							P1 <= OP_ID(1);
							P0 <= OP_ID(0);
						when x"8" =>
							SO <= OP_ID;
						when x"9" =>
							SO <= OP_ID;
						when x"A" =>
							K <= OP_ID;
						when x"B" =>
							K <= OP_ID;
							L <= DATA_ROM_Q;
						when x"C" =>
							K <= DATA_RAM_Q_B;
							L <= OP_ID;
						when x"D" =>
							L <= OP_ID;
						when x"E" =>
							TRB <= OP_ID;
						when others => null;
					end case;
				end if;
			end if;
		end if;
	end process; 
	
	process(CLK, RST_N)
		variable NEXT_SP : unsigned(2 downto 0);
		variable NEXT_PC : std_logic_vector(10 downto 0);
		variable COND : std_logic;
	begin
		if RST_N = '0' then
			STACK_RAM <= (others => (others => '0'));
			SP <= (others => '0');
			PC <= (others => '0');
		elsif rising_edge(CLK) then
			if EN = '1' then
				NEXT_PC := std_logic_vector(unsigned(PC) + 1);
				if OP_INSTR = INSTR_RT then
					NEXT_SP := SP - 1;
					PC <= STACK_RAM(to_integer((NEXT_SP(2) and VER(2))&NEXT_SP(1 downto 0)));
					SP <= NEXT_SP;
				elsif OP_INSTR = INSTR_JP then
					case OP_BRCH(5 downto 2) is
						when "0000" =>
							COND := FLAGS(ACC_A)(FLAG_C) xor (not OP_BRCH(1));
						when "0001" =>
							COND := FLAGS(ACC_B)(FLAG_C) xor (not OP_BRCH(1));
						when "0010" =>
							COND := FLAGS(ACC_A)(FLAG_Z) xor (not OP_BRCH(1));
						when "0011" =>
							COND := FLAGS(ACC_B)(FLAG_Z) xor (not OP_BRCH(1));
						when "0100" =>
							COND := FLAGS(ACC_A)(FLAG_OV0) xor (not OP_BRCH(1));
						when "0101" =>
							COND := FLAGS(ACC_B)(FLAG_OV0) xor (not OP_BRCH(1));
						when "0110" =>
							COND := FLAGS(ACC_A)(FLAG_OV1) xor (not OP_BRCH(1));
						when "0111" =>
							COND := FLAGS(ACC_B)(FLAG_OV1) xor (not OP_BRCH(1));
						when "1000" =>
							COND := FLAGS(ACC_A)(FLAG_S0) xor (not OP_BRCH(1));
						when "1001" =>
							COND := FLAGS(ACC_B)(FLAG_S0) xor (not OP_BRCH(1));
						when "1010" =>
							COND := FLAGS(ACC_A)(FLAG_S1) xor (not OP_BRCH(1));
						when "1011" =>
							COND := FLAGS(ACC_B)(FLAG_S1) xor (not OP_BRCH(1));
						when "1100" =>
							if (DP(3 downto 0) = (0 to 3 => OP_BRCH(1)) and OP_BRCH(0) = '0') or
								(DP(3 downto 0) /= (0 to 3 => OP_BRCH(1)) and OP_BRCH(0) = '1') then
								COND := '1';
							else
								COND := '0';
							end if;
						when "1111" =>
							COND := RQM xor (not OP_BRCH(1));
						when others => 
							COND := '0';
					end case;
					
					if OP_BRCH = "000000000" then
						PC <= SO(10 downto 0);
					elsif OP_BRCH(8 downto 6) = "010" and COND = '1' then
						PC <= OP_NA;
					elsif OP_BRCH(8 downto 7) = "10" and OP_BRCH(5 downto 0) = "000000" then
						PC <= OP_NA;
						if OP_BRCH(6) = '1' then
							STACK_RAM(to_integer((SP(2) and VER(2))&SP(1 downto 0))) <= NEXT_PC;
							SP <= SP + 1;
						end if;
					else
						PC <= NEXT_PC;
					end if;
				else
					PC <= NEXT_PC;
				end if;
			end if;
		end if;
	end process; 

	PROG_ROM_ADDR <= std_logic_vector(unsigned(PC) + ("0"&x"000")) when VER="000" else
                    std_logic_vector(unsigned(PC) + ("0"&x"500")) when VER="001" else
                    std_logic_vector(unsigned(PC) + ("0"&x"C00")) when VER="010" else
                    std_logic_vector(unsigned(PC) + ("1"&x"254")) when VER="011" else
                    std_logic_vector(unsigned(PC) + ("1"&x"954"));

	PROG_ROM : sprom_verilog generic map(13, 24, 7018, "../src/chip/DSP/dsp1b23410_p.hex")
	port map(
		clock		=> CLK,
		address	=> PROG_ROM_ADDR,
		q			=> PROG_ROM_Q
	);

	DATA_ROM_ADDR <= VER(2 downto 1) & (VER(0) or (RP(10) and VER(2))) & RP(9 downto 0);
	DATA_ROM : sprom_verilog generic map(13, 16, 6144, "../src/chip/DSP/dsp1b23410_d.hex")
	port map(
		clock		=> CLK,
		address	=> DATA_ROM_ADDR,
		q			=> DATA_ROM_Q
	);
	
	DATA_RAM_ADDR_A <= "000" & DP(7 downto 0) when VER(2)='0' else DP;
	DATA_RAM_ADDR_B <= DP_ADDR(11 downto 1) when DP_SEL = '1' and (WR_N = '0' or RD_N = '0') else DATA_RAM_ADDR_A or x"40";
	DATA_RAM_WE <= '1' when OP_INSTR /= INSTR_JP and OP_DST = x"F" and EN = '1' else '0';

	DATA_RAML : dp16k_wrapper_8bit generic map(11)
	port map(
		clock			=> CLK,
		address_a	=> DATA_RAM_ADDR_A,
		data_a		=> OP_ID(7 downto 0),
		wren_a		=> DATA_RAM_WE,
		q_a			=> DATA_RAM_Q_A(7 downto 0),
		address_b	=> DATA_RAM_ADDR_B,
		data_b		=> DI,
		wren_b		=> not WR_N and DP_SEL and not DP_ADDR(0),
		q_b			=> DATA_RAM_Q_B(7 downto 0)
	);

	DATA_RAMH : dp16k_wrapper_8bit generic map(11)
	port map(
		clock			=> CLK,
		address_a	=> DATA_RAM_ADDR_A,
		data_a		=> OP_ID(15 downto 8),
		wren_a		=> DATA_RAM_WE,
		q_a			=> DATA_RAM_Q_A(15 downto 8),
		address_b	=> DATA_RAM_ADDR_B,
		data_b		=> DI,
		wren_b		=> not WR_N and DP_SEL and DP_ADDR(0),
		q_b			=> DATA_RAM_Q_B(15 downto 8)
	);
	
	--I/O Ports
	process(CLK, RST_N)
	begin
		if RST_N = '0' then
			DRS <= '0';
			RQM <= '0';
			DR <= (others => '0');
			WR_Nr <= (others => '1');
			RD_Nr <= (others => '1');
			PORT_ACTIVE <= '0';
		elsif rising_edge(CLK) then
			if ENABLE = '1' then
				WR_Nr <= WR_Nr(1 downto 0) & WR_N;
				RD_Nr <= RD_Nr(1 downto 0) & RD_N;
				
				if WR_Nr = "110" and CS_N = '0' and A0 = '0' then
					if DRC = '0' then
						if DRS = '0' then
							DR(7 downto 0) <= DI;
						else
							DR(15 downto 8) <= DI;
						end if;
					else
						DR(7 downto 0) <= DI;
					end if;
					PORT_ACTIVE <= '1';
				elsif RD_Nr = "110" and CS_N = '0' and A0 = '0' then
					PORT_ACTIVE <= '1';
				end if;
				
				if (WR_Nr = "001" or RD_Nr = "001") and PORT_ACTIVE = '1' then
					if DRC = '0' then
						if DRS = '0' then
							DRS <= '1';
						else
							RQM <= '0';
							DRS <= '0';
						end if;
					else
						RQM <= '0';
					end if;
					PORT_ACTIVE <= '0';
				elsif EN = '1' then
					if OP_INSTR /= INSTR_JP and OP_DST = x"6" then
						DR <= OP_ID;
						RQM <= '1';
					elsif (OP_INSTR = INSTR_OP or OP_INSTR = INSTR_RT) and OP_SRC = x"8" then
						RQM <= '1';
					end if;
				end if;
			end if;
		end if;
	end process; 

	process( A0, SR, DR, DRC, DRS, DP_SEL, DP_ADDR, DATA_RAM_Q_B )
	begin
		if DP_SEL = '1' then
			if DP_ADDR(0) = '0' then
				DO <= DATA_RAM_Q_B(7 downto 0);
			else
				DO <= DATA_RAM_Q_B(15 downto 8);
			end if;
		elsif A0 = '1' then
			DO <= SR(15 downto 8);
		else
			if DRC = '0' then
				if DRS = '0' then
					DO <= DR(7 downto 0);
				else
					DO <= DR(15 downto 8);
				end if;
			else
				DO <= DR(7 downto 0);
			end if;
		end if;
	end process;


	--Debug
	process(CLK, RST_N)
	begin
		if RST_N = '0' then
			BRK_OUT <= '0';
			DBG_RUN_LAST <= '0';
		elsif rising_edge(CLK) then
			if EN = '1' then
				BRK_OUT <= '0';
				if DBG_CTRL(0) = '1' then	--step
					BRK_OUT <= '1';
				elsif DBG_CTRL(2) = '1' and DBG_BRK_ADDR = PC then	--opcode address break
					BRK_OUT <= '1';
				end if;
			end if;
			
			DBG_RUN_LAST <= DBG_CTRL(7);
			if DBG_CTRL(7) = '1' and DBG_RUN_LAST = '0' then
				BRK_OUT <= '0';
			end if;
		end if;
	end process; 
	
	process( RST_N, CLK, DBG_REG, ACC, FLAGS, PC, RP, DP, TR, TRB, K, L, M, N, DR, SR, SP, IDB )
	begin
			case DBG_REG is
				when x"00" => DBG_DAT_OUT <= ACC(ACC_A)(7 downto 0);
				when x"01" => DBG_DAT_OUT <= ACC(ACC_A)(15 downto 8);
				when x"02" => DBG_DAT_OUT <= ACC(ACC_B)(7 downto 0);
				when x"03" => DBG_DAT_OUT <= ACC(ACC_B)(15 downto 8);
				when x"04" => DBG_DAT_OUT <= "00"&FLAGS(ACC_A);
				when x"05" => DBG_DAT_OUT <= "00"&FLAGS(ACC_B);
				when x"06" => DBG_DAT_OUT <= PC(7 downto 0);
				when x"07" => DBG_DAT_OUT <= "00000"&PC(10 downto 8);
				when x"08" => DBG_DAT_OUT <= RP(7 downto 0);
				when x"09" => DBG_DAT_OUT <= "000000"&RP(9 downto 8);
				when x"0A" => DBG_DAT_OUT <= DP(7 downto 0);
				when x"0B" => DBG_DAT_OUT <= TR(7 downto 0);
				when x"0C" => DBG_DAT_OUT <= TR(15 downto 8);
				when x"0D" => DBG_DAT_OUT <= TRB(7 downto 0);
				when x"0E" => DBG_DAT_OUT <= TRB(15 downto 8);
				when x"0F" => DBG_DAT_OUT <= K(7 downto 0);
				when x"10" => DBG_DAT_OUT <= K(15 downto 8);
				when x"11" => DBG_DAT_OUT <= L(7 downto 0);
				when x"12" => DBG_DAT_OUT <= L(15 downto 8);
				when x"13" => DBG_DAT_OUT <= M(7 downto 0);
				when x"14" => DBG_DAT_OUT <= M(15 downto 8);
				when x"15" => DBG_DAT_OUT <= N(7 downto 0);
				when x"16" => DBG_DAT_OUT <= N(15 downto 8);
				when x"17" => DBG_DAT_OUT <= DR(7 downto 0);
				when x"18" => DBG_DAT_OUT <= DR(15 downto 8);
				when x"19" => DBG_DAT_OUT <= SR(7 downto 0);
				when x"1A" => DBG_DAT_OUT <= SR(15 downto 8);
				when x"1B" => DBG_DAT_OUT <= "00000" & std_logic_vector(SP);
				when x"1C" => DBG_DAT_OUT <= IDB(7 downto 0);
				when x"1D" => DBG_DAT_OUT <= IDB(15 downto 8);
				when others => DBG_DAT_OUT <= x"00";
			end case;
			
		if RST_N = '0' then
			DBG_DAT_WRr <= '0';
		elsif rising_edge(CLK) then
			DBG_DAT_WRr <= DBG_DAT_WR;
			if DBG_DAT_WR = '1' and DBG_DAT_WRr = '0' then
				case DBG_REG is
					when x"80" => DBG_BRK_ADDR(7 downto 0) <= DBG_DAT_IN;
					when x"81" => DBG_BRK_ADDR(10 downto 8) <= DBG_DAT_IN(2 downto 0);
					when x"82" => null;
					when x"83" => DBG_CTRL <= DBG_DAT_IN;
					when others => null;
				end case;
			end if;
		end if;
	end process;

end rtl;
