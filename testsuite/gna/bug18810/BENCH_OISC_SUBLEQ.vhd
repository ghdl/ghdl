library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.OISC_SUBLEQ_PKG.all;

entity BENCH_OISC_SUBLEQ is
begin
end entity BENCH_OISC_SUBLEQ;

architecture BENCH of BENCH_OISC_SUBLEQ is

	signal sPCLK : std_logic := '0';
	signal sDCLK : std_logic := '0';
	signal sCLK  : std_logic := '0';
	signal sCLR  : std_logic := '1';

	constant cPCLK_CYCLE : time := 1000.0 ns / 250.0; -- NOTE: 250[MHz]
	constant cDCLK_CYCLE : time := 1000.0 ns / 200.0; -- NOTE: 200[MHz]
	constant cCLK_CYCLE  : time := 1000.0 ns / 150.0; -- NOTE: 150[MHz]
	constant cCLR_TIME   : time := 10*cCLK_CYCLE;

	constant clog2PADDR : integer range 0 to integer'high := 8;
	constant clog2DADDR : integer range 0 to integer'high := 4;
	constant cDW        : integer range 1 to integer'high := 8;

	type tPIF is record
		OISC_SUBLEQ_oPINST : std_logic_vector(clog2DADDR+clog2DADDR+clog2PADDR-1 downto 0);
	end record tPIF;
	signal sP : tPIF;

	type tP is record
		OISC_SUBLEQ_iPWE   : std_logic;
		OISC_SUBLEQ_iPADDR : integer range 0 to 2**clog2PADDR-1;
		OISC_SUBLEQ_iPINST : std_logic_vector(clog2DADDR+clog2DADDR+clog2PADDR-1 downto 0);
		DONE               : std_logic;
	end record tP;
	constant cP : tP := (
		OISC_SUBLEQ_iPWE   => '0',
		OISC_SUBLEQ_iPADDR => 0,
		OISC_SUBLEQ_iPINST => (clog2DADDR+clog2DADDR+clog2PADDR-1 downto 0 => '0'),
		DONE               => '0'
	);
	signal rP : tP := cP;

	type tDIF is record
		OISC_SUBLEQ_oDDATA : std_logic_vector(cDW-1 downto 0);
	end record tDIF;
	signal sD : tDIF;

	type tD is record
		OISC_SUBLEQ_iDWE   : std_logic;
		OISC_SUBLEQ_iDADDR : integer range 0 to 2**clog2DADDR-1;
		OISC_SUBLEQ_iDDATA : std_logic_vector(cDW-1 downto 0);
		DONE               : std_logic;
	end record tD;
	constant cD : tD := (
		OISC_SUBLEQ_iDWE   => '0',
		OISC_SUBLEQ_iDADDR => 0,
		OISC_SUBLEQ_iDDATA => (cDW-1 downto 0 => '0'),
		DONE               => '0'
	);
	signal rD : tD := cD;

	type tIF is record
		OISC_SUBLEQ_oACT : std_logic;
		OISC_SUBLEQ_oPC  : integer range 0 to 2**clog2PADDR-1;
		OISC_SUBLEQ_oLEQ : std_logic;
	end record tIF;
	signal s : tIF;

	type t is record
		ACT  : std_logic;
		DONE : std_logic;
	end record t;
	constant c : t := (
		ACT  => '0',
		DONE => '0'
	);
	signal r : t := c;

begin

	P_sPCLK : process
	begin
		sPCLK <= '0'; wait for cPCLK_CYCLE/2;
		sPCLK <= '1'; wait for cPCLK_CYCLE/2;
	end process P_sPCLK;

	P_sDCLK : process
	begin
		sDCLK <= '0'; wait for cDCLK_CYCLE/2;
		sDCLK <= '1'; wait for cDCLK_CYCLE/2;
	end process P_sDCLK;

	P_sCLK : process
	begin
		sCLK <= '0'; wait for cCLK_CYCLE/2;
		sCLK <= '1'; wait for cCLK_CYCLE/2;
	end process P_sCLK;

	P_sCLR : process
	begin
		sCLR <= '1'; wait for cCLR_TIME;
		sCLR <= '0'; wait;
	end process P_sCLR;

	B_STIM : block is
		pure function fINST (
			iA : integer range 0 to 2**clog2DADDR-1;
			iB : integer range 0 to 2**clog2DADDR-1;
			iC : integer range 0 to 2**clog2PADDR-1
		) return std_logic_vector is
			variable vA : std_logic_vector(clog2DADDR-1 downto 0);
			variable vB : std_logic_vector(clog2DADDR-1 downto 0);
			variable vC : std_logic_vector(clog2PADDR-1 downto 0);
		begin
			vA := std_logic_vector(to_unsigned(iA, clog2DADDR));
			vB := std_logic_vector(to_unsigned(iB, clog2DADDR));
			vC := std_logic_vector(to_unsigned(iC, clog2PADDR));
			return vA & vB & vC;
		end function fINST;

		constant cD_Z     : integer range 0 to 2**clog2DADDR-1 := 0;
		constant cD_X     : integer range 0 to 2**clog2DADDR-1 := 1;
		constant cD_Y     : integer range 0 to 2**clog2DADDR-1 := 2;
		constant cV_Z     : std_logic_vector(cDW-1 downto 0)   := std_logic_vector(to_signed( 0, cDW));
		constant cV_X     : std_logic_vector(cDW-1 downto 0)   := std_logic_vector(to_signed(12, cDW));
		constant cV_Y     : std_logic_vector(cDW-1 downto 0)   := std_logic_vector(to_signed(34, cDW));
		constant cP_ORG   : integer range 0 to 2**clog2PADDR-1 := 0;
		constant cP_START : integer range 0 to 2**clog2PADDR-1 := cP_ORG+8;
		constant cP_STOP  : integer range 0 to 2**clog2PADDR-1 := 2**clog2PADDR-1;
	begin
		P_STIM_P : process
		begin
			-- ORG: JMP START = ORG: subleq Z, Z, START
			rP.OISC_SUBLEQ_iPADDR <= cP_ORG;
			rP.OISC_SUBLEQ_iPINST <= fINST(cD_Z, cD_Z, cP_START);
			rP.OISC_SUBLEQ_iPWE   <= '1';
			wait until (rising_edge(sPCLK));

			-- START: ADD X, Y = START: subleq X, Z, START+1
			--                          subleq Z, Y, START+2
			--                          subleq Z, Z, START+3
			rP.OISC_SUBLEQ_iPADDR <= cP_START;
			rP.OISC_SUBLEQ_iPINST <= fINST(cD_X, cD_Z, cP_START+1);
			rP.OISC_SUBLEQ_iPWE   <= '1';
			wait until (rising_edge(sPCLK));
			rP.OISC_SUBLEQ_iPADDR <= cP_START+1;
			rP.OISC_SUBLEQ_iPINST <= fINST(cD_Z, cD_Y, cP_START+2);
			rP.OISC_SUBLEQ_iPWE   <= '1';
			wait until (rising_edge(sPCLK));
			rP.OISC_SUBLEQ_iPADDR <= cP_START+2;
			rP.OISC_SUBLEQ_iPINST <= fINST(cD_Z, cD_Z, cP_START+3);
			rP.OISC_SUBLEQ_iPWE   <= '1';
			wait until (rising_edge(sPCLK));

			-- START+3: JUMP STOP = START+3: subleq Z, Z, STOP
			rP.OISC_SUBLEQ_iPADDR <= cP_START+3;
			rP.OISC_SUBLEQ_iPINST <= fINST(cD_Z, cD_Z, cP_STOP);
			rP.OISC_SUBLEQ_iPWE   <= '1';
			wait until (rising_edge(sPCLK));

			-- STOP: JUMP STOP = STOP: subleq Z, Z, STOP
			rP.OISC_SUBLEQ_iPADDR <= cP_STOP;
			rP.OISC_SUBLEQ_iPINST <= fINST(cD_Z, cD_Z, cP_STOP);
			rP.OISC_SUBLEQ_iPWE   <= '1';
			wait until (rising_edge(sPCLK));

			rP      <= cP;
			rP.DONE <= '1';
			wait;
		end process P_STIM_P;

		P_STIM_D : process
		begin
			rD.OISC_SUBLEQ_iDADDR <= cD_Z;
			rD.OISC_SUBLEQ_iDDATA <= cV_Z;
			rD.OISC_SUBLEQ_iDWE   <= '1';
			wait until (rising_edge(sDCLK));

			rD.OISC_SUBLEQ_iDADDR <= cD_X;
			rD.OISC_SUBLEQ_iDDATA <= cV_X;
			rD.OISC_SUBLEQ_iDWE   <= '1';
			wait until (rising_edge(sDCLK));

			rD.OISC_SUBLEQ_iDADDR <= cD_Y;
			rD.OISC_SUBLEQ_iDDATA <= cV_Y;
			rD.OISC_SUBLEQ_iDWE   <= '1';
			wait until (rising_edge(sDCLK));

			rD      <= cD;
			rD.DONE <= '1';
			wait;
		end process P_STIM_D;

		P_STIM : process
		begin
			wait until (rP.DONE = '1' and rD.DONE = '1' and sCLR /= '1');
			wait until (rising_edge(sCLK));

			r.ACT <= '1';
			wait until (rising_edge(sCLK));

			wait until (s.OISC_SUBLEQ_oPC = cP_STOP);

			r.ACT  <= '0';
			r.DONE <= '1';
			wait;
		end process P_STIM;
	end block B_STIM;

	U_OISC_SUBLEQ : OISC_SUBLEQ
	generic map (
		log2PADDR => clog2PADDR,
		log2DADDR => clog2DADDR,
		DW        => cDW,
		ZERO      => false,
		ASYNC     => false
	)
	port map (
		iPCLK  => sPCLK,
		iPWE   => rP.OISC_SUBLEQ_iPWE,
		iPADDR => rP.OISC_SUBLEQ_iPADDR,
		iPINST => rP.OISC_SUBLEQ_iPINST,
		oPINST => sP.OISC_SUBLEQ_oPINST,

		iDCLK  => sDCLK,
		iDWE   => rD.OISC_SUBLEQ_iDWE,
		iDADDR => rD.OISC_SUBLEQ_iDADDR,
		iDDATA => rD.OISC_SUBLEQ_iDDATA,
		oDDATA => sD.OISC_SUBLEQ_oDDATA,

		iCLR   => sCLR,
		iCLK   => sCLK,
		iACT   => r.ACT,
		oACT   => s.OISC_SUBLEQ_oACT,
		oPC    => s.OISC_SUBLEQ_oPC,
		oLEQ   => s.OISC_SUBLEQ_oLEQ
	);

end architecture BENCH;
