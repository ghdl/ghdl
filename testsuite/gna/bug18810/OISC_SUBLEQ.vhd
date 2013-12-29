library ieee;
use ieee.std_logic_1164.all;
use work.DMEM_PKG.all;

package OISC_SUBLEQ_PKG is

	component OISC_SUBLEQ is
		generic (
			log2PADDR : integer range 0 to integer'high := 8;
			log2DADDR : integer range 0 to integer'high := 4;
			DW        : integer range 1 to integer'high := 8;
			ZERO      : boolean                         := false;
			LVT_DMEM  : boolean                         := true;
			ASYNC     : boolean                         := false
		);
		port (
			iPCLK  : in  std_logic;
			iPWE   : in  std_logic;
			iPADDR : in  integer range 0 to 2**log2PADDR-1;
			iPINST : in  std_logic_vector(log2DADDR+log2DADDR+log2PADDR-1 downto 0);
			oPINST : out std_logic_vector(log2DADDR+log2DADDR+log2PADDR-1 downto 0);

			iDCLK  : in  std_logic;
			iDWE   : in  std_logic;
			iDADDR : in  integer range 0 to 2**log2DADDR-1;
			iDDATA : in  std_logic_vector(DW-1 downto 0);
			oDDATA : out std_logic_vector(DW-1 downto 0);

			iCLR   : in  std_logic;
			iCLK   : in  std_logic;
			iACT   : in  std_logic;
			oACT   : out std_logic;
			oPC    : out integer range 0 to 2**log2PADDR-1;
			oLEQ   : out std_logic
		);
	end component OISC_SUBLEQ;

	constant cOISC_SUBLEQ_PW_LATENCY : integer := 1;
	constant cOISC_SUBLEQ_PR_LATENCY : integer := 0;
	constant cOISC_SUBLEQ_LATENCY    : integer := 1;

	pure function fOISC_SUBLEQ_DW_LATENCY (
		iLVT_DMEM : boolean
	) return integer;
	pure function fOISC_SUBLEQ_DR_LATENCY (
		iLVT_DMEM : boolean
	) return integer;

end package OISC_SUBLEQ_PKG;

package body OISC_SUBLEQ_PKG is

	pure function fOISC_SUBLEQ_DW_LATENCY (
		iLVT_DMEM : boolean
	) return integer is
	begin
		if (iLVT_DMEM = true) then
			return cDMEM_DW_LATENCY;
		else
			return 1;
		end if;
	end function fOISC_SUBLEQ_DW_LATENCY;

	pure function fOISC_SUBLEQ_DR_LATENCY (
		iLVT_DMEM : boolean
	) return integer is
	begin
		if (iLVT_DMEM = true) then
			return cDMEM_DR_LATENCY;
		else
			return 0;
		end if;
	end function fOISC_SUBLEQ_DR_LATENCY;

end package body OISC_SUBLEQ_PKG;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.DMEM_PKG.all;

entity OISC_SUBLEQ is
	generic (
		log2PADDR : integer range 0 to integer'high := 8;
		log2DADDR : integer range 0 to integer'high := 4;
		DW        : integer range 1 to integer'high := 8;
		ZERO      : boolean                         := true;
		LVT_DMEM  : boolean                         := true;
		ASYNC     : boolean                         := false
	);
	port (
		iPCLK  : in  std_logic;
		iPWE   : in  std_logic;
		iPADDR : in  integer range 0 to 2**log2PADDR-1;
		iPINST : in  std_logic_vector(log2DADDR+log2DADDR+log2PADDR-1 downto 0);
		oPINST : out std_logic_vector(log2DADDR+log2DADDR+log2PADDR-1 downto 0);

		iDCLK  : in  std_logic;
		iDWE   : in  std_logic;
		iDADDR : in  integer range 0 to 2**log2DADDR-1;
		iDDATA : in  std_logic_vector(DW-1 downto 0);
		oDDATA : out std_logic_vector(DW-1 downto 0);

		iCLR   : in  std_logic;
		iCLK   : in  std_logic;
		iACT   : in  std_logic;
		oACT   : out std_logic;
		oPC    : out integer range 0 to 2**log2PADDR-1;
		oLEQ   : out std_logic
	);
begin
	-- RTL_SYNTHESIS OFF
	A_DMEM_AR_LATENCY : assert (not (LVT_DMEM = true and cDMEM_AR_LATENCY /= 0))
		report OISC_SUBLEQ'instance_name &
			"cDMEM_AR_LATENCY =" & integer'image(cDMEM_AR_LATENCY)
		severity warning;
	A_DMEM_BW_LATENCY : assert (not (LVT_DMEM = true and cDMEM_BW_LATENCY /= 1))
		report OISC_SUBLEQ'instance_name &
			"cDMEM_BW_LATENCY =" & integer'image(cDMEM_BW_LATENCY)
		severity warning;
	A_DMEM_BR_LATENCY : assert (not (LVT_DMEM = true and cDMEM_BR_LATENCY /= 0))
		report OISC_SUBLEQ'instance_name &
			"cDMEM_BR_LATENCY =" & integer'image(cDMEM_BR_LATENCY)
		severity warning;
	-- RTL_SYNTHESIS ON
end entity OISC_SUBLEQ;

architecture TP of OISC_SUBLEQ is

	type tIF is record
		PMEM_oA : integer range 0 to 2**log2DADDR-1;
		PMEM_oB : integer range 0 to 2**log2DADDR-1;
		PMEM_oC : integer range 0 to 2**log2PADDR-1;
		DMEM_oA : std_logic_vector(DW-1 downto 0);
		DMEM_oB : std_logic_vector(DW-1 downto 0);
	end record tIF;
	signal s : tIF;

	type t is record
		ACT : std_logic;
		SUB : std_logic_vector(DW-1 downto 0);
		PC  : integer range 0 to 2**log2PADDR-1;
		LEQ : std_logic;
	end record t;
	constant c : t := (
		ACT  => '0',
		SUB  => (DW-1 downto 0 => '0'),
		PC   => 0,
		LEQ  => '0'
	);
	signal g : t;
	signal r : t := c;

begin

	B_BLOB : block is
		type tPMEM is array (0 to 2**log2PADDR-1) of std_logic_vector(log2DADDR+log2DADDR+log2PADDR-1 downto 0);
		signal aPMEM : tPMEM := (0 to 2**log2PADDR-1 => (log2DADDR+log2DADDR+log2PADDR-1 downto 0 => '0'));

		signal gPMEM_oINST : std_logic_vector(log2DADDR+log2DADDR+log2PADDR-1 downto 0);
		signal gPMEM_oA    : std_logic_vector(log2DADDR-1 downto 0);
		signal gPMEM_oB    : std_logic_vector(log2DADDR-1 downto 0);
		signal gPMEM_oC    : std_logic_vector(log2PADDR-1 downto 0);
	begin
		P_PMEM_P : process (iPCLK)
		begin
			if (rising_edge(iPCLK)) then
				if (iPWE = '1') then
					aPMEM(iPADDR) <= iPINST;
				end if;
			end if;
		end process P_PMEM_P;

		oPINST      <= aPMEM(iPADDR);
		gPMEM_oINST <= aPMEM(r.PC);
		gPMEM_oA    <= gPMEM_oINST(log2DADDR+log2DADDR+log2PADDR-1 downto log2DADDR+log2PADDR);
		gPMEM_oB    <= gPMEM_oINST(          log2DADDR+log2PADDR-1 downto           log2PADDR);
		gPMEM_oC    <= gPMEM_oINST(                    log2PADDR-1 downto                   0);
		s.PMEM_oA   <= to_integer(unsigned(gPMEM_oA));
		s.PMEM_oB   <= to_integer(unsigned(gPMEM_oB));
		s.PMEM_oC   <= to_integer(unsigned(gPMEM_oC));

		G_LVT_DMEM : if (LVT_DMEM = true) generate
		begin
			U_DMEM : DMEM
			generic map (
				log2DADDR => log2DADDR,
				DW        => DW,
				ZERO      => ZERO
			)
			port map (
				iDCLK  => iDCLK,
				iDWE   => iDWE,
				iDADDR => iDADDR,
				iDDATA => iDDATA,
				oDDATA => oDDATA,

				iCLK   => iCLK,
				iAADDR => s.PMEM_oA,
				oADATA => s.DMEM_oA,
				iBWE   => iACT,
				iBADDR => s.PMEM_oB,
				iBDATA => g.SUB,
				oBDATA => s.DMEM_oB
			);
		end generate G_LVT_DMEM;

		G_2W3R_DMEM : if (LVT_DMEM = false) generate
			-- FIXME: ISE 13.2 does not support "protected"... :(
			--type tDMEM is protected
			--	procedure pWRITE(
			--		iADDR : in integer range 0 to 2**log2DADDR-1;
			--		iDATA : in std_logic_vector(DW-1 downto 0)
			--	);
			--	impure function fREAD(
			--		iADDR : integer range 0 to 2**log2DADDR-1
			--	) return std_logic_vector;
			--end protected tDMEM;
			--type tDMEM is protected body
			--	type tDMEM_PRIM is array (0 to 2**log2DADDR-1) of std_logic_vector(DW-1 downto 0);
			--	variable aDMEM_PRIM : tDMEM_PRIM := (0 to 2**log2DADDR-1 => (DW-1 downto 0 => '0'));
			--	procedure pWRITE(
			--		iADDR : in integer range 0 to 2**log2DADDR-1;
			--		iDATA : in std_logic_vector(DW-1 downto 0)
			--	) is
			--	begin
			--		aDMEM_PRIM(iADDR) := iDATA;
			--	end procedure pWRITE;
			--	impure function fREAD(
			--		iADDR : integer range 0 to 2**log2DADDR-1
			--	) return std_logic_vector is
			--	begin
			--		return aDMEM_PRIM(iADDR);
			--	end function fREAD;
			--end protected body tDMEM;
			--shared variable aDMEM : tDMEM;

			-- FIXME: VHDL-93 shared variable does not provide mutex... :(
			type tDMEM is array (0 to 2**log2DADDR-1) of std_logic_vector(DW-1 downto 0);
			shared variable aDMEM : tDMEM := (0 to 2**log2DADDR-1 => (DW-1 downto 0 => '0'));
		begin
			P_DMEM_D : process (iDCLK)
			begin
				if (rising_edge(iDCLK)) then
					if (iDWE = '1') then
						--aDMEM.pWRITE(iDADDR, iDDATA);
						aDMEM(iDADDR) := iDDATA;
					end if;
				end if;
			end process P_DMEM_D;

			--oDDATA    <= (DW-1 downto 0 => '0') when (ZERO = true and iDADDR    = 0) else aDMEM.fREAD(iDADDR);
			--s.DMEM_oA <= (DW-1 downto 0 => '0') when (ZERO = true and s.PMEM_oA = 0) else aDMEM.fREAD(s.PMEM_oA);
			--s.DMEM_oB <= (DW-1 downto 0 => '0') when (ZERO = true and s.PMEM_oB = 0) else aDMEM.fREAD(s.PMEM_oB);
			oDDATA    <= (DW-1 downto 0 => '0') when (ZERO = true and iDADDR    = 0) else aDMEM(iDADDR);
			s.DMEM_oA <= (DW-1 downto 0 => '0') when (ZERO = true and s.PMEM_oA = 0) else aDMEM(s.PMEM_oA);
			s.DMEM_oB <= (DW-1 downto 0 => '0') when (ZERO = true and s.PMEM_oB = 0) else aDMEM(s.PMEM_oB);

			-- FIXME: This DMEM write back is kludge... :(
			P_DMEM_WRITE_BACK : process (iCLK)
			begin
				if (rising_edge(iCLK)) then
					if (iACT = '1') then
						--aDMEM.pWRITE(s.PMEM_oB, g.SUB);
						aDMEM(s.PMEM_oB) := g.SUB;
					end if;
				end if;
			end process P_DMEM_WRITE_BACK;
		end generate G_2W3R_DMEM;
	end block B_BLOB;

	P_COMB : process (iACT, r, s)
		variable v : t := c;
		pure function fSUB (
			iA : std_logic_vector(DW-1 downto 0);
			iB : std_logic_vector(DW-1 downto 0)
		) return std_logic_vector is
			variable vSUB : signed(DW-1 downto 0);
		begin
			-- FIXME: Consider th3 borrow?
			vSUB := signed(iB) - signed(iA);
			return std_logic_vector(vSUB);
		end function fSUB;
	begin
		if (iACT = '1') then
			v.ACT := '1';
			v.SUB := fSUB(s.DMEM_oA, s.DMEM_oB);
			if (signed(v.SUB) <= 0) then
				v.PC  := s.PMEM_oC;
				v.LEQ := '1';
			else
				if (r.PC >= 2**log2PADDR-1) then
					v.PC := 0;
				else
					v.PC := r.PC + 1;
				end if;
				v.LEQ := '0';
			end if;
		else
			v.ACT := '0';
			v.SUB := r.SUB;
			v.PC  := r.PC;
			v.LEQ := r.LEQ;
		end if;

		g <= v;

		oACT <= r.ACT;
		oPC  <= r.PC;
		oLEQ <= r.LEQ;
	end process P_COMB;

	G_ASYNC : if (ASYNC = true) generate
	begin
		P_SEQ : process (iCLR, iCLK)
		begin
			if (iCLR = '1') then
				r <= c;
			elsif (rising_edge(iCLK)) then
				r <= g;
			end if;
		end process P_SEQ;
	end generate G_ASYNC;

	G_SYNC : if (ASYNC = false) generate
	begin
		P_SEQ : process (iCLK)
		begin
			if (rising_edge(iCLK)) then
				if (iCLR = '1') then
					r <= c;
				else
					r <= g;
				end if;
			end if;
		end process P_SEQ;
	end generate G_SYNC;

end architecture TP;
