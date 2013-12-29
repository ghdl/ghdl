-- NOTE: http://www.eecg.toronto.edu/~steffan/papers/laforest_fpga10.pdf

library ieee;
use ieee.std_logic_1164.all;

package DMEM_PRIM_PKG is

	component DMEM_PRIM is
		generic (
			log2A : integer range 0 to integer'high := 4;
			DW    : integer range 1 to integer'high := 8;
			ZERO  : boolean                         := false
		);
		port (
			iWCLK  : in  std_logic;
			iWE    : in  std_logic;
			iWA    : in  integer range 0 to 2**log2A-1;
			iWD    : in  std_logic_vector(DW-1 downto 0);

			iRA0   : in  integer range 0 to 2**log2A-1;
			iRA1   : in  integer range 0 to 2**log2A-1;
			iRA2   : in  integer range 0 to 2**log2A-1;
			oRD0   : out std_logic_vector(DW-1 downto 0);
			oRD1   : out std_logic_vector(DW-1 downto 0);
			oRD2   : out std_logic_vector(DW-1 downto 0)
		);
	end component DMEM_PRIM;

	constant cDMEM_PRIM_W_LATENCY  : integer := 1;
	constant cDMEM_PRIM_R0_LATENCY : integer := 0;
	constant cDMEM_PRIM_R1_LATENCY : integer := 0;
	constant cDMEM_PRIM_R2_LATENCY : integer := 0;

end package DMEM_PRIM_PKG;

package body DMEM_PRIM_PKG is

	-- NOTE: This body should keep to be empty to stub.

end package body DMEM_PRIM_PKG;

library ieee;
use ieee.std_logic_1164.all;

entity DMEM_PRIM is
	generic (
		log2A : integer range 0 to integer'high := 4;
		DW    : integer range 1 to integer'high := 8;
		ZERO  : boolean                         := false
	);
	port (
		iWCLK  : in  std_logic;
		iWE    : in  std_logic;
		iWA    : in  integer range 0 to 2**log2A-1;
		iWD    : in  std_logic_vector(DW-1 downto 0);

		iRA0   : in  integer range 0 to 2**log2A-1;
		iRA1   : in  integer range 0 to 2**log2A-1;
		iRA2   : in  integer range 0 to 2**log2A-1;
		oRD0   : out std_logic_vector(DW-1 downto 0);
		oRD1   : out std_logic_vector(DW-1 downto 0);
		oRD2   : out std_logic_vector(DW-1 downto 0)
	);
begin
end entity DMEM_PRIM;

architecture RTL of DMEM_PRIM is

	type tDMEM_PRIM is array (0 to 2**log2A-1) of std_logic_vector(DW-1 downto 0);
	signal aDMEM_PRIM0 : tDMEM_PRIM := (0 to 2**log2A-1 => (DW-1 downto 0 => '0'));
	signal aDMEM_PRIM1 : tDMEM_PRIM := (0 to 2**log2A-1 => (DW-1 downto 0 => '0'));
	signal aDMEM_PRIM2 : tDMEM_PRIM := (0 to 2**log2A-1 => (DW-1 downto 0 => '0'));

begin

	P_DMEM_PRIM : process (iWCLK)
	begin
		if (rising_edge(iWCLK)) then
			if (iWE = '1') then
				aDMEM_PRIM0(iWA) <= iWD;
				aDMEM_PRIM1(iWA) <= iWD;
				aDMEM_PRIM2(iWA) <= iWD;
			end if;
		end if;
	end process P_DMEM_PRIM;

	oRD0 <= (DW-1 downto 0 => '0') when (ZERO = true and iRA0 = 0) else aDMEM_PRIM0(iRA0);
	oRD1 <= (DW-1 downto 0 => '0') when (ZERO = true and iRA1 = 0) else aDMEM_PRIM1(iRA1);
	oRD2 <= (DW-1 downto 0 => '0') when (ZERO = true and iRA2 = 0) else aDMEM_PRIM2(iRA2);

end architecture RTL;

library ieee;
use ieee.std_logic_1164.all;
use work.DMEM_PRIM_PKG.all;

package DMEM_PKG is

	component DMEM is
		generic (
			log2DADDR : integer range 0 to integer'high := 4;
			DW        : integer range 1 to integer'high := 8;
			ZERO      : boolean                         := false
		);
		port (
			iDCLK  : in  std_logic;
			iDWE   : in  std_logic;
			iDADDR : in  integer range 0 to 2**log2DADDR-1;
			iDDATA : in  std_logic_vector(DW-1 downto 0);
			oDDATA : out std_logic_vector(DW-1 downto 0);

			iCLK   : in  std_logic;
			iAADDR : in  integer range 0 to 2**log2DADDR-1;
			oADATA : out std_logic_vector(DW-1 downto 0);
			iBWE   : in  std_logic;
			iBADDR : in  integer range 0 to 2**log2DADDR-1;
			iBDATA : in  std_logic_vector(DW-1 downto 0);
			oBDATA : out std_logic_vector(DW-1 downto 0)
		);
	end component DMEM;

	constant cDMEM_DW_LATENCY : integer := cDMEM_PRIM_W_LATENCY;
	constant cDMEM_DR_LATENCY : integer := cDMEM_PRIM_R0_LATENCY;
	constant cDMEM_AR_LATENCY : integer := cDMEM_PRIM_R1_LATENCY;
	constant cDMEM_BW_LATENCY : integer := cDMEM_PRIM_W_LATENCY;
	constant cDMEM_BR_LATENCY : integer := cDMEM_PRIM_R2_LATENCY;

end package DMEM_PKG;

package body DMEM_PKG is

	-- NOTE: This body should keep to be empty to stub.

end package body DMEM_PKG;

library ieee;
use ieee.std_logic_1164.all;
use work.DMEM_PRIM_PKG.all;

entity DMEM is
	generic (
		log2DADDR : integer range 0 to integer'high := 4;
		DW        : integer range 1 to integer'high := 8;
		ZERO      : boolean                         := false
	);
	port (
		iDCLK  : in  std_logic;
		iDWE   : in  std_logic;
		iDADDR : in  integer range 0 to 2**log2DADDR-1;
		iDDATA : in  std_logic_vector(DW-1 downto 0);
		oDDATA : out std_logic_vector(DW-1 downto 0);

		iCLK   : in  std_logic;
		iAADDR : in  integer range 0 to 2**log2DADDR-1;
		oADATA : out std_logic_vector(DW-1 downto 0);
		iBWE   : in  std_logic;
		iBADDR : in  integer range 0 to 2**log2DADDR-1;
		iBDATA : in  std_logic_vector(DW-1 downto 0);
		oBDATA : out std_logic_vector(DW-1 downto 0)
	);
begin
end entity DMEM;

architecture RTL of DMEM is

	-- FIXME: ISE 13.2 does not support "protected"... :(
	type tBANK is (BANK_D, BANK_B);
	type tLVT is array (0 to 2**log2DADDR-1) of tBANK;
	shared variable aLVT : tLVT := (0 to 2**log2DADDR-1 => BANK_D);

	signal sDMEM_PRIM_D_oDDATA : std_logic_vector(DW-1 downto 0);
	signal sDMEM_PRIM_D_oADATA : std_logic_vector(DW-1 downto 0);
	signal sDMEM_PRIM_D_oBDATA : std_logic_vector(DW-1 downto 0);

	signal sDMEM_PRIM_B_oDDATA : std_logic_vector(DW-1 downto 0);
	signal sDMEM_PRIM_B_oADATA : std_logic_vector(DW-1 downto 0);
	signal sDMEM_PRIM_B_oBDATA : std_logic_vector(DW-1 downto 0);

begin

	P_LVT_D : process (iDCLK)
	begin
		if (rising_edge(iDCLK)) then
			if (iDWE = '1') then
				aLVT(iDADDR) := BANK_D;
			end if;
		end if;
	end process P_LVT_D;

	P_LVT_B : process (iCLK)
	begin
		if (rising_edge(iCLK)) then
			if (iBWE = '1') then
				aLVT(iBADDR) := BANK_B;
			end if;
		end if;
	end process P_LVT_B;

	U_DMEM_PRIM_D : DMEM_PRIM
	generic map (
		log2A => log2DADDR,
		DW    => DW,
		ZERO  => ZERO
	)
	port map (
		iWCLK  => iDCLK,
		iWE    => iDWE,
		iWA    => iDADDR,
		iWD    => iDDATA,

		iRA0   => iDADDR,
		iRA1   => iAADDR,
		iRA2   => iBADDR,
		oRD0   => sDMEM_PRIM_D_oDDATA,
		oRD1   => sDMEM_PRIM_D_oADATA,
		oRD2   => sDMEM_PRIM_D_oBDATA
	);

	U_DMEM_PRIM_B : DMEM_PRIM
	generic map (
		log2A => log2DADDR,
		DW    => DW,
		ZERO  => ZERO
	)
	port map (
		iWCLK  => iCLK,
		iWE    => iBWE,
		iWA    => iBADDR,
		iWD    => iBDATA,

		iRA0   => iDADDR,
		iRA1   => iAADDR,
		iRA2   => iBADDR,
		oRD0   => sDMEM_PRIM_B_oDDATA,
		oRD1   => sDMEM_PRIM_B_oADATA,
		oRD2   => sDMEM_PRIM_B_oBDATA
	);

	oDDATA <= sDMEM_PRIM_D_oDDATA when (aLVT(iDADDR) = BANK_D) else sDMEM_PRIM_B_oDDATA;
	oADATA <= sDMEM_PRIM_D_oADATA when (aLVT(iAADDR) = BANK_D) else sDMEM_PRIM_B_oADATA;
	oBDATA <= sDMEM_PRIM_D_oBDATA when (aLVT(iBADDR) = BANK_D) else sDMEM_PRIM_B_oBDATA;

end architecture RTL;
