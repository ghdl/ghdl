library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.AbstractMmPkg.all;

entity testbench is
end entity testbench;

architecture TB of testbench is

	signal AWADDR	: std_logic_vector(6 downto 0);
	signal AWPROT	: std_logic_vector(2 downto 0);
	signal AWVALID	: std_logic;
	signal AWREADY	: std_logic;
	signal WDATA	: std_logic_vector(31 downto 0);
	signal WSTRB	: std_logic_vector(3 downto 0);
	signal WVALID	: std_logic;
	signal WREADY	: std_logic;
	signal BRESP	: std_logic_vector(1 downto 0);
	signal BVALID	: std_logic;
	signal BREADY	: std_logic;
	signal ARADDR	: std_logic_vector(6 downto 0);
	signal ARPROT	: std_logic_vector(2 downto 0);
	signal ARVALID	: std_logic;
	signal ARREADY	: std_logic;
	signal RDATA	: std_logic_vector(31 downto 0);
	signal RRESP	: std_logic_vector(1 downto 0);
	signal RVALID	: std_logic;
	signal RREADY	: std_logic;
	signal ACLK	    : std_logic;
	signal ARESETn	: std_logic;
	
	signal rec : AbstractMmRecType(
		writedata(31 downto 0),
		readdata(31 downto 0),
		address(4 downto 0),
		byteen(3 downto 0)
	);

begin

	BFM: entity work.axi_master
		generic map (
			DATAWIDTH => 32,
			ADDRWIDTH => AWADDR'length
		) port map(
			-- AXI interface,
			AWADDR	=> AWADDR,
			AWPROT	=> AWPROT,
			AWVALID	=> AWVALID,
			AWREADY	=> AWREADY,
			
			WDATA	=> WDATA,
			WSTRB	=> WSTRB,
			WVALID	=> WVALID,
			WREADY	=> WREADY,
			
			BRESP	=> BRESP,
			BVALID	=> BVALID,
			BREADY	=> BREADY,
			ARADDR	=> ARADDR,
			ARPROT	=> ARPROT,
			ARVALID	=> ARVALID,
			ARREADY	=> ARREADY,
			
			RDATA	=> RDATA,
			RRESP	=> RRESP,
			RVALID	=> RVALID,
			RREADY	=> RREADY,
			
			ACLK	=> ACLK,
			ARESETn	=> ARESETn,

			-- AMR interface
			amr => rec
		);
end architecture TB;
