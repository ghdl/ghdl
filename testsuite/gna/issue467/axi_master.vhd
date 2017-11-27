-------------------------------------------------------------------------------
-- Title      : AXI-Lite Master BFM
-- Project    : P500
-------------------------------------------------------------------------------
-- File       : axi_master.vhd
-- Author     : Rob Gaddi  <rgaddi@highlandtechnology.com>
-- Company    : Highland Technology, Inc.
-- Created    : 21-Nov-2017
-- Last update: 21-Nov-2017
-- Platform   : Simulation
-- Standard   : VHDL-2008
-------------------------------------------------------------------------------
-- Description: Simulation model of an AXI4-Lite bus master.
-------------------------------------------------------------------------------
-- Revision History:
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library osvvm;
use osvvm.TbUtilPkg.all;
use osvvm.AlertLogPkg.all;

use work.AbstractMmPkg.all;

entity axi_master is
	generic (
		LOG_NAME  : string := "axi_master";
		DATAWIDTH : positive := 32;
		ADDRWIDTH : positive := 12
	);
	port (
		-- AXI interface
		AWADDR	: out std_logic_vector(ADDRWIDTH-1 downto 0);
		AWPROT	: out std_logic_vector(2 downto 0);
		AWVALID	: out std_logic;
		AWREADY	: in  std_logic;
		
		WDATA	: out std_logic_vector(DATAWIDTH-1 downto 0);
		WSTRB	: out std_logic_vector((DATAWIDTH/8)-1 downto 0);
		WVALID	: out std_logic;
		WREADY	: in  std_logic;
		
		BRESP	: in  std_logic_vector(1 downto 0);
		BVALID	: in  std_logic;
		BREADY	: out std_logic;
		
		ARADDR	: out std_logic_vector(ADDRWIDTH-1 downto 0);
		ARPROT	: out std_logic_vector(2 downto 0);
		ARVALID	: out std_logic;
		ARREADY	: in  std_logic;
		
		RDATA	: in  std_logic_vector(DATAWIDTH-1 downto 0);
		RRESP	: in  std_logic_vector(1 downto 0);
		RVALID	: in  std_logic;
		RREADY	: out std_logic;
		
		ACLK	: in  std_logic;
		ARESETn	: in  std_logic;
		
		-- AMR interface
		amr : inout AbstractMmRecType
	);
end entity axi_master;

architecture Behavioral of axi_master is

	constant ALRT : AlertLogIDType := GetAlertLogID(LOG_NAME);

	signal prot: std_logic_vector(2 downto 0);
	constant BADREADDATA : std_logic_vector(RDATA'range) := (others => 'X');

begin

	INITIALIZE: process
	begin
		prot <= (others => '0');
		wait;
	end process INITIALIZE;

	AXI: process 
	
		procedure SingleRead is
			variable request_ack : boolean := false;
		begin
			ARADDR <= STD_LOGIC_VECTOR(GetByteAddress(amr));
			ARPROT <= prot;
			ARVALID <= '1';
			
			while not request_ack loop
				wait until rising_edge(ACLK);
				if RVALID = '1' then
					RREADY <= '0';
					request_ack := true;
				end if;
			end loop;
			
			RREADY <= '1';
			wait until rising_edge(ACLK) and RVALID='1';
			if RRESP = "00" then
				amr.readdata <= RDATA;
			else
				amr.readdata <= BADREADDATA;
			end if;
			RREADY <= '0';
		end procedure SingleRead;
		
		procedure SingleWrite is
			variable addr_ack : boolean := false;
			variable data_ack : boolean := false;
			variable resp_ack : boolean := false;
		begin
			AWADDR <= STD_LOGIC_VECTOR(GetByteAddress(amr));
			AWPROT <= prot;
			AWVALID <= '1';
			WDATA <= amr.writedata;
			WSTRB <= amr.byteen;
			WVALID <= '1';
			
			while not (addr_ack and data_ack) loop
				wait until rising_edge(ACLK);
				if AWREADY = '1' then
					AWVALID <= '0';
					addr_ack := true;
				end if;
				if WREADY = '1' then
					WVALID <= '0';
					data_ack := true;
				end if;
			end loop;
			
			BREADY <= '1';
			wait until rising_edge(ACLK) and BVALID='1';
			BREADY <= '0';
		end procedure SingleWrite;
	
	begin
		InitializeAmr(amr);
		amr.alert <= ALRT;
		
		loop
			AWVALID <= '0';
			WVALID <= '0';
			BREADY <= '0';
			ARVALID <= '0';
			RREADY <= '0';
			
			WaitForTransaction(ACLK, amr.rdy, amr.ack);
			if ARESETn = '0' then
				wait until ARESETn = '1';
			end if;
			case amr.trans is
				when SINGLE =>
					if amr.write = AMR_READ then
						SingleRead;
					else
						SingleWrite;
					end if;
					
				when others =>
					Alert(ALRT, "Transaction type " & TransactionType'image(amr.trans) &
						" not supported by model.", FAILURE);
						
			end case;
		end loop;
	end process AXI;

end architecture Behavioral;
