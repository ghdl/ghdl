--********************************************************************************************************************--
--! @brief Testbench for decoder simulator
--********************************************************************************************************************--
library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;

library STD;
use std.env.all; 

use work.irqc_pif_pkg.all;
use work.ilos_sim_pkg.all;

--! Local libraries
library work;

--! Entity/Package Description
entity tb_irqc is
end entity tb_irqc;

architecture tb of tb_irqc is

-- Signal declarations
SIGNAL arst_sig: 	std_logic;
SIGNAL clk_sig: 	std_logic;
SIGNAL cs_sig: 		std_logic;
SIGNAL addr_sig: 	unsigned(2 DOWNTO 0);
SIGNAL wr_sig: 		std_logic;
SIGNAL rd_sig: 		std_logic;
SIGNAL din_sig: 	std_logic_vector(7 DOWNTO 0);
SIGNAL dout_sig: 	std_logic_vector(7 DOWNTO 0);
SIGNAL p2c_sig: 	t_p2c;
SIGNAL c2p_sig: 	t_c2p;
SIGNAL run_sig:		std_logic;

signal sbi_if : t_sbi_if(addr(2 downto 0), wdata(7 downto 0), rdata(7 downto 0)) := init_sbi_if_signals(3, 8);
 

--! Component declaration for Behavioral Decoder

COMPONENT irqc_pif IS
  PORT(
    arst : in  std_logic;
    clk  : in  std_logic;
    -- CPU interface
    cs   : in  std_logic;
    addr : in  unsigned;
    wr   : in  std_logic;
    rd   : in  std_logic;
    din  : in  std_logic_vector(7 downto 0);
    dout : out std_logic_vector(7 downto 0)   := (others => '0');
    --
    p2c  : out t_p2c;
    c2p  : in  t_c2p
  );
END COMPONENT irqc_pif;

BEGIN


IRQC: irqc_pif PORT MAP (
	arst => arst_sig,
	clk  => clk_sig,
	cs   => cs_sig,
	addr => addr_sig,
	wr   => wr_sig, 
	rd   => rd_sig, 
	din  => din_sig,
	dout => dout_sig,
	p2c  => p2c_sig,
	c2p  => c2p_sig
);
	
-- Clock generation with concurrent procedure call
clk_gen(clk_sig, 50.0E6, 0 fs, run_sig);  -- 50 MHz clock

-- Time resolution show
-- assert FALSE report "Time resolution: " & time'image(time'succ(0 fs)) severity NOTE;


tb: PROCESS
BEGIN	
	run_sig <= '1';
	arst_sig <= '1';
	
	cs_sig <= '0';
	addr_sig <= to_unsigned(0, addr_sig'length);
	wr_sig <= '0';
	rd_sig <= '0';
	din_sig <= (others => '0');
	c2p_sig.aro_irr <= (others => '0');
	c2p_sig.aro_ipr <= (others => '0');
	c2p_sig.aro_irq2cpu_allowed <= '0';

	wait for 200 ns;
	arst_sig <= '0';
	
	wait for 205 nS;
	rd_sig <= '1';
	cs_sig <= '1';
	
	wait for 20 ns;
	addr_sig <= to_unsigned(C_ADDR_IER, addr_sig'length);
	wait for 20 nS;
	addr_sig <= to_unsigned(C_ADDR_IPR, addr_sig'length);
	wait for 20 nS;
	addr_sig <= to_unsigned(C_ADDR_IRQ2CPU_ALLOWED, addr_sig'length);
	wait for 20 nS;
	addr_sig <= to_unsigned(C_ADDR_IER, addr_sig'length);
	din_sig <= X"15";
	wr_sig <= '1';
	rd_sig <= '0';
	wait for 20 ns;
	cs_sig <= '0';
	wr_sig <= '0';
	rd_sig <= '1';
	cs_sig <= '1';
	wait for 80 ns;
	
	
	cs_sig <= '0';
	rd_sig <= '0';
	wait for 200 nS;
	
	-- End simulation
	run_sig <= '0';
	wait for 200 nS;
	finish;
	
	
END PROCESS tb;

	
end architecture tb;
