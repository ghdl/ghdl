library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library work;


entity ENDAT_TB is
end ENDAT_TB;


architecture BEHAVIORAL of ENDAT_TB is

  -- Component declaration
  component master
    port (	 -- local clock
 		clk: in std_logic;
 		rst: in std_logic;

 		-- generated clock for slave
		 -- fdiv divides CLK_FREQ
		 ma_fdiv: in unsigned;
		 ma_clk: out std_logic;

		 -- master out, slave in
		 mosi: out std_logic;
		 miso: in std_logic;

		 -- gate to drive output (1 to drive it)
		 gate: out std_logic;

		 -- data from slave, and data length
		 data: out std_logic_vector;
		 len: in unsigned;

		 -- encoder type
		 enc_type: in integer;

		 -- ssi specific control registers
		 ssi_flags: in std_logic_vector;
		 ssi_delay_fdiv: in unsigned
	); end component;

-- Configuration
for u_master: master use entity work.master(absenc_master_rtl);

-- Clock period
constant period: time := 10 ns;

-- Signals
signal clk, rst, ma_clk, mosi, miso, gate, down, up: std_logic;
signal cnt_out: std_logic_vector (3 downto 0);
signal ma_fdiv, ssi_delay_fdiv, len: unsigned(31 downto 0);
signal enc_type: integer;
signal data, ssi_flags: std_logic_vector(31 downto 0);

begin

  -- Instantiate counter...
  u_master : master port map (
	clk => clk,
	rst => rst,

	ma_fdiv => ma_fdiv,
	ma_clk => ma_clk,

	mosi => mosi,
	miso => miso,
	
	gate => gate,

	data => data,
	len => len,

	enc_type => enc_type,

	ssi_flags => ssi_flags,
	ssi_delay_fdiv => ssi_delay_fdiv	
    
  );

  -- Process for applying patterns
  process

    -- Helper to perform one clock cycle...
    procedure run_cycle is
    begin
      clk <= '0';
      wait for period / 2;
      clk <= '1';
      wait for period / 2;
    end procedure;

  begin
	
    -- Reset counter...
    --down <= '1'; up <= '1';
    --run_cycle;
    --assert cnt_out = "0000" report "Reset does not work";

    -- Count up and keep state...
   -- for n in 1 to 20 loop
     -- down <= '0'; up <= '1';
    --  run_cycle;
    --  assert cnt_out = std_logic_vector (to_unsigned (n mod 16, 4)) report "Counting up does not work";
    --  down <= '0'; up <= '0';
    --  run_cycle;
     -- assert cnt_out = std_logic_vector (to_unsigned (n mod 16, 4)) report "Keeping the state does not work";
   -- end loop;

    -- Count down and keep state...
   -- down <= '1'; up <= '1';
   -- run_cycle;
   -- for n in 15 downto integer(-5) loop
    --  down <= '1'; up <= '0';
     -- run_cycle;
     -- assert cnt_out = std_logic_vector (to_unsigned (n mod 16, 4)) report "Counting down does not work";
     -- down <= '0'; up <= '0';
    --  run_cycle;
    --  assert cnt_out = std_logic_vector (to_unsigned (n mod 16, 4)) report "Keeping the state does not work";
   -- end loop;

    -- Print a note & finish simulation...
   -- assert false report "Simulation finished" severity note;
   -- wait;

  end process;

end BEHAVIORAL;

