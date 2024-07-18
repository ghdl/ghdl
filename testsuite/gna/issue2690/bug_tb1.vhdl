library ieee;
use ieee.std_logic_1164.all;

entity ghdl_bug_register is
   generic (
      -- data type
      type DAT_G
   );
   port (
      -- system signals
      clk_i  : in  std_logic;
      rst_i  : in  std_logic;
      -- data
      rx_dat : in  DAT_G;
      tx_dat : out DAT_G
   );
end ghdl_bug_register;

architecture rtl of ghdl_bug_register is
begin

   process(clk_i)
   begin
      if rising_edge(clk_i) then
         tx_dat <= rx_dat;
      end if;
   end process;

end rtl;




use std.env.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ghdl_bug_tb is
   generic (
      WIDTH_G : positive := 8;
      DUT_G : string := "dut"
   );
   package CslTbStreamDefPkg is new work.CslTbStreamGenPkg generic map (
      DAT_G => std_logic_vector(WIDTH_G-1 downto 0),
      IDL_G => (others => 'X')
   );
   use CslTbStreamDefPkg.all;
end ghdl_bug_tb;

architecture behavior of ghdl_bug_tb is

   -- Clock period definitions
   constant T_C  : time := 10.0 ns; -- Clock period constant

   -- system signals
   signal clk    : std_logic := '1'; -- clock
   signal rst    : std_logic := '1'; -- reset
   -- data
   signal rx_dat : DAT_G;
   signal tx_dat : DAT_G;
   signal tx_ref : DAT_G;

   -- Wait for a number of rising edges and TPD
   procedure tbClkPeriod(
      signal   clk_i : in std_logic;
      constant num_i : in natural := 1
   ) is
   begin
      for i in 0 to num_i-1 loop
         wait until rising_edge(clk_i);
      end loop;
   end tbClkPeriod;

begin

   -- DUT instance
   -- all DUT_G options must be of the same length to avoid tool errors
   GEN_DUT: case DUT_G generate
      when "dut" =>

         dut : entity work.ghdl_bug_register
         generic map (
            DAT_G => DAT_G
         )
         port map (
            clk_i  => clk,
            rst_i  => rst,
            rx_dat => rx_dat,
            tx_dat => tx_dat
         );

      when others =>

         tx_dat <= rx_dat;

   end generate GEN_DUT;

   -- clock source
   p_Clock : process
   begin
      clk <= '1'; wait for T_C/2;
      clk <= '0'; wait for T_C/2;
   end process p_Clock;
   
   -- clock source
   p_Delay : process(clk)
   begin
      if rising_edge(clk) then
         tx_ref <= rx_dat;
      end if;
   end process p_Delay;
   
   p_Test : process
   begin
      -- release reset
      tbClkPeriod(clk, 4);
      rst <= '0';
      tbClkPeriod(clk, 4);

      -- source data driver
      drv_loop: for i in 0 to 7 loop
         rx_dat <= std_logic_vector(to_unsigned(i, WIDTH_G));
         tbClkPeriod(clk, 1);
      end loop drv_loop;

      tbClkPeriod(clk, 4);
      -- end simulation
      finish;
   end process p_Test;

   p_Check : process
   begin
      -- skip reset
      tbClkPeriod(clk, 4);
      tbClkPeriod(clk, 4);

      -- receiver data listener
      lsn_loop: for i in 0 to 7 loop
         assert (tx_dat = tx_ref) report "ERROR: mismatch." severity ERROR;
         tbClkPeriod(clk, 1);
      end loop lsn_loop;

      -- end simulation (this code is not active, it is intended as a timeout during debugging)
      tbClkPeriod(clk, 4);
      report "Simulation timeout." severity ERROR;
      finish;
   end process p_Check;

end behavior;
