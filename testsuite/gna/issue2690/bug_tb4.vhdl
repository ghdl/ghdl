package TestGenPkg is
  generic(type DAT_G; IDL_G : DAT_G);
end TestGenPkg;

library ieee;
use ieee.std_logic_1164.all;

entity ghdl_bug_register is
   generic (type DAT_G);
   port (
      -- system signals
      clk_i  : in  std_logic;
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ghdl_bug_tb is
   generic (
     WIDTH_G : positive := 8;
     g_n : natural := 1
   );
end ghdl_bug_tb;

architecture behavior of ghdl_bug_tb is
   package TestDefPkg is new work.TestGenPkg generic map (
      DAT_G => std_logic_vector(WIDTH_G-1 downto 0),
      IDL_G => (others => 'X')
   );
   use TestDefPkg.all;
   signal clk    : std_logic := '1'; -- clock
   signal rx_dat : DAT_G;
begin
   GEN_DUT: for i in 1 to g_n generate
        signal tx_dat : DAT_G;
      begin
        dut : entity work.ghdl_bug_register
             generic map (DAT_G => DAT_G)
             port map (
                  clk_i  => clk,
                     rx_dat => rx_dat,
                     tx_dat => tx_dat);
   end generate GEN_DUT;

   p_Clock : process
   begin
     for i in 1 to 3 loop
       clk <= not clk;
       wait for 5 ns;
     end loop;
     wait;
   end process p_Clock;
end behavior;


