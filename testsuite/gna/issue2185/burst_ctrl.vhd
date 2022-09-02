-- This generates a sequence of commands with varying values
-- of write_burstcount and read_burstcount.
-- Signals start_i and wait_o are connected "upstream", in this case
-- to the keyboard LEDs, while start_o and wait_i are connected "downstream",
-- in this case to the avm_master entity.
--
-- Created by Michael Jørgensen in 2022 (mjoergen.github.io/HyperRAM).

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity burst_ctrl is
   port (
      clk_i              : in  std_logic;
      rst_i              : in  std_logic;
      -- Connect "upstream", i.e. to keyboard and LED
      start_i            : in  std_logic;
      wait_o             : out std_logic;
      -- Connect "downstream", i.e. to avm_master
      start_o            : out std_logic;
      wait_i             : in  std_logic;
      write_burstcount_o : out std_logic_vector(7 downto 0);
      read_burstcount_o  : out std_logic_vector(7 downto 0)
   );
end entity burst_ctrl;

architecture synthesis of burst_ctrl is

   constant C_MAX_BURST : std_logic_vector(7 downto 0) := X"08";

   type state_t is (
      IDLE_ST,
      BUSY_ST
   );

   signal state : state_t;

begin

   p_fsm : process (clk_i)
   begin
      if rising_edge(clk_i) then
         -- Clear outgoing request when accepted
         if wait_i = '0' then
            start_o <= '0';
         end if;

         case state is
            when IDLE_ST =>
               if start_i = '1' then
                  start_o <= '1';
                  state   <= BUSY_ST;
               end if;

            when BUSY_ST =>
               if wait_i = '0' and start_o = '0' then
                  start_o <= '1';

                  if write_burstcount_o /= C_MAX_BURST then
                     write_burstcount_o <= write_burstcount_o(6 downto 0) & "0";
                  else
                     write_burstcount_o <= X"01";
                     if read_burstcount_o /= C_MAX_BURST then
                        read_burstcount_o <= read_burstcount_o(6 downto 0) & "0";
                     else
                        read_burstcount_o <= X"01";
                        start_o <= '0';
                        state   <= IDLE_ST;
                        report "Test completed";
                     end if;
                  end if;
               end if;
         end case;

         if rst_i = '1' then
            start_o            <= '0';
            write_burstcount_o <= X"01";
            read_burstcount_o  <= X"01";
         end if;

      end if;
   end process p_fsm;

   wait_o <= '0' when state = IDLE_ST else '1';

end architecture synthesis;

