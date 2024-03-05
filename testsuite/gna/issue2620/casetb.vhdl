use std.env.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity case_questionmark_tb is
   generic (
      DEC_WIDTH_G : positive := 4;
      ENC_WIDTH_LOG_C : natural := natural(ceil(log2(real(DEC_WIDTH_G))))
   );
end case_questionmark_tb;

architecture behavior of case_questionmark_tb is

   constant T_C  : time := 10.0 ns;

   -- local signals
   signal dec_vld        : std_logic_vector(DEC_WIDTH_G    -1 downto 0);  --! decoded valid vector input (must be one-hot)
   signal enc_idx_case   : std_logic_vector(ENC_WIDTH_LOG_C-1 downto 0);  --! encoding index
   signal enc_idx_select : std_logic_vector(ENC_WIDTH_LOG_C-1 downto 0);  --! encoding index

begin

   enc_case : process (all)
   begin
      case? (dec_vld) is
         when 4B"---1" => enc_idx_case <= 2D"0";
         when 4B"--10" => enc_idx_case <= 2D"1";
         when 4B"-100" => enc_idx_case <= 2D"2";
         when 4B"1000" => enc_idx_case <= 2D"3";
         when others   => enc_idx_case <= (others => 'X');
      end case?;
   end process enc_case;

   with (dec_vld) select?
   enc_idx_select <= 2D"0" when 4B"---1",
                     2D"1" when 4B"--10",
                     2D"2" when 4B"-100",
                     2D"3" when 4B"1000",
                     (others => 'X') when others;

   p_test : process
   begin
      -- idle
      dec_vld <= (others => '0');
      wait for T_C;
      -- check
      assert enc_idx_case   = 2B"XX" report "enc_idx_case   mismatch at idle input" severity ERROR;
      assert enc_idx_select = 2B"XX" report "enc_idx_select mismatch at idle input" severity ERROR;
      wait for T_C;

      for i in 0 to DEC_WIDTH_G-1 loop
         -- i-th bit is active
         dec_vld             <= (others => 'X');
         dec_vld(i downto 0) <= (others => '0');
         dec_vld(i)          <=            '1' ;
         wait for T_C;
         -- check
         assert enc_idx_case   = std_logic_vector(to_unsigned(i, ENC_WIDTH_LOG_C)) report "enc_idx_case   mismatch at index=" & integer'image(i) severity ERROR;
         assert enc_idx_select = std_logic_vector(to_unsigned(i, ENC_WIDTH_LOG_C)) report "enc_idx_select mismatch at index=" & integer'image(i) severity ERROR;
         wait for T_C;
      end loop;

      -- end simulation
      wait for T_C;
      report "Simulation end.";
      finish;
   end process p_test;

end behavior;
