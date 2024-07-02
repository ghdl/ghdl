library ieee;
   use ieee.std_logic_1164.all;
   use ieee.numeric_std_unsigned.all;

entity ent is
   port (
      clk_i     : in    std_logic;
      rst_i     : in    std_logic;
      s_val_n_i : in    std_logic_vector(7 downto 0);
      s_val_d_i : in    std_logic_vector(7 downto 0);
      m_res_q_o : out   std_logic_vector(7 downto 0)
   );
end entity ent;

architecture synthesis of ent is

   pure function leading_index (
      arg : std_logic_vector
   ) return natural is
   begin
      assert arg /= 0;
      --
      for i in arg'range loop
         if arg(i) = '1' then
            return i;
         end if;
      end loop;

      -- This should never occur
      assert false;
      return 0;
   end function leading_index;

begin

   fsm_proc : process (clk_i)
      variable index_res_v : natural range 0 to 7;
      variable index_val_v : natural range 0 to 7;
      variable shift_v     : natural range 0 to 7;
   begin
      if rising_edge(clk_i) then
         if s_val_n_i /= 0 then
            index_res_v := leading_index(s_val_n_i);
            index_val_v := leading_index(s_val_d_i);
            if index_res_v >= index_val_v then
               shift_v   := index_res_v - index_val_v;
               m_res_q_o <= shift_left(s_val_d_i, shift_v);
            else
               m_res_q_o <= s_val_d_i;
            end if;
         end if;
      end if;
   end process fsm_proc;

end architecture synthesis;
