library ieee;
    use ieee.std_logic_1164.all;

library work;
    use work.pkg_foo_public.all;

entity tb_foo is
  generic
  (  runner_cfg     : string := ""
   ; encoded_tb_cfg : string := ""
  );
end entity tb_foo;
architecture tb of tb_foo is 

  signal s_Clk : std_ulogic := '0'; 
  signal s_Rst : std_ulogic;
  
  signal s_M2S : t_foo_o;
  signal s_S2M : t_foo_i;

begin

    s_Clk  <= not(s_Clk) after 5.1 ns;

    P_Stim:
    process
    is
    begin
        wait for 10 ns;
        s_Rst <= '1'; wait for 100 ns;
        s_Rst <= '0'; wait for 100 ns;
        wait;
    end process;

   E_Uut:
   entity work.foo(rtl)
   port map
   (  Clk => s_Clk
    , Rst => s_Rst
    , M2S => s_M2S
    , S2M => s_S2M
   );
end architecture tb;