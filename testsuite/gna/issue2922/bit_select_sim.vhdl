library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
entity bit_select_sim is
  port (clk : out std_logic);
end bit_select_sim;

architecture behaviour of bit_select_sim
  is
  constant ADDR_PHY_RD  : std_logic_vector(15 downto 0) := x"C028";
  constant ADDR_ETHSIZE : std_logic_vector(15 downto 0) := x"C02C";

  -- Address ranges
  subtype AR_CSR is natural range 9 downto 2;

  subtype slv_AR_CSR is std_logic_vector(AR_CSR);

  signal sAR_TEST1 : std_logic_vector(ADDR_PHY_RD'range);
  signal sAR_TEST2 : std_logic_vector(ADDR_ETHSIZE'range);

  signal sAR_TEST3 : slv_AR_CSR;
  signal sAR_TEST4 : slv_AR_CSR;

begin
  
  sAR_TEST1(AR_CSR) <= ADDR_PHY_RD(AR_CSR);
  sAR_TEST2(AR_CSR) <= ADDR_ETHSIZE(AR_CSR);

  sAR_TEST3 <= ADDR_PHY_RD(AR_CSR);
  sAR_TEST4 <= ADDR_ETHSIZE(AR_CSR);

  bit_extract_process : process
  begin

    report "AR_CSR'range = " & integer'image(AR_CSR'high) & " downto " & integer'image(AR_CSR'low);
    report "slv_AR_CSR'range = " & integer'image(slv_AR_CSR'high) & " downto " & integer'image(slv_AR_CSR'low);
    report "ADDR_ETHSIZE'range = " & integer'image(ADDR_ETHSIZE'high) & " downto " & integer'image(ADDR_ETHSIZE'low);
    report "ADDR_ETHSIZE(AR_CSR)'range = " & integer'image(ADDR_ETHSIZE(AR_CSR)'high) & " downto " & integer'image(ADDR_ETHSIZE(AR_CSR)'low);

    report "STD_MATCH(ADDR_ETHSIZE(AR_CSR), ADDR_PHY_RD(AR_CSR)) = " & boolean'image(STD_MATCH(ADDR_ETHSIZE(AR_CSR), ADDR_PHY_RD(AR_CSR)));
    -- above should return false...

--    report "sAR_TEST1 = " & to_hstring(ADDR_PHY_RD(AR_CSR));
--    report "sAR_TEST2 = " & to_hstring(ADDR_ETHSIZE(AR_CSR));
    -- above lines shows different numbers, this is OK
    report "bool = " & boolean'image((ADDR_PHY_RD(AR_CSR) = ADDR_ETHSIZE(AR_CSR)));
    -- if different, this should be false

    --further strange behaviours:
    report "(ADDR_PHY_RD(AR_CSR)) = " & integer'image(to_integer(unsigned(ADDR_PHY_RD(AR_CSR))));
    report "(ADDR_ETHSIZE(AR_CSR)) = " & integer'image(to_integer(unsigned(ADDR_ETHSIZE(AR_CSR))));
    -- above lines shows exact same number, most probably this is NOT OK

    report "sAR_TEST3 = " & integer'image(to_integer(unsigned(sAR_TEST3))); -- there is no metavalue here
    report "sAR_TEST4 = " & integer'image(to_integer(unsigned(sAR_TEST4))); -- there is no metavalue here

    report "(ADDR_PHY_RD(AR_CSR)) = " & integer'image(to_integer(unsigned(std_logic_vector(ADDR_PHY_RD(AR_CSR))))); -- THIS LINE CRASHES GHDL
    report "(ADDR_ETHSIZE(AR_CSR)) = " & integer'image(to_integer(unsigned(std_logic_vector(ADDR_ETHSIZE(AR_CSR))))); -- THIS LINE CRASHES GHDL
    wait for 10 ns;
    wait;
  end process;

end behaviour;
