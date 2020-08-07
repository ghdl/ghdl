library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity tb_cmpge is
end;

architecture behav of tb_cmpge is
  signal li : integer := 0;
  signal ri : integer := 0;
  signal l4 : std_logic_vector (3 downto 0) := "0000";
  signal r3 : std_logic_vector (2 downto 0) := "000";
  signal ge_v4v3 : boolean;
  signal ge_v4i :  boolean;
  signal ge_iv3 :  boolean;
begin

  dut: entity work.cmpge
    port map (
      l4 => l4,
      r3 => r3,
      li => li,
      ri => ri,
      ge_v4v3 => ge_v4v3,
      ge_v4i  => ge_v4i,
      ge_iv3  => ge_iv3);

  process
  begin
    for i in -8 to 7 loop
      li <= i;
      l4 <= conv_std_logic_vector (i, 4);
      for j in -4 to 3 loop
        r3 <= conv_std_logic_vector (j, 3);
        ri <= j;
        wait for 1 ns;
        report "v4v3: " & integer'image(i) & " < " & integer'image(j) & " = "
          & boolean'image(ge_v4v3);
        report "v4i:  " & integer'image(i) & " < " & integer'image(j) & " = "
          & boolean'image(ge_v4i);
        report "iv3:  " & integer'image(i) & " < " & integer'image(j) & " = "
          & boolean'image(ge_iv3);
      end loop;
    end loop;
    wait;
  end process;
end behav;
