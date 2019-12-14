library ieee;
  use ieee.std_logic_1164.all;

entity repro1 is
  generic (
    g : std_logic := '1'
  );
  port (
    i : in std_logic_vector(7 downto 0);
    o : out std_logic_vector(7 downto 0)
  );
end repro1;

architecture rtl of repro1 is
begin
  process (i)
    variable slv_out : std_logic_vector(7 downto 0);
  begin
    if g = '0' then
      slv_out := i;
    elsif g = '1' then
      slv_out := not i;
    end if;
    o <= slv_out;
  end process;
end rtl;
