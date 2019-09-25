library ieee;
  use ieee.std_logic_1164.all;

entity sub_slv is
  port (
    clk : in std_logic;
    a : in std_logic_vector(7 downto 0);
    b : out std_logic_vector(7 downto 0)
  );
end sub_slv;

architecture rtl of sub_slv is
begin
  process(clk)
  begin
    if rising_edge(clk) then
      b <= a;
    end if;
  end process;
end rtl;


library ieee;
  use ieee.std_logic_1164.all;

entity repro_slv is
  port (
    clk : in std_logic;
    a : in std_logic_vector(7 downto 0);
    b : out std_logic_vector(7 downto 0)
  );
end repro_slv;

architecture rtl of repro_slv is
begin
  i_sub_slv : entity work.sub_slv
  port map (
    clk => clk,
    a => a,
    b => b
  );
end rtl;
