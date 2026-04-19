library ieee;
use ieee.std_logic_1164.all;

entity tb2 is
  port (
    clk : in  std_logic;
    a   : in  std_logic_vector(31 downto 0);
    b   : out std_logic_vector(31 downto 0)
    );
end entity;

architecture synthesis of tb2 is
begin
  f : for i in 0 to 0 generate
    type xyz is array (0 to 1) of std_logic_vector (31 downto 0);
    signal m   : xyz := (others => (others => '1'));
    signal idx : integer;
  begin
    p : process (clk)
    begin
      if rising_edge(clk) then
        m(idx) <= a;
        b      <= m(idx);
      end if;
    end process;
  end generate;
end architecture;
