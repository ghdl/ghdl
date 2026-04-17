library ieee;
use ieee.std_logic_1164.all;


entity tb is
  port (
    clk : in  std_logic;
    a   : in  std_logic_vector(31 downto 0);
    b   : out std_logic_vector(31 downto 0)
    );
end entity;

architecture synthesis of tb is
  type xyz is array (0 to 1) of std_logic_vector (31 downto 0);
  signal \:\ : xyz := (others => (others => '1'));
  signal idx : integer;
begin
  p : process (clk)
  begin
    if rising_edge(clk) then
      \:\(idx) <= a;
      b        <= \:\(idx);
    end if;
  end process;
end architecture;
