library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity top is
generic (
  W : integer := 1
);
end entity top;

architecture rtl of top is

  signal write : std_logic;
  -- workaround
  signal wen   : std_logic_vector(W-1 downto 0);

begin

  process begin
    report "Hello world from top" severity note;
    wait;
  end process;

  write <= '0';

  -- workaround
  wen <= (others => write);

  u_ent: entity work.ent
  generic map(
    W => W
  )
  port map(
  -- workaround
    wen => (others => write)
--  wen => wen
  );

end rtl;
