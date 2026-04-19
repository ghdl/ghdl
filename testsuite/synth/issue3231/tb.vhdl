library ieee;

entity rec is
  generic (
    i : integer := 10
  );
end entity;

architecture a of rec is
begin
  gen: if i /= 0 generate
    rec_inst: entity work.rec generic map (i => i - 1);
  end generate;
end architecture;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb is
end entity;

architecture synthesis of tb is
begin
  rec0: entity work.rec generic map (i => 3);
  rec1: entity work.rec generic map (i => 1);

end architecture synthesis;
