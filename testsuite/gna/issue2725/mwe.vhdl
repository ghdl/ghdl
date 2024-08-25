package mwe_pkg is generic (N : natural);
 alias N1 : integer is N;
end package;

entity mwe is
end entity;

architecture bhv of mwe is
  package inst_pkg is new work.mwe_pkg generic map (N => 0);
begin
end architecture;

