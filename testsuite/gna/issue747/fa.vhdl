library ieee;
use ieee.std_logic_1164.all;

entity fa is
  port(
    a:in std_ulogic;
    b: in std_ulogic;
    ci: in std_ulogic;
    co: out std_ulogic;
    s: out std_ulogic);
end fa;

architecture fa_behave of fa is
begin
  s <= a xor b xor ci;
  co <= (a and b) or (a and ci) or (b and ci);
end fa_behave;
