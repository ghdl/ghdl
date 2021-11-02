library ieee;
use ieee.std_logic_1164.all;

entity tb is port (
   clk  :  in std_logic;
   rst  :  in std_logic;
   a    :  in std_logic;
   y    : out std_logic);
end entity;

architecture beh of tb is
signal yo : std_logic;

procedure ff(signal c: in std_logic; r: in std_logic; i: in std_logic; signal o: out std_logic) is
begin
   wait until c'event and c ='1';
   if r = '1' then o <= '0'; else o <= i; end if;
end ff;

begin
   ff(clk, rst, a, yo);
   y <= yo;
end beh;
