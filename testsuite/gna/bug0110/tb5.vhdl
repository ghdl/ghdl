package pkg_5 is
  type my_arr is array (natural range <>) of bit_vector (7 downto 0);
end pkg_5;

use work.pkg_5.all;

entity ent_5 is
  port (v : out my_arr;
        b : in bit);
end ent_5;

architecture behav of ent_5 is
begin
  v (1) <= (others => b);
end behav;

entity top_5 is
end top_5;

use work.pkg_5.all;
architecture behav of top_5 is
  signal s : bit_vector (7 downto 0);
  signal b : bit;
begin
  dut : entity work.ent_5
    port map (
      v(1)(3 downto 2) => s (3 downto 2),
      v(2)(7 downto 6) => s (7 downto 6),
      b => b);
  b <= '0';
end behav;
