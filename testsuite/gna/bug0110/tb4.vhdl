package pkg4 is
  type my_arr is array (1 to 2) of bit_vector (7 downto 0);
end pkg4;

use work.pkg4.all;

entity ent4 is
  port (v : out my_arr;
        b : in bit);
end ent4;

architecture behav of ent4 is
begin
  v <= (others => (others => b));
end behav;

entity top4 is
end top4;

use work.pkg4.all;
architecture behav of top4 is
  signal s : bit_vector (7 downto 0);
  signal b : bit;
begin
  dut : entity work.ent4
    port map (
      v(1)(3 downto 2) => s (3 downto 2),
      v(2)(7 downto 6) => s (7 downto 6),
      b => b);
  b <= '0';
end behav;
