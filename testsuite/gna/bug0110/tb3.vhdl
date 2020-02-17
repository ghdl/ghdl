package pkg3 is
  type my_rec is record
    adr : bit_vector (7 downto 0);
  end record;
end pkg3;

use work.pkg3.all;

entity ent3 is
  port (v : out my_rec;
        b : in bit);
end ent3;

architecture behav of ent3 is
begin
  v.adr <= (others => b);
end behav;

entity top3 is
end top3;

use work.pkg3.all;
architecture behav of top3 is
  signal s : bit_vector (7 downto 0);
  signal b : bit;
begin
  dut : entity work.ent3
    port map (
      -- ERROR: missing 1 downto 0!
      v.adr (3 downto 2) => s (3 downto 2),
      v.adr (7 downto 6) => s (7 downto 6),
      b => b);
  b <= '0';
end behav;
