package pkg is
  type my_rec is record
    adr : bit_vector (7 downto 0);
  end record;
end pkg;

use work.pkg.all;

entity ent is
  port (v : out my_rec;
        b : in bit);
end ent;

architecture behav of ent is
begin
  v.adr <= (others => b);
end behav;

entity top is
end top;

use work.pkg.all;
architecture behav of top is
  signal s : bit_vector (7 downto 0);
  signal b : bit;
begin
  dut : entity work.ent
    port map (
      -- ERROR: missing 1 downto 0!
      v.adr (3 downto 2) => s (3 downto 2),
      v.adr (7 downto 4) => s (7 downto 4),
      b => b);
  b <= '0';
end behav;
