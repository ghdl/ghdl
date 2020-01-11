entity repro_sub is
  generic (
    val : natural := 10);
  port (
    a : natural := val;
    b : out natural);
end repro_sub;

architecture behav of repro_sub is
begin
  b <= a + 1;
end behav;

entity repro is
  port (
    a : natural;
    b : out natural);
end repro;

architecture rtl of repro is
begin
  dut: entity work.repro_sub
    port map (b => b);
end rtl;


