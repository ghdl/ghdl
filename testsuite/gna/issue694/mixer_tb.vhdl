entity mixer_tb is
end;

use work.mixer_pkg.all;

architecture behav of mixer_tb is
  signal s : sample_array(0 to 127)(3 downto 0);
begin
  inst : entity work.mixer generic map (sample_bits => 4)
    port map(i_samples => s);
end behav;
