package st_pkg is
  type st_t is record
    data : bit_vector(7 downto 0);
  end record;

  view st_source_v of st_t is
    data : out;
  end view;
end package;

--

use work.st_pkg.all;

entity st_skid is
  port (source : view st_source_v);
end entity;

architecture rtl of st_skid is
begin
  source.data <= x"08";
end architecture;

--

use work.st_pkg.all;

entity top is
  port (v : out bit_vector(7 downto 0));
end entity;

architecture rtl of top is
  signal s : st_t;
begin
  st_skid_inst : entity work.st_skid port map (source => s);
  v <= s.data;
end architecture;
