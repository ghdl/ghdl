library ieee; use ieee.std_logic_1164.all;
package rp is
  type r_t is record a : std_logic_vector(3 downto 0); b : std_logic; end record;
  type s_t is record x : std_logic_vector(3 downto 0); y : std_logic; end record;
  function to_s (v : r_t) return s_t;
end package;
package body rp is
  function to_s (v : r_t) return s_t is
  begin
    return (x => v.a, y => v.b);
  end function;
end package body;

library ieee; use ieee.std_logic_1164.all; use work.rp.all;
entity sub is port (i : in s_t; o : out std_logic_vector(3 downto 0)); end entity;
architecture a of sub is begin o <= i.x when i.y = '1' else (others => '0'); end architecture;

library ieee; use ieee.std_logic_1164.all; use work.rp.all;
entity rectop is port (ri : in r_t; ro : out std_logic_vector(3 downto 0)); end entity;
architecture a of rectop is
begin
  u : entity work.sub port map (i => to_s(ri), o => ro);
end architecture;
