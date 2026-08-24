library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

library ti;
use ti.crc.all;

entity crc_entity_draft is
  generic (
            word_w : integer := 4;
            crc_w : integer := 32
          );
  port (
         clk : in std_logic;
         rst : in std_logic;
         sof : in std_logic;
         eof : in std_logic;
         data : in std_logic_vector(word_w - 1 downto 0);
         crc : out std_logic_vector(crc_w - 1 downto 0)
       );
end;
architecture a_crc_entity_draft of crc_entity_draft is
  type state_t is (running, idle);
  signal state : state_t := idle;
begin

  process(all) is
    variable t : crc32;
    variable reg : std_logic_vector(N - 1 downto 0) := (others => '0');
    variable init : boolean := false;
  begin
    if rst then
      null;
    elsif rising_edge(clk) then
      if sof then
        init := t.initcrc4;
        reg := t.tablecrc4(data);
      elsif eof then
        reg := t.tablecrc4(data);
        crc <= t.finishcrc4(reg);
      else
        reg := t.tablecrc4(data);
      end if;
    end if;
  end process;

end;
