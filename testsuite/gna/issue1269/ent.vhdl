library ieee;
context ieee.ieee_std_context;

entity ent is
end ent;

architecture arch of ent is
begin
  process
    variable color: bit_vector(2 downto 0);
    variable lcol:  std_logic_vector(31 downto 0);
  begin
        lcol := (
          23 downto 16 => color(2),
          15 downto 8 => color(1),
          7 downto 0 => color(0),
          others=> '0'
        );
    wait;
  end process;
end architecture;
