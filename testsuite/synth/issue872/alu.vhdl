library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity alu is
  port (
    clk : in std_logic;
    rst : in std_logic;
    opcode : in std_logic_vector(15 downto 0);
    a : in std_logic;
    b : in std_logic;
    y : out std_logic
  );
end alu;

architecture mux of alu is
  signal ci : std_logic;
  signal co : std_logic;
  signal mux1, mux2: std_logic_vector(7 downto 0);
begin

  process(a, b, ci, mux1, mux2)
    variable sel : unsigned(2 downto 0);
  begin
    sel := a & b & ci;
    y <= mux1(to_integer(sel));
    co <= mux2(to_integer(sel));
  end process;

  process(clk, rst)
  begin
    if(rst = '0') then
      ci <= '0';
      mux1 <= (others => '0');
      mux2 <= (others => '0');
    elsif(rising_edge(clk)) then
      ci <= co;
      mux1 <= opcode(15 downto 8);
      mux2 <= opcode(7 downto 0);
    end if;
  end process;
end mux;
