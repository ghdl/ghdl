entity e1 is
port (
  CLK: in std_logic;
  RST: in std_logic;
  Q:  out std_logic_vector(7 downto 0)
);
end e1;

architecture behav of e1 is
begin
  assert false report "arch" severity note;
end behav;
