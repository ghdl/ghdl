 library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
entity test_tb is
port(
  dout : std_logic
);
end entity;

architecture sim of test_tb is
  
  signal rst : std_logic := '1';
  signal clk : std_logic := '1';
  signal din : std_logic ;

  type myrec is
  record
    rst : std_logic;
    vld : std_logic; 
  end record;

  procedure myproc(
    variable din : in std_logic;
    variable dout : out std_logic
  ) is
  begin
    dout := din;
  end procedure;

begin
end architecture;
