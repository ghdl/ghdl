use work.types_pkg.all;

entity design_tb is
end entity;

architecture testbench of design_tb is
  constant PERIOD : time := 10 ns;

  signal clk   : bit := '1';

  type pass_through_t is record
    texture_id         : mytype_t;
  end record;

  signal input_pass_through       : pass_through_t;

begin
  clk <= '0' after 1 ns, '1' after 2 ns;
end architecture;
