library ieee;
use ieee.std_logic_1164.all;

entity example_recursive_instantiation_entity_style is
  generic (
    A_WIDTH : positive;
    B_WIDTH : positive);
  port (
    in_a : in std_logic_vector(A_WIDTH-1 downto 0);
    in_b : in std_logic_vector(B_WIDTH-1 downto 0));
end example_recursive_instantiation_entity_style;


architecture rtl of example_recursive_instantiation_entity_style is
begin

  ------------------------------------------------------------------------------
  -- If A_WIDTH < B_WIDTH, swap the inputs.
  -- The remainder of this module assumes that A_WIDTH >= B_WIDTH.
  ------------------------------------------------------------------------------
  if_gen_a_width_lt_b_width : if (A_WIDTH < B_WIDTH) generate
  begin
    assert false
      report "A_WIDTH=" & integer'image(A_WIDTH) & " < B_WIDTH=" & integer'image(B_WIDTH) & ", so swap them."
      severity note;

    example_recursive_instantiation_entity_style_i : entity work.example_recursive_instantiation_entity_style
      generic map (
        A_WIDTH => B_WIDTH,
        B_WIDTH => A_WIDTH)
      port map (
        in_a => in_b,
        in_b => in_a);
  end generate if_gen_a_width_lt_b_width;

  if_gen_a_width_ge_b_width : if (A_WIDTH >= B_WIDTH) generate
  begin
    assert false
      report "Do stuff with A_WIDTH=" & integer'image(A_WIDTH) & " >= B_WIDTH=" & integer'image(B_WIDTH) & "..."
      severity note;
  end generate if_gen_a_width_ge_b_width;

end rtl;
