vunit issue_vunit (issue(psl)) {

  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- GHDL crash if condition evaluates to true
  test_g : if true generate

    -- This assertion holds
    CHECK_0_a : assert always (a -> b);

  end generate test_g;

}
