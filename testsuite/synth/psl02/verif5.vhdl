vunit verif5 (assert2(behav))
{
  signal cnti : natural;

  -- Aliases to objects
  alias cnti_a is cnti;
  alias clk_a is clk;
  alias rst_a is rst;
  alias cnt_a is cnt;

  -- Alias to non-object
  alias incr_a is incr[integer return integer];

  cnti_a <= to_integer(cnt);

  default clock is rising_edge(clk_a);

  assert always cnt_a /= 5 abort rst_a;
  assert always rst_a = rst;
  assert always cnt_a = cnt;
  assert always cnti_a = cnt;
  assert always cnti_a = incr_a(prev(cnti_a));
}
