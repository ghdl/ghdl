vunit verif3 (assert2(behav))
{
  default clock is rising_edge(clk);
  assume always val < 10;
  assert always val /= 5 abort rst;
}
