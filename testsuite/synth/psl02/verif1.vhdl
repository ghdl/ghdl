vunit verif1 (assert2)
{
  default clock is rising_edge(clk);
  assert always cnt /= 5 abort rst;
}
