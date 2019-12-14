vunit verif2 (assert2)
{
  default clock is rising_edge(clk);
  assume always cnt < 10;
  assert always cnt /= 5 abort rst;
}
