vunit verif4 (assert2(behav))
{
  default clock is rising_edge(clk);
  function check_val (v : unsigned) return boolean is
  begin
    return v < 10;
  end check_val;
  assume always check_val (val);
  assert always val /= 5 abort rst;
}
