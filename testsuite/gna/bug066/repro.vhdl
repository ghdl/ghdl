package foo is
  procedure bar (signal a, b : in bit; signal c : out bit);
  procedure bar (signal a, b, c : in bit; signal d : out bit);
end package foo;

package body foo is
  procedure bar (signal a, b : in bit; signal c : out bit) is
  begin
    c <= a xor b;
  end procedure bar;

  procedure bar (signal a, b, c : in bit; signal d : out bit)
  is
  begin
    d <= a xor b xor c;
  end procedure bar;
end package body foo;
