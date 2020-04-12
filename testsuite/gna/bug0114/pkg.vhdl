package pkg is
  procedure say_hello;
end pkg;

package body pkg is
  procedure say_hello is
  begin
    report "Hello";
  end say_hello;
end pkg;
