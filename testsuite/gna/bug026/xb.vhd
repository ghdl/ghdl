package body dosomething is

  procedure dosomething_c_hello (
    constant r            : in    dosomething_t);

  attribute foreign of dosomething_c_hello : procedure is "VHPIDIRECT dosomething_c_hello";

  procedure dosomething_c_hello (
    constant r            : in    dosomething_t) is
  begin
    assert false severity failure;
  end dosomething_c_hello;

  procedure dosomething_hello (
    variable r : inout dosomething_t) is
  begin
    dosomething_c_hello(r);
  end dosomething_hello;

end dosomething;
