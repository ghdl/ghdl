package fft_package is
  TYPE complex IS ARRAY(0 TO 1) OF INTEGER;
  function butterfly(X1: complex )return complex;
END fft_package;

package body fft_package is

    function butterfly ( X1: complex )return complex is
      VARIABLE Y1  : complex := X1;

    BEGIN
      return X1;
      return Y1;
    END butterfly;
end fft_package;
