package filpkg is
	type foo;
	type foo_acc is access foo;

	type foo is file of character;
end package filpkg;
