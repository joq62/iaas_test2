all:
	ok
unit_test:
	rm -rf ebin/* src/*.beam *.beam test_src/*.beam test_ebin/*;
	rm -rf  *~ */*~  erl_cra*;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
#	test application
	erlc -o test_ebin test_src/*.erl;
	erl -pa ebin -pa test_ebin\
	    -pa */ebin\
	    -setcookie abc\
	    -iaas config_file nodes\
	    -sname iaas\
	    -mnesia dir mneisa_dir\
	    -run iaas_unit_test test
