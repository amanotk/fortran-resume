# -*- Makefile -*-

#F90 = gfortran -fbounds-check # for debug
F90 = gfortran
KADAI = kadai2 kadai3 kadai4 kadai5 kadai6 kadai7 kadai8_1 kadai8_2

default: exec_kadai2 exec_kadai3 exec_kadai4 exec_kadai5 exec_kadai6 \
	exec_kadai7 exec_kadai8

exec_kadai2: kadai2
	./kadai2    10000 > kadai2.dat
	./kadai2.py 10000 > kadai2_answer.dat
	diff -Ew kadai2.dat kadai2_answer.dat

exec_kadai3: kadai3
	echo 1.3 | ./kadai3 > kadai3_1.dat
	echo 1.4 | ./kadai3 > kadai3_2.dat
	echo 1.5 | ./kadai3 > kadai3_3.dat
	echo 1.6 | ./kadai3 > kadai3_4.dat
	gnuplot kadai3.gpl

exec_kadai4: kadai4
	./kadai4 < kadai4_1.txt > kadai4_1.dat
	./kadai4 < kadai4_2.txt > kadai4_2.dat
	./kadai4 < kadai4_3.txt > kadai4_3.dat
	gnuplot kadai4.gpl

exec_kadai5: kadai5
	./kadai5 0 0 > kadai5_0.dat
	./kadai5 1 0 > kadai5_1.dat
	./kadai5 2 0 > kadai5_2.dat
	./kadai5 3 0 > kadai5_3.dat
	./kadai5 4 0 > kadai5_4.dat
	./kadai5 5 0 > kadai5_5.dat
	gnuplot kadai5.gpl

exec_kadai6: kadai6
	./kadai6 > kadai6.dat
	gnuplot kadai6.gpl

exec_kadai7: kadai7
	./kadai7 > kadai7.dat
	gnuplot kadai7.gpl

exec_kadai8: kadai8_1 kadai8_2
	./kadai8_1 < kadai8_1a.txt
	./kadai8_1 < kadai8_1b.txt
	./kadai8_2 < kadai8_2a.txt
	./kadai8_2 < kadai8_2b.txt
	./kadai8_2 < kadai8_2c.txt


kadai2: kadai2.f90
	$(F90) -o $@ $^

kadai3: kadai3.f90
	$(F90) -o $@ $^

kadai4: kadai4.f90
	$(F90) -o $@ $^

kadai5: kadai5.f90
	$(F90) -o $@ $^

kadai6: kadai6_sort.f90 kadai6.f90
	$(F90) -o $@ $^

kadai7: kadai7.f90
	$(F90) -o $@ $^

kadai8_1: kadai8_1.f90
	$(F90) -o $@ $^

kadai8_2: kadai8_2.f90
	$(F90) -o $@ $^


clean:
	rm -fr $(KADAI) *.o *.mod *.dat *.png
