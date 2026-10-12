subroutine FtoC(n, x)
	integer n
	double precision x(n)
	integer i
	do 100 i = 1, n
	x(i) = (x(i)-32)*(5./9.)
	100 continue
end