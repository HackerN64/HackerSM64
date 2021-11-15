#include <ultra64.h>
// jk i dont think this file is needed lol
// update: it was needed :(
// update 2: gu.h already has macros for this :(

// hopefully I'll only need s15.16
// ^ clueless

// Q to float

// To convert a number from Qm.n format to floating point:

//     Convert the number to floating point as if it were an integer, in other words remove the binary point
//     Multiply by 2^−n

f32 qtof(int q, int decimal_n) {
	f32 mul = (1.0f / (float) (1 << decimal_n));
	return ((float)q) * mul;
}

// Float to Q

// To convert a number from floating point to Qm.n format:

//     Multiply the floating point number by 2^n
//     Round to the nearest integer
int ftoq(f32 f, int decimal_n) {
	f *= (float) (1 << decimal_n);
	return (int)f;
}


