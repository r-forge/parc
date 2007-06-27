#define         HOST            0

/*
#define         MAX_ARRAY       100000
#define         MAX_ARRAY       2000000
*/
#define         MAX_ARRAY       1200
#define         MAX_ONED_ARRAY      2000000

#define         MAX_NODES       1 << MAX_DIM

#define         SEED            17


enum            mess_type       {
                                 DATA, DATA_A, DATA_B,
				 DIAGONAL, A,
		                 POINT, AFTER, C,
				FINAL,
				ODD,
				ODDRESULT,
				EVEN,
				EVENRESULT,
                TIME,
};
