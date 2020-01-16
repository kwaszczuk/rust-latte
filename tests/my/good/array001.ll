; compilation options:
;     mem2reg: true
;     optimizations-branches: true
;     optimizations-constants: true
;     optimizations-dead-code: true
;     optimizations-phis: true
;     optimize: true

declare void @printString(i8*)
declare i8* @calloc(i64, i64)

@static_0 = constant [ 5 x i8 ] c"true\00", align 1
@static_1 = constant [ 6 x i8 ] c"false\00", align 1

define i32 @main() {
	br label %l_0
l_0:
	%v_2 = call i8* @calloc(i64 46, i64 4)
	%v_3 = bitcast i8* %v_2 to i32*
	store i32 42, i32* %v_3
	%v_4 = bitcast i8* %v_2 to i32*
	%v_5 = bitcast i32* %v_4 to i32*
	%v_6 = load i32, i32* %v_5
	%v_7 = icmp eq i32 %v_6, 42
	br i1 %v_7, label %l_1, label %l_2
l_1:
	%v_8 = getelementptr [ 5 x i8 ], [ 5 x i8 ]* @static_0, i32 0, i32 0
	call void @printString(i8* %v_8)
	br label %l_3
l_2:
	%v_10 = getelementptr [ 6 x i8 ], [ 6 x i8 ]* @static_1, i32 0, i32 0
	call void @printString(i8* %v_10)
	br label %l_3
l_3:
	ret i32 0
}