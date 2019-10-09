type vec = real list;
type matrix = real list list;

(*problem 1*)
fun MatGet(Mat : matrix, i : int, j : int) : real = 
	List.nth(List.nth(Mat, i), j)

(*problem 2*)
fun MatMean(Mat : matrix) : real = 
	let
		val sum_list = foldl op + 0.0
		val sum = sum_list (map sum_list Mat)
		val count = sum_list (map real (map length Mat))
	in
		sum / count
	end

(*problem 3*)
fun MatCrop(Mat : matrix, (H_start, H_end), (W_start, W_end)) : matrix = 
	let
		val map_crop = map (fn mat => (List.drop((List.take(mat, W_end)), W_start)))
	in
		map_crop (List.drop((List.take(Mat, H_end)), H_start))
	end

(*problem 4*)
fun MatBinary(Mat : matrix, threshold : real) : matrix = 
	map (map (fn x => (if x < threshold then 0.0 else 1.0))) Mat

(*problem 5*)
fun dotProduct(Vec1 : vec, Vec2 : vec) =
	ListPair.foldl (fn (x, y, z) => x * y + z) 0.0 (Vec1, Vec2);

fun nullMat(Mat : matrix) = 
	foldl (fn (x, y) => (x andalso y)) true (map (fn x => null(x)) Mat)
	
fun vecDotMat (Mat : matrix) (Vec : vec) =
	if nullMat(Mat) then nil
	else dotProduct(Vec, map hd Mat) :: (vecDotMat (map tl Mat) Vec)
	
fun MatDot(Mat1 : matrix, Mat2 : matrix) = 
	map (vecDotMat Mat2) Mat1

(* ~~test cases~~ *)

(* test cases for problem1 *)
val m = [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]]; 
MatGet(m, 0, 0); (*1.0*)
MatGet(m, 1, 2); (*6.0*)
MatGet(m, 2, 1); (*8.0*)

(* test cases for problem2 *)
val m = [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]];
MatMean(m); (*5.0*)
val MatA = [[1.0,3.0],[~1.0,1.1]];
MatMean(MatA); (*1.025*)

(* test cases for problem3 *)
val m = [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]];
MatCrop(m,(1,3),(1,3)); (*[[5.0,6.0],[8.0,9.0]]*)
MatCrop(m,(0,1),(0,1)); (*[[1.0]]*)

(* test cases for problem4 *)
val m = [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]];
MatBinary(m,4.0); (*[[0.0,0.0,0.0],[1.0,1.0,1.0],[1.0,1.0,1.0]]*)
val MatA = [[1.0,3.0],[~1.0,1.1]]; 
MatBinary(MatA,0.9); (*[[1.0,1.0],[0.0,1.0]]*)

(* test cases for problem5 *)
val m = [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]];
MatDot(m,m); (*[[30.0,36.0,42.0],[66.0,81.0,96.0],[102.0,126.0,150.0]]*)
val Mat1 = [[1.0]];
MatDot(Mat1,Mat1); (*[[1.0]]*)
val MatX = [[0.0,1.0,1.0],[2.0, 2.0, 2.0]];
val MatY = [[1.0,3.0,1.0],[2.0, 2.0, 0.0], [3.0, 1.0, 1.0]]; 
MatDot(MatX,MatY); (*[[5.0,3.0,1.0], [12.0,12.0,4.0]]*)