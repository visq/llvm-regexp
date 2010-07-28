; ModuleID = 'main.bc'

declare i32 @matcher() nounwind;

define i32 @main(i32, i32 **) nounwind {
bb.1:
  %2 = invoke i32 @matcher() 
					to label %bb.3 unwind label %bb.2
bb.2:
  br label %bb.3
bb.3:
  %3 = phi i32 [ %2, %bb.1 ], [ -1, %bb.2 ]
  ret i32 %3
}

declare noalias i8* @malloc(i32) nounwind

; Compiled with TryLLVM

@.str = private constant [4 x i8] c"%d \00", align 1 ; <[4 x i8]*> [#uses=1]
@.str1 = private constant [6 x i8] c"| %d\0A\00", align 1 ; <[6 x i8]*> [#uses=1]

define void @matcher_dump(i32* nocapture %arr, i32 %sz, i32 %final) nounwind {
entry:
  %0 = icmp sgt i32 %sz, 0                        ; <i1> [#uses=1]
  br i1 %0, label %bb.nph, label %bb2

bb.nph:                                           ; preds = %entry
  %tmp = zext i32 %sz to i64                      ; <i64> [#uses=1]
  br label %bb

bb:                                               ; preds = %bb, %bb.nph
  %indvar = phi i64 [ 0, %bb.nph ], [ %indvar.next, %bb ] ; <i64> [#uses=2]
  %scevgep = getelementptr i32* %arr, i64 %indvar ; <i32*> [#uses=1]
  %1 = load i32* %scevgep, align 4                ; <i32> [#uses=1]
  %2 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([4 x i8]* @.str, i64 0, i64 0), i32 %1) nounwind ; <i32> [#uses=0]
  %indvar.next = add i64 %indvar, 1               ; <i64> [#uses=2]
  %exitcond = icmp eq i64 %indvar.next, %tmp      ; <i1> [#uses=1]
  br i1 %exitcond, label %bb2, label %bb

bb2:                                              ; preds = %bb, %entry
  %3 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([6 x i8]* @.str1, i64 0, i64 0), i32 %final) nounwind ; <i32> [#uses=0]
  ret void
}

declare i32 @printf(i8* nocapture, ...) nounwind