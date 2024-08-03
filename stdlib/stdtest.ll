; ModuleID = 'stdtest.c'
source_filename = "stdtest.c"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx13.0.0"

%struct.KArrBox = type { ptr, i32, i32, i32 }
%struct.KoanArray = type { ptr, i8 }

@.str = private unnamed_addr constant [40 x i8] c"Failed to allocate memory for KoanArray\00", align 1
@.str.1 = private unnamed_addr constant [28 x i8] c"Index `%u` is out of bounds\00", align 1
@.str.2 = private unnamed_addr constant [29 x i8] c"Memory Re-allocation failed.\00", align 1
@.str.3 = private unnamed_addr constant [28 x i8] c"Value `%u` is out of bounds\00", align 1
@.str.5 = private unnamed_addr constant [12 x i8] c"\09.ptr = %p\0A\00", align 1
@.str.6 = private unnamed_addr constant [17 x i8] c"\09.refcount = %u\0A\00", align 1
@.str.7 = private unnamed_addr constant [12 x i8] c"\09.len = %u\0A\00", align 1
@.str.8 = private unnamed_addr constant [12 x i8] c"\09.cap = %u\0A\00", align 1
@.str.11 = private unnamed_addr constant [5 x i8] c"%f, \00", align 1
@str = private unnamed_addr constant [34 x i8] c"Tried to operate on a freed array\00", align 1
@str.14 = private unnamed_addr constant [12 x i8] c"KoanArray {\00", align 1
@str.15 = private unnamed_addr constant [3 x i8] c"};\00", align 1
@str.16 = private unnamed_addr constant [4 x i8] c"\08\08]\00", align 1

; Function Attrs: nounwind ssp uwtable(sync)
define [2 x i64] @init_array(i32 noundef %0) local_unnamed_addr #0 {
  %2 = zext i32 %0 to i64
  %3 = tail call ptr @calloc(i64 noundef %2, i64 noundef 8) #9
  %4 = tail call dereferenceable_or_null(24) ptr @malloc(i64 noundef 24) #10
  store ptr %3, ptr %4, align 8, !tbaa.struct !5
  %5 = getelementptr inbounds i8, ptr %4, i64 8
  store <2 x i32> <i32 1, i32 0>, ptr %5, align 8
  %6 = getelementptr inbounds i8, ptr %4, i64 16
  store i32 %0, ptr %6, align 8, !tbaa.struct !12
  %7 = icmp eq ptr %3, null
  br i1 %7, label %8, label %10

8:                                                ; preds = %1
  %9 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull dereferenceable(1) @.str)
  tail call void @exit(i32 noundef 1) #11
  unreachable

10:                                               ; preds = %1
  %11 = ptrtoint ptr %4 to i64
  %12 = insertvalue [2 x i64] poison, i64 %11, 0
  %13 = insertvalue [2 x i64] %12, i64 0, 1
  ret [2 x i64] %13
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #1

; Function Attrs: mustprogress nofree nounwind willreturn allockind("alloc,zeroed") allocsize(0,1) memory(inaccessiblemem: readwrite)
declare noalias noundef ptr @calloc(i64 noundef, i64 noundef) local_unnamed_addr #2

; Function Attrs: mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0) memory(inaccessiblemem: readwrite)
declare noalias noundef ptr @malloc(i64 noundef) local_unnamed_addr #3

; Function Attrs: nofree nounwind
declare noundef i32 @printf(ptr nocapture noundef readonly, ...) local_unnamed_addr #4

; Function Attrs: noreturn
declare void @exit(i32 noundef) local_unnamed_addr #5

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #1

; Function Attrs: nounwind ssp uwtable(sync)
define i32 @len_array(ptr nocapture noundef readonly %0) local_unnamed_addr #0 {
  %2 = load ptr, ptr %0, align 8, !tbaa !13
  %3 = icmp eq ptr %2, null
  br i1 %3, label %4, label %6

4:                                                ; preds = %1
  %5 = tail call i32 @puts(ptr nonnull dereferenceable(1) @str)
  tail call void @exit(i32 noundef 1) #11
  unreachable

6:                                                ; preds = %1
  %7 = getelementptr inbounds %struct.KArrBox, ptr %2, i64 0, i32 2
  %8 = load i32, ptr %7, align 4, !tbaa !16
  ret i32 %8
}

; Function Attrs: nounwind ssp uwtable(sync)
define i32 @cap_array(ptr nocapture noundef readonly %0) local_unnamed_addr #0 {
  %2 = load ptr, ptr %0, align 8, !tbaa !13
  %3 = icmp eq ptr %2, null
  br i1 %3, label %4, label %6

4:                                                ; preds = %1
  %5 = tail call i32 @puts(ptr nonnull dereferenceable(1) @str)
  tail call void @exit(i32 noundef 1) #11
  unreachable

6:                                                ; preds = %1
  %7 = getelementptr inbounds %struct.KArrBox, ptr %2, i64 0, i32 3
  %8 = load i32, ptr %7, align 8, !tbaa !18
  ret i32 %8
}

; Function Attrs: nounwind ssp uwtable(sync)
define double @nth_array(ptr nocapture noundef readonly %0, i32 noundef %1) local_unnamed_addr #0 {
  %3 = load ptr, ptr %0, align 8, !tbaa !13
  %4 = icmp eq ptr %3, null
  br i1 %4, label %5, label %7

5:                                                ; preds = %2
  %6 = tail call i32 @puts(ptr nonnull dereferenceable(1) @str)
  tail call void @exit(i32 noundef 1) #11
  unreachable

7:                                                ; preds = %2
  %8 = getelementptr inbounds %struct.KArrBox, ptr %3, i64 0, i32 2
  %9 = load i32, ptr %8, align 4, !tbaa !16
  %10 = icmp ugt i32 %9, %1
  br i1 %10, label %14, label %11

11:                                               ; preds = %7
  %12 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull dereferenceable(1) @.str.1, i32 noundef %1)
  %13 = load ptr, ptr %0, align 8, !tbaa !13
  br label %14

14:                                               ; preds = %11, %7
  %15 = phi ptr [ %13, %11 ], [ %3, %7 ]
  %16 = load ptr, ptr %15, align 8, !tbaa !19
  %17 = zext i32 %1 to i64
  %18 = getelementptr inbounds double, ptr %16, i64 %17
  %19 = load double, ptr %18, align 8, !tbaa !20
  ret double %19
}

; Function Attrs: nounwind ssp uwtable(sync)
define void @resize_array(ptr nocapture noundef readonly %0) local_unnamed_addr #0 {
  %2 = load ptr, ptr %0, align 8, !tbaa !13
  %3 = icmp eq ptr %2, null
  br i1 %3, label %4, label %6

4:                                                ; preds = %1
  %5 = tail call i32 @puts(ptr nonnull dereferenceable(1) @str)
  tail call void @exit(i32 noundef 1) #11
  unreachable

6:                                                ; preds = %1
  %7 = load ptr, ptr %2, align 8, !tbaa !19
  %8 = getelementptr inbounds %struct.KArrBox, ptr %2, i64 0, i32 3
  %9 = load i32, ptr %8, align 8, !tbaa !18
  %10 = shl i32 %9, 1
  %11 = zext i32 %10 to i64
  %12 = shl nuw nsw i64 %11, 3
  %13 = tail call ptr @realloc(ptr noundef %7, i64 noundef %12) #12
  %14 = load ptr, ptr %0, align 8, !tbaa !13
  store ptr %13, ptr %14, align 8, !tbaa !19
  %15 = icmp eq ptr %13, null
  br i1 %15, label %16, label %18

16:                                               ; preds = %6
  %17 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull dereferenceable(1) @.str.2)
  tail call void @exit(i32 noundef 1) #11
  unreachable

18:                                               ; preds = %6
  %19 = getelementptr inbounds %struct.KArrBox, ptr %14, i64 0, i32 3
  %20 = load i32, ptr %19, align 8, !tbaa !18
  %21 = shl i32 %20, 1
  store i32 %21, ptr %19, align 8, !tbaa !18
  ret void
}

; Function Attrs: mustprogress nounwind willreturn allockind("realloc") allocsize(1) memory(argmem: readwrite, inaccessiblemem: readwrite)
declare noalias noundef ptr @realloc(ptr allocptr nocapture noundef, i64 noundef) local_unnamed_addr #6

; Function Attrs: nounwind ssp uwtable(sync)
define void @set_array(ptr nocapture noundef readonly %0, i32 noundef %1, double noundef %2) local_unnamed_addr #0 {
  %4 = load ptr, ptr %0, align 8, !tbaa !13
  %5 = icmp eq ptr %4, null
  br i1 %5, label %6, label %8

6:                                                ; preds = %3
  %7 = tail call i32 @puts(ptr nonnull dereferenceable(1) @str)
  tail call void @exit(i32 noundef 1) #11
  unreachable

8:                                                ; preds = %3
  %9 = getelementptr inbounds %struct.KArrBox, ptr %4, i64 0, i32 2
  %10 = load i32, ptr %9, align 4, !tbaa !16
  %11 = icmp ugt i32 %10, %1
  br i1 %11, label %14, label %12

12:                                               ; preds = %8
  %13 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull dereferenceable(1) @.str.3, i32 noundef %1)
  tail call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %8
  %15 = load ptr, ptr %4, align 8, !tbaa !19
  %16 = zext i32 %1 to i64
  %17 = getelementptr inbounds double, ptr %15, i64 %16
  store double %2, ptr %17, align 8, !tbaa !20
  ret void
}

; Function Attrs: nounwind ssp uwtable(sync)
define void @push_array(ptr nocapture noundef readonly %0, double noundef %1) local_unnamed_addr #0 {
  %3 = load ptr, ptr %0, align 8, !tbaa !13
  %4 = icmp eq ptr %3, null
  br i1 %4, label %5, label %7

5:                                                ; preds = %2
  %6 = tail call i32 @puts(ptr nonnull dereferenceable(1) @str)
  tail call void @exit(i32 noundef 1) #11
  unreachable

7:                                                ; preds = %2
  %8 = getelementptr inbounds %struct.KArrBox, ptr %3, i64 0, i32 2
  %9 = load i32, ptr %8, align 4, !tbaa !16
  %10 = add i32 %9, 1
  %11 = getelementptr inbounds %struct.KArrBox, ptr %3, i64 0, i32 3
  %12 = load i32, ptr %11, align 8, !tbaa !18
  %13 = icmp ult i32 %10, %12
  br i1 %13, label %31, label %14

14:                                               ; preds = %7
  %15 = load ptr, ptr %3, align 8, !tbaa !19
  %16 = shl i32 %12, 1
  %17 = zext i32 %16 to i64
  %18 = shl nuw nsw i64 %17, 3
  %19 = tail call ptr @realloc(ptr noundef %15, i64 noundef %18) #12
  %20 = load ptr, ptr %0, align 8, !tbaa !13
  store ptr %19, ptr %20, align 8, !tbaa !19
  %21 = icmp eq ptr %19, null
  br i1 %21, label %22, label %24

22:                                               ; preds = %14
  %23 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull dereferenceable(1) @.str.2)
  tail call void @exit(i32 noundef 1) #11
  unreachable

24:                                               ; preds = %14
  %25 = getelementptr inbounds %struct.KArrBox, ptr %20, i64 0, i32 3
  %26 = load i32, ptr %25, align 8, !tbaa !18
  %27 = shl i32 %26, 1
  store i32 %27, ptr %25, align 8, !tbaa !18
  %28 = getelementptr inbounds %struct.KArrBox, ptr %20, i64 0, i32 2
  %29 = load i32, ptr %28, align 4, !tbaa !16
  %30 = add i32 %29, 1
  br label %31

31:                                               ; preds = %24, %7
  %32 = phi i32 [ %30, %24 ], [ %10, %7 ]
  %33 = phi i32 [ %29, %24 ], [ %9, %7 ]
  %34 = phi ptr [ %20, %24 ], [ %3, %7 ]
  %35 = getelementptr inbounds %struct.KArrBox, ptr %34, i64 0, i32 2
  store i32 %32, ptr %35, align 4, !tbaa !16
  %36 = icmp eq i32 %33, -1
  br i1 %36, label %37, label %39

37:                                               ; preds = %31
  %38 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull dereferenceable(1) @.str.3, i32 noundef -1)
  tail call void @exit(i32 noundef 1) #11
  unreachable

39:                                               ; preds = %31
  %40 = load ptr, ptr %34, align 8, !tbaa !19
  %41 = zext i32 %33 to i64
  %42 = getelementptr inbounds double, ptr %40, i64 %41
  store double %1, ptr %42, align 8, !tbaa !20
  ret void
}

; Function Attrs: nounwind ssp uwtable(sync)
define void @free_array(ptr nocapture noundef %0) local_unnamed_addr #0 {
  %2 = load ptr, ptr %0, align 8, !tbaa !13
  %3 = icmp eq ptr %2, null
  br i1 %3, label %4, label %6

4:                                                ; preds = %1
  %5 = tail call i32 @puts(ptr nonnull dereferenceable(1) @str)
  tail call void @exit(i32 noundef 1) #11
  unreachable

6:                                                ; preds = %1
  %7 = getelementptr inbounds %struct.KArrBox, ptr %2, i64 0, i32 1
  %8 = load i32, ptr %7, align 8, !tbaa !22
  %9 = icmp eq i32 %8, 1
  br i1 %9, label %10, label %13

10:                                               ; preds = %6
  %11 = load ptr, ptr %2, align 8, !tbaa !19
  tail call void @free(ptr noundef %11)
  %12 = load ptr, ptr %0, align 8, !tbaa !13
  tail call void @free(ptr noundef %12)
  br label %15

13:                                               ; preds = %6
  %14 = add i32 %8, -1
  store i32 %14, ptr %7, align 8, !tbaa !22
  br label %15

15:                                               ; preds = %13, %10
  %16 = getelementptr inbounds %struct.KoanArray, ptr %0, i64 0, i32 1
  store i8 1, ptr %16, align 8
  store ptr null, ptr %0, align 8, !tbaa !13
  ret void
}

; Function Attrs: mustprogress nounwind willreturn allockind("free") memory(argmem: readwrite, inaccessiblemem: readwrite)
declare void @free(ptr allocptr nocapture noundef) local_unnamed_addr #7

; Function Attrs: nounwind ssp uwtable(sync)
define [2 x i64] @copy_array(ptr nocapture noundef readonly %0) local_unnamed_addr #0 {
  %2 = load ptr, ptr %0, align 8, !tbaa !13
  %3 = icmp eq ptr %2, null
  br i1 %3, label %4, label %6

4:                                                ; preds = %1
  %5 = tail call i32 @puts(ptr nonnull dereferenceable(1) @str)
  tail call void @exit(i32 noundef 1) #11
  unreachable

6:                                                ; preds = %1
  %7 = getelementptr inbounds %struct.KoanArray, ptr %0, i64 0, i32 1
  %8 = load i8, ptr %7, align 8, !tbaa !23, !range !24, !noundef !25
  %9 = getelementptr inbounds %struct.KArrBox, ptr %2, i64 0, i32 1
  %10 = load i32, ptr %9, align 8, !tbaa !22
  %11 = add i32 %10, 1
  store i32 %11, ptr %9, align 8, !tbaa !22
  %12 = ptrtoint ptr %2 to i64
  %13 = insertvalue [2 x i64] poison, i64 %12, 0
  %14 = zext nneg i8 %8 to i64
  %15 = insertvalue [2 x i64] %13, i64 %14, 1
  ret [2 x i64] %15
}

; Function Attrs: nounwind ssp uwtable(sync)
define void @print_array(ptr nocapture noundef readonly %0) local_unnamed_addr #0 {
  %2 = load ptr, ptr %0, align 8, !tbaa !13
  %3 = icmp eq ptr %2, null
  br i1 %3, label %4, label %6

4:                                                ; preds = %1
  %5 = tail call i32 @puts(ptr nonnull dereferenceable(1) @str)
  tail call void @exit(i32 noundef 1) #11
  unreachable

6:                                                ; preds = %1
  %7 = tail call i32 @puts(ptr nonnull dereferenceable(1) @str.14)
  %8 = load ptr, ptr %2, align 8, !tbaa !19
  %9 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull dereferenceable(1) @.str.5, ptr noundef %8)
  %10 = getelementptr inbounds %struct.KArrBox, ptr %2, i64 0, i32 1
  %11 = load i32, ptr %10, align 8, !tbaa !22
  %12 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull dereferenceable(1) @.str.6, i32 noundef %11)
  %13 = getelementptr inbounds %struct.KArrBox, ptr %2, i64 0, i32 2
  %14 = load i32, ptr %13, align 4, !tbaa !16
  %15 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull dereferenceable(1) @.str.7, i32 noundef %14)
  %16 = getelementptr inbounds %struct.KArrBox, ptr %2, i64 0, i32 3
  %17 = load i32, ptr %16, align 8, !tbaa !18
  %18 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull dereferenceable(1) @.str.8, i32 noundef %17)
  %19 = tail call i32 @puts(ptr nonnull dereferenceable(1) @str.15)
  ret void
}

; Function Attrs: nounwind ssp uwtable(sync)
define void @print_arr_elems(ptr nocapture noundef readonly %0) local_unnamed_addr #0 {
  %2 = load ptr, ptr %0, align 8, !tbaa !13
  %3 = icmp eq ptr %2, null
  br i1 %3, label %4, label %6

4:                                                ; preds = %1
  %5 = tail call i32 @puts(ptr nonnull dereferenceable(1) @str)
  tail call void @exit(i32 noundef 1) #11
  unreachable

6:                                                ; preds = %1
  %7 = tail call i32 @putchar(i32 91)
  %8 = load ptr, ptr %0, align 8, !tbaa !13
  %9 = icmp eq ptr %8, null
  br i1 %9, label %10, label %12

10:                                               ; preds = %21, %6
  %11 = tail call i32 @puts(ptr nonnull dereferenceable(1) @str)
  tail call void @exit(i32 noundef 1) #11
  unreachable

12:                                               ; preds = %6, %21
  %13 = phi i64 [ %26, %21 ], [ 0, %6 ]
  %14 = phi ptr [ %27, %21 ], [ %8, %6 ]
  %15 = getelementptr inbounds %struct.KArrBox, ptr %14, i64 0, i32 2
  %16 = load i32, ptr %15, align 4, !tbaa !16
  %17 = zext i32 %16 to i64
  %18 = icmp ult i64 %13, %17
  br i1 %18, label %21, label %19

19:                                               ; preds = %12
  %20 = tail call i32 @puts(ptr nonnull dereferenceable(1) @str.16)
  ret void

21:                                               ; preds = %12
  %22 = load ptr, ptr %14, align 8, !tbaa !19
  %23 = getelementptr inbounds double, ptr %22, i64 %13
  %24 = load double, ptr %23, align 8, !tbaa !20
  %25 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull dereferenceable(1) @.str.11, double noundef %24)
  %26 = add nuw nsw i64 %13, 1
  %27 = load ptr, ptr %0, align 8, !tbaa !13
  %28 = icmp eq ptr %27, null
  br i1 %28, label %10, label %12, !llvm.loop !26
}

; Function Attrs: nounwind ssp uwtable(sync)
define noundef i32 @main() local_unnamed_addr #0 {
  %1 = alloca %struct.KoanArray, align 8
  %2 = alloca %struct.KoanArray, align 8
  call void @llvm.lifetime.start.p0(i64 16, ptr nonnull %1) #13
  %3 = tail call dereferenceable_or_null(16) ptr @calloc(i64 noundef 2, i64 noundef 8) #9
  %4 = tail call dereferenceable_or_null(24) ptr @malloc(i64 noundef 24) #10
  store ptr %3, ptr %4, align 8, !tbaa.struct !5
  %5 = getelementptr inbounds i8, ptr %4, i64 8
  store <2 x i32> <i32 1, i32 0>, ptr %5, align 8
  %6 = getelementptr inbounds i8, ptr %4, i64 16
  store i32 2, ptr %6, align 8, !tbaa.struct !12
  %7 = icmp eq ptr %3, null
  br i1 %7, label %8, label %10

8:                                                ; preds = %0
  %9 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull dereferenceable(1) @.str)
  tail call void @exit(i32 noundef 1) #11
  unreachable

10:                                               ; preds = %0
  %11 = ptrtoint ptr %4 to i64
  store i64 %11, ptr %1, align 8
  %12 = getelementptr inbounds [2 x i64], ptr %1, i64 0, i64 1
  store i64 0, ptr %12, align 8
  call void @push_array(ptr noundef nonnull %1, double noundef 1.000000e+00)
  call void @push_array(ptr noundef nonnull %1, double noundef 2.000000e+00)
  call void @push_array(ptr noundef nonnull %1, double noundef 3.000000e+00)
  call void @llvm.lifetime.start.p0(i64 16, ptr nonnull %2) #13
  %13 = load i32, ptr %5, align 8, !tbaa !22
  %14 = add i32 %13, 1
  store i32 %14, ptr %5, align 8, !tbaa !22
  %15 = getelementptr inbounds [2 x i64], ptr %2, i64 0, i64 1
  store i64 1, ptr %15, align 8
  call void @print_array(ptr noundef nonnull %1)
  call void @print_arr_elems(ptr noundef nonnull %1)
  %16 = load i32, ptr %5, align 8, !tbaa !22
  %17 = icmp eq i32 %16, 1
  br i1 %17, label %18, label %20

18:                                               ; preds = %10
  %19 = load ptr, ptr %4, align 8, !tbaa !19
  tail call void @free(ptr noundef %19)
  tail call void @free(ptr noundef nonnull %4)
  br label %22

20:                                               ; preds = %10
  %21 = add i32 %16, -1
  store i32 %21, ptr %5, align 8, !tbaa !22
  br label %22

22:                                               ; preds = %20, %18
  store ptr null, ptr %2, align 8, !tbaa !13
  call void @print_arr_elems(ptr noundef nonnull %2)
  %23 = load i32, ptr %5, align 8, !tbaa !22
  %24 = icmp eq i32 %23, 1
  br i1 %24, label %25, label %27

25:                                               ; preds = %22
  %26 = load ptr, ptr %4, align 8, !tbaa !19
  tail call void @free(ptr noundef %26)
  tail call void @free(ptr noundef nonnull %4)
  br label %29

27:                                               ; preds = %22
  %28 = add i32 %23, -1
  store i32 %28, ptr %5, align 8, !tbaa !22
  br label %29

29:                                               ; preds = %25, %27
  call void @llvm.lifetime.end.p0(i64 16, ptr nonnull %2) #13
  call void @llvm.lifetime.end.p0(i64 16, ptr nonnull %1) #13
  ret i32 0
}

; Function Attrs: nofree nounwind
declare noundef i32 @puts(ptr nocapture noundef readonly) local_unnamed_addr #8

; Function Attrs: nofree nounwind
declare noundef i32 @putchar(i32 noundef) local_unnamed_addr #8

attributes #0 = { nounwind ssp uwtable(sync) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+complxnum,+crc,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+jsconv,+lse,+neon,+pauth,+ras,+rcpc,+rdm,+sha2,+sha3,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }
attributes #1 = { mustprogress nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #2 = { mustprogress nofree nounwind willreturn allockind("alloc,zeroed") allocsize(0,1) memory(inaccessiblemem: readwrite) "alloc-family"="malloc" "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+complxnum,+crc,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+jsconv,+lse,+neon,+pauth,+ras,+rcpc,+rdm,+sha2,+sha3,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }
attributes #3 = { mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0) memory(inaccessiblemem: readwrite) "alloc-family"="malloc" "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+complxnum,+crc,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+jsconv,+lse,+neon,+pauth,+ras,+rcpc,+rdm,+sha2,+sha3,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }
attributes #4 = { nofree nounwind "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+complxnum,+crc,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+jsconv,+lse,+neon,+pauth,+ras,+rcpc,+rdm,+sha2,+sha3,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }
attributes #5 = { noreturn "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+complxnum,+crc,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+jsconv,+lse,+neon,+pauth,+ras,+rcpc,+rdm,+sha2,+sha3,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }
attributes #6 = { mustprogress nounwind willreturn allockind("realloc") allocsize(1) memory(argmem: readwrite, inaccessiblemem: readwrite) "alloc-family"="malloc" "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+complxnum,+crc,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+jsconv,+lse,+neon,+pauth,+ras,+rcpc,+rdm,+sha2,+sha3,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }
attributes #7 = { mustprogress nounwind willreturn allockind("free") memory(argmem: readwrite, inaccessiblemem: readwrite) "alloc-family"="malloc" "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+complxnum,+crc,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+jsconv,+lse,+neon,+pauth,+ras,+rcpc,+rdm,+sha2,+sha3,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }
attributes #8 = { nofree nounwind }
attributes #9 = { allocsize(0,1) }
attributes #10 = { allocsize(0) }
attributes #11 = { noreturn nounwind }
attributes #12 = { allocsize(1) }
attributes #13 = { nounwind }

!llvm.module.flags = !{!0, !1, !2, !3}
!llvm.ident = !{!4}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"uwtable", i32 1}
!3 = !{i32 7, !"frame-pointer", i32 1}
!4 = !{!"Homebrew clang version 18.1.8"}
!5 = !{i64 0, i64 8, !6, i64 8, i64 4, !10, i64 12, i64 4, !10, i64 16, i64 4, !10}
!6 = !{!7, !7, i64 0}
!7 = !{!"any pointer", !8, i64 0}
!8 = !{!"omnipotent char", !9, i64 0}
!9 = !{!"Simple C/C++ TBAA"}
!10 = !{!11, !11, i64 0}
!11 = !{!"int", !8, i64 0}
!12 = !{i64 0, i64 4, !10}
!13 = !{!14, !7, i64 0}
!14 = !{!"", !7, i64 0, !15, i64 8}
!15 = !{!"_Bool", !8, i64 0}
!16 = !{!17, !11, i64 12}
!17 = !{!"", !7, i64 0, !11, i64 8, !11, i64 12, !11, i64 16}
!18 = !{!17, !11, i64 16}
!19 = !{!17, !7, i64 0}
!20 = !{!21, !21, i64 0}
!21 = !{!"double", !8, i64 0}
!22 = !{!17, !11, i64 8}
!23 = !{!14, !15, i64 8}
!24 = !{i8 0, i8 2}
!25 = !{}
!26 = distinct !{!26, !27}
!27 = !{!"llvm.loop.mustprogress"}
