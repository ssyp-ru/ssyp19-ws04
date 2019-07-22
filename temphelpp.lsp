;;
;; "HomeLisp Вер=1.13.53 Date=27.11.2016 Time=20:46:42 (Файфель Б.Л.)"
;;
;; Функции
;;
;;
(SEXPR fact ( LAMBDA (X) (COND ((MINUSP X) (RAISEERROR "Аргумент FACT отрицателен")
) ((EQ X 0) 1) (T (TIMES X (FACT (DIFFERENCE X 1)))))) )
;;
(SEXPR copy ( LAMBDA (X) (COND ((ATOM X) X) (T (CONS (COPY (CAR X)) (COPY 
(CDR X)))))) )
;;
(SEXPR append_ ( LAMBDA (&OPTIONAL (X NIL) (Y NIL)) (COND ((NULL X) Y) 
(T (CONS (CAR X) (APPEND_ (CDR X) Y))))) )
;;
(SEXPR equal ( LAMBDA (X Y) (COND ((ATOM X) (EQ X Y)) ((ATOM Y) NIL) ((EQUAL 
(CAR X) (CAR Y)) (EQUAL (CDR X) (CDR Y))) (T NIL))) )
;;
(SEXPR member ( LAMBDA (X Y) (COND ((NULL Y) NIL) ((EQUAL X (CAR Y)) T)
 (T (MEMBER X (CDR Y))))) )
;;
(SEXPR memb ( LAMBDA (X Y) (PROG NIL L (COND ((NULL Y) (RETURN NIL)) ((EQ 
X (CAR Y)) (RETURN T))) (SETQ Y (CDR Y)) (GO L))) )
;;
(SEXPR rev1 ( LAMBDA (U V) (COND ((NULL U) V) (T (REV1 (CDR U) (CONS (REV 
(CAR U)) V))))) )
;;
(SEXPR rev ( LAMBDA (X) (COND ((ATOM X) X) (T (REV1 X NIL)))) )
;;
(SEXPR remove ( LAMBDA (X L) (COND ((NULL L) NIL) ((EQUAL X (CAR L)) (REMOVE 
X (CDR L))) (T (CONS (CAR L) (REMOVE X (CDR L)))))) )
;;
(SEXPR removef ( LAMBDA (X Y) (COND ((NULL Y) NIL) ((EQUAL X (CAR Y)) (CDR 
Y)) (T (CONS (CAR Y) (REMOVEF X (CDR Y)))))) )
;;
(SEXPR last ( LAMBDA (X) (COND ((NULL X) NIL) ((NULL (CDR X)) X) (T (LAST 
(CDR X))))) )
;;
(SEXPR length ( LAMBDA (X) (COND ((ATOM X) 0) (T (PLUS 1 (LENGTH (CDR X)
))))) )
;;
(SEXPR addifnone ( LAMBDA (X L) (COND ((MEMBER X L) L) (T (CONS X L))))
 )
;;
(SEXPR collect ( LAMBDA (L) (COND ((NULL L) NIL) (T (CONS (CAR L) (COLLECT 
(COND ((MEMBER (CAR L) (CDR L)) (CONS (CAR L) (REMOVEF (CAR L) (CDR L)
))) (T (CDR L)))))))) )
;;
(SEXPR reverse ( LAMBDA (X) (PROG (U) A (COND ((NULL X) (RETURN U))) (SETQ 
U (CONS (CAR X) U)) (SETQ X (CDR X)) (GO A))) )
;;
(SEXPR flatten ( LAMBDA (X) (COND ((NULL X) NIL) ((ATOM X) (LIST X)) (T 
(APPEND (FLATTEN (CAR X)) (FLATTEN (CDR X)))))) )
;;
(SEXPR attach ( LAMBDA (X Y) (RPLACA (RPLACD Y (CONS (CAR Y) (CDR Y))) 
X)) )
;;
(SEXPR dreverse ( LAMBDA (X) (PROG (U V) A (COND ((NULL X) (RETURN U)))
 (SETQ V X) (SETQ X (CDR X)) (SETQ U (RPLACD V U)) (GO A))) )
;;
(SEXPR nconc ( LAMBDA (&REST LSTS) (COND ((NULL (CDR LSTS)) (CAR LSTS))
 (T (NCONC1 (CAR LSTS) (APPLY (QUOTE NCONC) (CDR LSTS)))))) )
;;
(SEXPR nconc1 ( LAMBDA (X Y) (COND ((NULL X) Y) (T (RPLACD X (NCONC1 (CDR 
X) Y))))) )
;;
(SEXPR tconc ( LAMBDA (X Q) (COND ((NULL Q) (CONS (SETQ Q (CONS X NIL))
 Q)) (T (RPLACD Q (CDR (RPLACD (CDR Q) (CONS X NIL))))))) )
;;
(SEXPR efface ( LAMBDA (X Y) (COND ((NULL Y) NIL) ((EQUAL X (CAR Y)) (CDR 
Y)) (T (RPLACD Y (EFFACE X (CDR Y)))))) )
;;
(SEXPR dremove ( LAMBDA (X Y) (COND ((NULL Y) NIL) ((EQUAL X (CAR Y)) (DREMOVE 
X (CDR Y))) (T (RPLACD Y (DREMOVE X (CDR Y)))))) )
;;
(SEXPR lcyclep ( LAMBDA (X) (AND (NOT (ATOM X)) (NOT (ATOM (CDR X))) (LCYCLE1 
(CDR X) (CDDR X)))) )
;;
(SEXPR lcycle1 ( LAMBDA (X Y) (OR (EQ X Y) (AND (NOT (ATOM Y)) (NOT (ATOM 
(CDR Y))) (LCYCLE1 (CDR X) (CDDR Y))))) )
;;
(SEXPR cyclep ( LAMBDA (X) (NOT (CYCLE1 X NIL (QUOTE (T))))) )
;;
(SEXPR cycle1 ( LAMBDA (X U V) (COND ((ATOM X) V) ((MEMB X U) NIL) ((MEMB 
X V) V) ((NULL (SETQ V (CYCLE1 (CAR X) (SETQ U (CONS X U)) V))) NIL) ((NULL 
(SETQ V (CYCLE1 (CDR X) U V))) NIL) (T (CONS X V)))) )
;;
(SEXPR forall ( LAMBDA (L P) (COND ((NULL L) T) ((FUNCALL P (CAR L)) (FORALL 
(CDR L) P)) (T NIL))) )
;;
(SEXPR forsome ( LAMBDA (L P) (COND ((NULL L) NIL) ((FUNCALL P (CAR L))
 T) (T (FORSOME (CDR L) P)))) )
;;
(SEXPR forodd ( LAMBDA (L P) (COND ((NULL L) NIL) ((FUNCALL P (CAR L)) 
(NOT (FORODD (CDR L) P))) (T (FORODD (CDR L) P)))) )
;;
(SEXPR atomlist ( LAMBDA (X) (COND ((NULL X) T) ((ATOM X) NIL) ((ATOM (CAR 
X)) (ATOMLIST (CDR X))) (T NIL))) )
;;
(SEXPR listp ( LAMBDA (X) (COND ((NULL X) T) ((ATOM X) NIL) ((OR (ATOM 
(CAR X)) (LISTP (CAR X))) (LISTP (CDR X))) (T NIL))) )
;;
(SEXPR order ( LAMBDA (X Y L) (COND ((NULL L) NIL) ((EQUAL X (CAR L)) T)
 ((EQUAL Y (CAR L)) NIL) (T (ORDER X Y (CDR L))))) )
;;
(SEXPR order1 ( LAMBDA (X Y L) (COND ((NULL L) (QUOTE ORDERUNDEF)) ((EQUAL 
X (CAR L)) T) ((EQUAL Y (CAR L)) NIL) (T (ORDER1 X Y (CDR L))))) )
;;
(SEXPR lexorder ( LAMBDA (X Y L) (COND ((NULL X) T) ((NULL Y) NIL) ((EQUAL 
(CAR X) (CAR Y)) (LEXORDER (CDR X) (CDR Y) L)) (T (ORDER1 (CAR X) (CAR 
Y) L)))) )
;;
(SEXPR lexorder1 ( LAMBDA (X Y L) (COND ((NULL X) T) ((NULL Y) NIL) ((NULL 
L) (QUOTE LEXORDERUNDEF)) ((EQUAL (CAR X) (CAR Y)) (LEXORDER1 (CDR X) 
(CDR Y) (CDR L))) (T (ORDER1 (CAR X) (CAR Y) (CAR L))))) )
;;
(SEXPR first ( LAMBDA (X Y) (COND ((NULL Y) (CAR X)) ((MEMBER (CAR Y) X)
 (CAR Y)) (T (FIRST X (CDR Y))))) )
;;
(SEXPR rank ( LAMBDA (X Y) (COND ((NULL X) NIL) (T (CONS (FIRST X Y) (RANK 
(REMOVEF (FIRST X Y) X) Y))))) )
;;
(SEXPR possessing ( LAMBDA (P L) (PROG (U) A (COND ((NULL L) (RETURN (REVERSE 
U))) ((FUNCALL P (CAR L)) (SETQ U (CONS (CAR L) U)))) (SETQ L (CDR L))
 (GO A))) )
;;
(SEXPR suchthat ( LAMBDA (P L) (COND ((NULL L) NIL) ((FUNCALL P (CAR L)
) (CAR L)) (T (SUCHTHAT P (CDR L))))) )
;;
(SEXPR suchthat1 ( LAMBDA (P L X Y) (COND ((NULL L) X) ((FUNCALL P (CAR 
L)) Y) (T (SUCHTHAT1 P (CDR L) X Y)))) )
;;
(SEXPR suchthat2 ( LAMBDA (P L F) (COND ((NULL L) NIL) ((FUNCALL P (CAR 
L)) (FUNCALL F L)) (T (SUCHTHAT2 P (CDR L) F)))) )
;;
(SEXPR setof ( LAMBDA (X) (COND ((NULL X) NIL) ((MEMBER (CAR X) (CDR X)
) (SETOF (CDR X))) (T (CONS (CAR X) (SETOF (CDR X)))))) )
;;
(SEXPR makeset ( LAMBDA (X) (PROG (Y) A (COND ((NULL X) (RETURN Y)) ((NULL 
(MEMBER (CAR X) Y)) (SETQ Y (CONS (CAR X) Y)))) (SETQ X (CDR X)) (GO A)
)) )
;;
(SEXPR diflist ( LAMBDA (X Y) (COND ((NULL Y) X) (T (DIFLIST (REMOVE (CAR 
Y) X) (CDR Y))))) )
;;
(SEXPR subset ( LAMBDA (X Y) (NULL (DIFLIST X Y))) )
;;
(SEXPR union0 ( LAMBDA (X Y) (COND ((NULL X) Y) ((MEMBER (CAR X) Y) (UNION 
(CDR X) Y)) (T (CONS (CAR X) (UNION (CDR X) Y))))) )
;;
(SEXPR union ( LAMBDA (X Y) (PROG NIL (SETQ Y (MAKESET Y)) A (COND ((NULL 
X) (RETURN Y)) ((NOT (MEMBER (CAR X) Y)) (SETQ Y (CONS (CAR X) Y)))) (SETQ 
X (CDR X)) (GO A))) )
;;
(SEXPR lunion ( LAMBDA (X) (COND ((NULL X) NIL) (T (UNION (CAR X) (LUNION 
(CDR X)))))) )
;;
(SEXPR intersection ( LAMBDA (X Y) (COND ((NULL X) NIL) ((MEMBER (CAR X)
 Y) (CONS (CAR X) (INTERSECTION (CDR X) Y))) (T (INTERSECTION (CDR X) 
Y)))) )
;;
(SEXPR equalset ( LAMBDA (X Y) (AND (SUBSET X Y) (SUBSET Y X))) )
;;
(SEXPR cart ( LAMBDA (X Y) (PROG (U V W) A (COND ((NULL X) (RETURN (REVERSE 
W)))) (SETQ U (CAR X)) (SETQ X (CDR X)) (SETQ V Y) B (COND ((NULL V) (GO 
A))) (SETQ W (CONS (LIST U (CAR V)) W)) (SETQ V (CDR V)) (GO B))) )
;;
(SEXPR dpair ( LAMBDA (X Y) (PROG (U) (SETQ U X) A (COND ((NULL U) (RETURN 
X))) (RPLACA U (CONS (CAR U) (CAR Y))) (SETQ U (CDR U)) (SETQ Y (CDR Y)
) (GO A))) )
;;
(SEXPR assoc ( LAMBDA (X A) (COND ((NULL A) NIL) ((EQUAL X (CAAR A)) (CAR 
A)) (T (ASSOC X (CDR A))))) )
;;
(SEXPR pairlis ( LAMBDA (X Y A) (COND ((NULL X) A) (T (CONS (CONS (CAR 
X) (CAR Y)) (PAIRLIS (CDR X) (CDR Y) A))))) )
;;
(SEXPR subst ( LAMBDA (X Y Z) (COND ((EQUAL Y Z) X) ((ATOM Z) Z) (T (CONS 
(SUBST X Y (CAR Z)) (SUBST X Y (CDR Z)))))) )
;;
(SEXPR sublis ( LAMBDA (A Y) (COND ((NULL Y) NIL) ((ATOM Y) (SUB2 A Y))
 (T (CONS (SUBLIS A (CAR Y)) (SUBLIS A (CDR Y)))))) )
;;
(SEXPR sub2 ( LAMBDA (A Y) (COND ((NULL A) Y) ((EQ Y (CAAR A)) (CDAR A)
) (T (SUB2 (CDR A) Y)))) )
;;
(SEXPR sassoc ( LAMBDA (X A F) (COND ((NULL A) (FUNCALL F)) ((EQUAL X (CAAR 
A)) (CAR A)) (T (SASSOC X (CDR A) F)))) )
;;
(SEXPR maplist_ ( LAMBDA (X F) (COND ((NULL X) NIL) (T (CONS (FUNCALL F 
X) (MAPLIST_ (CDR X) F))))) )
;;
(SEXPR maplisto ( LAMBDA (F X) (COND ((NULL X) NIL) (T (CONS (FUNCALL F 
X) (MAPLISTO F (CDR X)))))) )
;;
(SEXPR maplist ( LAMBDA (F &REST X) (COND ((NULL X) NIL) ((MEMBER NIL X)
 NIL) (T (CONS (APPLY F X) (APPLY (QUOTE MAPLIST) (CONS F (ACDR X)))))
)) )
;;
(SEXPR aCDR ( LAMBDA (X) (COND ((NULL X) NIL) (T (CONS (CDAR X) (ACDR (CDR 
X)))))) )
;;
(SEXPR mapcar_ ( LAMBDA (X F) (MAPLIST_ X (FUNCTION (LAMBDA (X) (FUNCALL 
F (CAR X)))))) )
;;
(SEXPR mapcaro ( LAMBDA (F X) (MAPLIST (FUNCTION (LAMBDA (X) (FUNCALL F 
(CAR X)))) X)) )
;;
(SEXPR mapcar ( LAMBDA (F &REST X) (COND ((NULL X) NIL) ((MEMBER NIL X)
 NIL) (T (CONS (APPLY F (ACAR X)) (APPLY (QUOTE MAPCAR) (CONS F (ACDR 
X))))))) )
;;
(SEXPR aCAR ( LAMBDA (X) (COND ((NULL X) NIL) (T (CONS (CAAR X) (ACAR (CDR 
X)))))) )
;;
(SEXPR map ( LAMBDA (X F) (PROG NIL A (COND ((ATOM X) (RETURN X))) (FUNCALL 
F X) (SETQ X (CDR X)) (GO A))) )
;;
(SMACRO push ( LAMBDA (A P) (BACKQUOTE (SETQ , P (CONS , A , P)))) )
;;
(SMACRO pop ( LAMBDA (P) (BACKQUOTE (PROG1 (CAR , P) (SETQ , P (CDR , P)
)))) )
;;
(SMACRO clear ( LAMBDA (P) (BACKQUOTE (SETQ , P NIL))) )
;;
(SMACRO popup ( LAMBDA (A P) (LIST (QUOTE PROG) NIL (LIST (QUOTE SETQ) 
A (LIST (QUOTE CAR) P)) (LIST (QUOTE SETQ) P (LIST (QUOTE CDR) P)))) )

;;
(SMACRO define ( LAMBDA (X) (COND ((NULL X) NIL) (T (LIST (QUOTE CONS) 
(LIST (QUOTE SEXPR) (CAAR X) (CADAR X)) (LIST (QUOTE DEFINE) (CDR X)))
))) )
;;
(SMACRO deflist ( LAMBDA (X I) (COND ((NULL X) NIL) (T (LIST (QUOTE CONS)
 (LIST (QUOTE QUOTE) (PUTPROP (CAAR X) I (CADAR X))) (LIST (QUOTE DEFLIST)
 (CDR X) I))))) )
;;
(SEXPR putprop ( LAMBDA (A I P) (PROG (U V) (SETQ U (CONS NIL (COPY (PROPLIST 
A)))) (SETQ V U) A (COND ((NULL (CDR V)) (RPLACD V (LIST I P))) ((EQ (CAR 
(SETQ V (CDR V))) I) (COND ((NULL (CDR V)) (RPLACD V (LIST P))) (T (RPLACA 
(CDR V) P)))) (T (GO A))) (SPROPL A (CDR U)) (RETURN A))) )
;;
(SEXPR select ( LAMBDA (E L E0) (COND ((NULL L) E0) ((EQ E (CAR L)) (CADR 
L)) (T (SELECT E (CDDR L) E0)))) )
;;
(SEXPR getprop ( LAMBDA (A I) (PROG (U) (SETQ U (PROPLIST A)) A (COND ((NULL 
U) (RETURN NIL)) ((EQ (CAR U) I) (GO B))) (SETQ U (CDR U)) (GO A) B (RETURN 
(COND ((EQ I EXPR) EXPR) ((EQ I FEXPR) FEXPR) ((EQ I APVAL) APVAL) ((EQ 
I FIXED) FIXED) ((EQ I BITS) BITS) ((EQ I STRING) STRING) ((EQ I FLOAT)
 FLOAT) ((EQ I SUBR) SUBR) ((EQ I FSUBR) FSUBR) ((NULL (CDR U)) NIL) (T 
(CADR U)))))) )
;;
(SEXPR prop ( LAMBDA (A I F) (PROG (U) (SETQ U (PROPLIST A)) A (COND ((NULL 
U) (RETURN (F))) ((EQ (CAR U) I) (RETURN (CDR U)))) (SETQ U (CDR U)) (GO 
A))) )
;;
(SEXPR remprop ( LAMBDA (A I) (PROG (U V) (COND ((EQ I EXPR) (GO C)) ((EQ 
I FEXPR) (GO C)) ((EQ I APVAL) (GO C)) ((EQ I FIXED) (GO C)) ((EQ I BITS)
 (GO C)) ((EQ I STRING) (GO C)) ((EQ I FLOAT) (GO C))) (SETQ U (CONS NIL 
(PROPLIST A))) (SETQ V U) A (COND ((NULL (CDR V)) (RETURN A)) ((EQ (CADR 
V) I) (GO B))) (SETQ V (CDR V)) (GO A) B (COND ((NULL (CDDR V)) (RPLACD 
V NIL)) (T (RPLACD V (CDDDR V)))) (SPROPL A (CDR U)) (RETURN A) C (TERPRI)
 (PRINT "remprop: Стандартные индикаторы не удаляются") (TERPRI) (RETURN 
A))) )
;;
(SEXPR putflag ( LAMBDA (A I) (SPROPL A (CONS I (PROPLIST A)))) )
;;
(SEXPR flagp ( LAMBDA (A I) (MEMBER I (PROPLIST A))) )
;;
(SEXPR flag ( LAMBDA (L I) (PROG (U) A (COND ((NULL L) (RETURN NIL))) (SETQ 
U (PROPLIST (CAR L))) (COND ((NOT (MEMBER I U)) (SPROPL (CAR L) (CONS 
I U)))) (SETQ L (CDR L)) (GO A))) )
;;
(SEXPR remflag ( LAMBDA (L I) (PROG (U V) A (COND ((NULL L) (RETURN NIL)
)) (SETQ U (CONS NIL (PROPLIST (CAR L)))) (SETQ V U) B (COND ((NULL (CDR 
V)) (GO D)) ((EQ (CADR V) I) (GO C))) (SETQ V (CDR V)) (GO B) C (RPLACD 
V (CDDR V)) (GO B) D (SPROPL (CAR L) (CDR U)) (SETQ L (CDR L)) (GO A))
) )
;;
(SEXPR alert ( LAMBDA NIL (PROG NIL (TERPRI) (PRINT "Индикатор не найден!")
 (TERPRI))) )
;;
(SMACRO for ( LAMBDA (I IBEG IEND BODY) (BACKQUOTE (PROG (, I) (SETQ , 
I , IBEG) $LOOP ,@ BODY (SETQ , I (ADD1 , I)) (COND ((<= , I , IEND) (GO 
$LOOP))) (RETURN , IEND)))) )
;;
(SEXPR prlists ( LAMBDA (X) (PROG (TAIL) (SETQ TAIL X) @L (COND ((NULL 
TAIL) (RETURN T))) (PRINTS (CAR TAIL)) (TERPRI) (SETQ TAIL (CDR TAIL))
 (GO @L))) )
;;
(SEXPR FLOOR ( LAMBDA (X) (FIX X)) )
;;
(SEXPR CEIL ( LAMBDA (X) (PLUS 1 (FIX X))) )
;;
(SEXPR fdir ( LAMBDA (ADIR) (PROG (TAIL FF FN) (SETQ TAIL (SYSDIR (STRCAT 
ADIR "\*.*") &H1F)) @L (COND ((NULL TAIL) (RETURN T))) (SETQ FF (CAR TAIL)
) (SETQ TAIL (CDR TAIL)) (SETQ FN (STRCAT (STRCAT ADIR (STRCHR 92)) FF)
) (PRINTS (CONS FN (SYSGETATTR FN))) (TERPRI) (GO @L))) )
;;
(SEXPR setmarg ( LAMBDA (M) (PROG NIL (COND ((GREATERP M 1) (PRINTS " ")
) (T (RETURN NIL))) (SETMARG (DIFFERENCE M 1)))) )
;;
(SEXPR pprint ( LAMBDA (L MARG) (COND ((ATOM L) (PROG NIL (SETMARG MARG)
 (PRINT L))) ((LESSP (LENGTH L) 3) (PROG NIL (SETMARG MARG) (PRINT L))
) (T (PROG NIL (TERPRI) (SETMARG MARG) (PRINTS "(") (PRINTS (CAR L)) (PRINT-TAIL 
(CDR L) MARG))))) )
;;
(SEXPR print-tail ( LAMBDA (L MARG) (COND ((NULL L) (PRINTS ")")) (T (PROG 
NIL (TERPRI) (SETMARG MARG) (PPRINT (CAR L) (PLUS MARG 3)) (PRINT-TAIL 
(CDR L) MARG))))) )
;;
(SEXPR lastItem ( LAMBDA (X) (COND ((NULL (CDR X)) (CAR X)) (T (LASTITEM 
(CDR X))))) )
;;
(SEXPR get_even ( LAMBDA (X) (COND ((NULL X) NIL) (T (APPEND (LIST (CADR 
X)) (GET_EVEN (CDDR X)))))) )
;;
(SEXPR get_odd ( LAMBDA (X) (COND ((NULL X) NIL) (T (APPEND (LIST (CAR 
X)) (GET_ODD (CDDR X)))))) )
;;
(SEXPR add_uniq ( LAMBDA (ITEM LST) (COND ((NULL LST) (LIST ITEM)) ((EQ 
ITEM (CAR LST)) LST) (T (APPEND (LIST (CAR LST)) (ADD_UNIQ ITEM (CDR LST)
))))) )
;;
(SEXPR amax ( LAMBDA (X) (COND ((NULL X) NIL) ((NULL (CDR X)) (CAR X)) 
(T (COND ((GREQP (CAR X) (AMAX (CDR X))) (CAR X)) (T (AMAX (CDR X)))))
)) )
;;
(SEXPR amin ( LAMBDA (X) (COND ((NULL X) NIL) ((NULL (CDR X)) (CAR X)) 
(T (COND ((LEEQP (CAR X) (AMIN (CDR X))) (CAR X)) (T (AMIN (CDR X)))))
)) )
;;
(SEXPR prlist ( LAMBDA (X) (COND ((NULL X) NIL) (T (PROG NIL (PRINTSLINE 
(CAR X)) (PRLIST (CDR X)))))) )
;;
(SEXPR weight ( LAMBDA (X) (COND ((EQ X (QUOTE +)) 1) ((EQ X (QUOTE -))
 1) ((EQ X (QUOTE *)) 2) ((EQ X (QUOTE \)) 2) ((EQ X (QUOTE /)) 2) ((EQ 
X (QUOTE ^)) 3) (T 5))) )
;;
(SEXPR opcode ( LAMBDA (OP) (COND ((EQ OP (QUOTE +)) (QUOTE +)) ((EQ OP 
(QUOTE -)) (QUOTE -)) ((EQ OP (QUOTE *)) (QUOTE *)) ((EQ OP (QUOTE \))
 (QUOTE \)) ((EQ OP (QUOTE /)) (QUOTE /)) ((EQ OP (QUOTE ^)) (QUOTE ^)
) (T (RAISEERROR (STRCAT "Неверен код операции " (OUTPUT OP)))))) )
;;
(SEXPR inf-aux ( LAMBDA (AE OPERATORS OPERANDS) (INF-ITER (CDR AE) OPERATORS 
(CONS (CAR AE) OPERANDS))) )
;;
(SEXPR inf-iter ( LAMBDA (AE OPERATORS OPERANDS) (PROG NIL (COND ((AND 
(NULL AE) (NULL OPERATORS)) (RETURN (CAR OPERANDS)))) (COND ((AND (NOT 
(NULL AE)) (OR (NULL OPERATORS) (GREATERP (WEIGHT (CAR AE)) (WEIGHT (CAR 
OPERATORS))))) (RETURN (INF-AUX (CDR AE) (CONS (CAR AE) OPERATORS) OPERANDS)
))) (RETURN (INF-ITER AE (CDR OPERATORS) (CONS (LIST (OPCODE (CAR OPERATORS)
) (CADR OPERANDS) (CAR OPERANDS)) (CDDR OPERANDS)))))) )
;;
(SEXPR inf2pref ( LAMBDA (X) (PROG (HD TL CC XX RR) (COND ((ATOMLIST X)
 (RETURN (INF-AUX X NIL NIL)))) (SETQ RR NIL) (SETQ XX X) LOOP (SETQ HD 
(CAR XX)) (SETQ TL (CDR XX)) (COND ((MEMB HD (QUOTE (SIN COS LOG EXP ATN 
ASN ACS SH CH SQR SIGN ABS))) (PROGN (SETQ RR (APPEND RR (LIST (LIST HD 
(INF2PREF (CAR TL)))))) (SETQ TL (CDR TL)))) ((ATOM HD) (SETQ RR (APPEND 
RR (LIST HD)))) (T (SETQ RR (APPEND RR (LIST (INF2PREF HD)))))) (COND 
((NULL TL) (RETURN (INF-AUX RR NIL NIL)))) (SETQ XX TL) (GO LOOP))) )
;;
(SEXPR reduce ( LAMBDA (FUNCT LIST &KEY INITIAL-VALUE FROM-END &AUX LEN)
 (IF FROM-END (IF INITIAL-VALUE (SETQ LIST (APPEND LIST (LIST INITIAL-VALUE)
))) (IF INITIAL-VALUE (SETQ LIST (CONS INITIAL-VALUE LIST)) NIL)) (SETQ 
LEN (LENGTH LIST)) (IF FROM-END (COND ((AND (NULL LIST) (NULL INITIAL-VALUE)
) (FUNCALL FUNCT)) ((AND (NULL LIST) (NOT (NULL INITIAL-VALUE))) INITIAL-VALUE)
 ((AND (= 1 LEN) (NULL INITIAL-VALUE)) (CAR LIST)) ((AND (= 1 LEN) (NOT 
(NULL INITIAL-VALUE))) INITIAL-VALUE) ((= 2 LEN) (FUNCALL FUNCT (CAR LIST)
 (CADR LIST))) (T (REDUCE FUNCT (APPEND (GETPART LIST 1 (- LEN 2)) (LIST 
(FUNCALL FUNCT (GETEL LIST (SUB1 LEN)) (GETEL LIST LEN)))) :FROM-END T)
)) (COND ((AND (NULL LIST) (NULL INITIAL-VALUE)) (FUNCALL FUNCT)) ((AND 
(NULL LIST) (NOT (NULL INITIAL-VALUE))) INITIAL-VALUE) ((AND (= 1 LEN)
 (NULL INITIAL-VALUE)) (CAR LIST)) ((AND (= 1 LEN) (NOT (NULL INITIAL-VALUE)
)) INITIAL-VALUE) ((= 2 LEN) (FUNCALL FUNCT (CAR LIST) (CADR LIST))) (T 
(REDUCE FUNCT (CONS (FUNCALL FUNCT (CAR LIST) (CADR LIST)) (CDDR LIST)
)))))) )
;;
(SMACRO setf ( LAMBDA (PLACE VAL) (COND ((ATOM PLACE) (LIST (QUOTE SETQ)
 PLACE VAL)) ((EQ (QUOTE CAR) (CAR PLACE)) (LIST (QUOTE PROG) NIL (LIST 
(QUOTE RPLACA) (CAR (CDR PLACE)) VAL) (LIST (QUOTE RETURN) VAL))) ((EQ 
(QUOTE CDR) (CAR PLACE)) (LIST (QUOTE PROG) NIL (LIST (QUOTE RPLACD) (CAR 
(CDR PLACE)) VAL) (LIST (QUOTE RETURN) VAL))) ((EQ (QUOTE GETPROP) (CAR 
PLACE)) (LIST (QUOTE PROG) NIL (LIST (QUOTE PUTPROP) (CADR PLACE) (CADDR 
PLACE) VAL) (LIST (QUOTE RETURN) VAL))) ((EQ (QUOTE HASHGET) (CAR PLACE)
) (LIST (QUOTE PROG) NIL (LIST (QUOTE HASHPUT) (CADR PLACE) (CADDR PLACE)
 VAL) (LIST (QUOTE RETURN) VAL))) ((EQ (QUOTE SUBSEQ) (CAR PLACE)) (LIST 
(QUOTE INJ-SEQ) (CADR PLACE) (CADDR PLACE) VAL)) ((= 2 (LENGTH PLACE))
 (LET* ((S-EXPR (CADDR (GETD (CAR PLACE)))) (S-V (CADR PLACE)) (N-M (APPEND 
(SUBSEQ S-EXPR 0 2) (LIST (CADR PLACE))))) (BACKQUOTE (SETF , N-M , VAL)
))) ((EQ (QUOTE NTH) (CAR PLACE)) (BACKQUOTE (LET ((N , (CADR PLACE)) 
(LST , (CADDR PLACE))) (COND ((OR (MINUSP N) (<= (LENGTH LST) N)) (RAISEERROR 
(STRCAT "Индекс " (FIX2STR N) " выходит за пределы списка " (OUTPUT LST)
))) (T (PROGN (DOTIMES (I N T) (SETQ LST (CDR LST))) (RPLACA LST , VAL)
 , VAL)))))) (T (LIST (QUOTE RAISEERROR) "Неверен первый параметр SETF")
))) )
;;
(SEXPR subseq ( LAMBDA (SEQ BEGIN &OPTIONAL (END (LENGTH SEQ))) (COND ((OR 
(NULL SEQ) (= BEGIN END)) NIL) (T (IF (OR (> END (LENGTH SEQ)) (> BEGIN 
END) (MINUSP BEGIN) (MINUSP END)) (RAISEERROR "subseq: индекс за пределами списка")
 (LET ((L (LENGTH SEQ)) (LST (IF (ZEROP BEGIN) SEQ (DOTIMES (I BEGIN SEQ)
 (SETQ SEQ (CDR SEQ))))) (ACC (QUOTE NIL))) (IF (= END L) LST (DOTIMES 
(I (- END BEGIN) (REVERSE ACC)) (PUSH (CAR LST) ACC) (SETQ LST (CDR LST)
)))))))) )
;;
(SEXPR inj-seq ( LAMBDA (LST FROM WHAT) (WHEN (> FROM 0) (DOTIMES (I FROM 
T) (SETQ LST (CDR LST)))) (LET ((N (MIN (LENGTH LST) (LENGTH WHAT))) (SV 
WHAT)) (DOTIMES (I N SV) (RPLACA LST (CAR WHAT)) (SETQ LST (CDR LST)) 
(SETQ WHAT (CDR WHAT))))) )
;;
(SEXPR nth ( LAMBDA (N LST) (COND ((= N 0) (CAR LST)) ((NULL LST) NIL) 
(T (NTH (- N 1) (CDR LST))))) )
;;
(SEXPR cdr-n ( LAMBDA (X N) (DOTIMES (I N X) (SETQ X (CDR X)))) )
;;
(SEXPR slice ( LAMBDA (X B L) (LET ((RES NIL) (C X)) (DOTIMES (I (SUB1 
B) T) (SETQ C (CDR C))) (COND ((NULL C) NIL) (T (DOTIMES (I L RES) (IF 
(NULL C) (RETURN RES) T) (SETQ RES (APPEND RES (LIST (CAR C)))) (SETQ 
C (CDR C))))))) )
;;
(SEXPR meps ( LAMBDA NIL (LET ((E 1.0)) (LOOP (SETQ E (* 0.5 E)) (WHEN 
(NOT (> (+ E 1) 1.0)) (RETURN E))))) )
;;
(SEXPR remove-if ( LAMBDA (P LST &KEY (COUNT NIL) (FROM-END NIL)) (COND 
(FROM-END (REVERSE (REMOVE-IF P (REVERSE LST) :COUNT COUNT :FROM-END NIL)
)) (T (COND ((NULL COUNT) (COND ((NULL LST) NIL) ((FUNCALL P (CAR LST)
) (REMOVE-IF P (CDR LST))) (T (CONS (CAR LST) (REMOVE-IF P (CDR LST)))
))) (T (COND ((NULL LST) NIL) ((= COUNT 0) LST) ((FUNCALL P (CAR LST))
 (REMOVE-IF P (CDR LST) :COUNT (SUB1 COUNT))) (T (CONS (CAR LST) (REMOVE-IF 
P (CDR LST) :COUNT COUNT))))))))) )
;;
(SEXPR count ( LAMBDA (X LST) (COND ((NULL LST) 0) ((EQUAL X (CAR LST))
 (+ 1 (COUNT X (CDR LST)))) (T (COUNT X (CDR LST))))) )
;;
(SEXPR remove-if-not ( LAMBDA (P LST &KEY (COUNT NIL) (FROM-END NIL)) (COND 
(FROM-END (REVERSE (REMOVE-IF-NOT P (REVERSE LST) :COUNT COUNT :FROM-END 
NIL))) (T (COND ((NULL COUNT) (COND ((NULL LST) NIL) ((NOT (FUNCALL P 
(CAR LST))) (REMOVE-IF-NOT P (CDR LST))) (T (CONS (CAR LST) (REMOVE-IF-NOT 
P (CDR LST)))))) (T (COND ((NULL LST) NIL) ((= COUNT 0) LST) ((NOT (FUNCALL 
P (CAR LST))) (REMOVE-IF-NOT P (CDR LST) :COUNT (SUB1 COUNT))) (T (CONS 
(CAR LST) (REMOVE-IF-NOT P (CDR LST) :COUNT COUNT))))))))) )
;;
(SEXPR evenp ( LAMBDA (X) (= 0 (% X 2))) )
;;
(SEXPR oddp ( LAMBDA (X) (<> 0 (% X 2))) )
;;
(SMACRO 1- ( LAMBDA (X) (BACKQUOTE (SUB1 , X))) )
;;
(SMACRO 1+ ( LAMBDA (X) (BACKQUOTE (ADD1 , X))) )
;;
(SEXPR qsort ( LAMBDA (X) (COND ((NULL X) NIL) (T (APPEND (QSORT (REMOVE-IF 
(FUNCTION (LAMBDA (Z) (> Z (CAR X)))) (CDR X))) (LIST (CAR X)) (QSORT 
(REMOVE-IF (FUNCTION (LAMBDA (Z) (<= Z (CAR X)))) (CDR X))))))) )
;;
(SEXPR mkrnd-list ( LAMBDA (N M) (LET ((RES NIL)) (DOTIMES (I N RES) (SETQ 
RES (CONS (RND M) RES))))) )
;;
(SEXPR CalcExpr ( LAMBDA (EXPRESSION) (EVAL (INF2PREF (PARSE EXPRESSION)
))) )
;;
(SEXPR every ( LAMBDA (FUNCP &REST LST) (COND ((FORSOME LST (QUOTE NULL)
) T) (T (AND (APPLY FUNCP (MAPCAR (QUOTE CAR) LST)) (APPLY (QUOTE EVERY)
 (CONS FUNCP (APPLY (QUOTE MAPCAR) (LIST (QUOTE CDR) LST)))))))) )
;;
(SEXPR subseq! ( LAMBDA (X N &OPTIONAL (L 0)) (IF (OR (< N 0) (> N (SUB1 
(LENGTH X)))) (RAISEERROR "subseq!: индекс за пределами списка") (IF (= 
L 0) (IF (ZEROP N) X (LET ((R X)) (DOTIMES (I N R) (SETQ R (CDR R)))))
 (LET ((R X) (RES NIL) (K (MIN L (- (LENGTH X) N)))) (WHEN (> N 0) (DOTIMES 
(I N R) (SETQ R (CDR R)))) (DOTIMES (I K RES) (SETQ RES (APPEND RES (LIST 
(CAR R)))) (SETQ R (CDR R))))))) )
;;
(SEXPR position ( LAMBDA (X Y) (COND ((NOT (MEMBER X Y)) NIL) ((EQ X (CAR 
Y)) 0) (T (+ 1 (POSITION X (CDR Y)))))) )
;;
(SEXPR butlast ( LAMBDA (LST &OPTIONAL (N 1)) (SUBSEQ LST 0 (- (LENGTH 
LST) N))) )
;;
(SEXPR nbutlast ( LAMBDA (LST &OPTIONAL (N 1)) (LET ((ITE LST)) (DOTIMES 
(I (- (LENGTH LST) N 1) T) (SETQ ITE (CDR ITE))) (RPLACD ITE NIL) LST)
) )
;;
(SEXPR qsort-b ( LAMBDA (X FKEY) (COND ((NULL X) NIL) (T (APPEND (QSORT-B 
(REMOVE-IF (FUNCTION (LAMBDA (Z) (FUNCALL FKEY Z (CAR X)))) (CDR X)) FKEY)
 (LIST (CAR X)) (QSORT-B (REMOVE-IF-NOT (FUNCTION (LAMBDA (Z) (FUNCALL 
FKEY Z (CAR X)))) (CDR X)) FKEY))))) )
;;
(SEXPR suff-arr ( LAMBDA (STR) (LET ((RES NIL)) (DOTIMES (I (STRLEN STR)
 (QSORT-B RES (QUOTE (LAMBDA (X Y) (> (STRCMP X Y) 0))))) (SETQ RES (CONS 
(STRMID STR (+ I 1)) RES))))) )
;;
(SEXPR qsort-a ( LAMBDA (X FKEY) (COND ((NULL X) NIL) (T (APPEND (QSORT-A 
(REMOVE-IF (FUNCTION (LAMBDA (Z) (> (FUNCALL FKEY Z) (FUNCALL FKEY (CAR 
X))))) (CDR X)) FKEY) (LIST (CAR X)) (QSORT-A (REMOVE-IF (FUNCTION (LAMBDA 
(Z) (<= (FUNCALL FKEY Z) (FUNCALL FKEY (CAR X))))) (CDR X)) FKEY))))) 
)
;;
(SEXPR symbolp ( LAMBDA (X) (COND ((NUMBERP X) NIL) ((STRINGP X) NIL) ((BITSP 
X) NIL) ((NULL X) T) ((LISTP X) NIL) ((ATOM X) T) (T NIL))) )
;;
(SEXPR fib ( LAMBDA (N &OPTIONAL (LST (QUOTE (1 1)))) (COND ((= N 0) (CAR 
LST)) (T (FIB (- N 1) (CONS (+ (CAR LST) (CADR LST)) LST))))) )
;;
(SEXPR clear-prop ( LAMBDA (A) (LOOP (WHEN (NULL (PROPLIST A)) (RETURN 
A)) (REMPROP A (CAR (PROPLIST A))))) )
;;
(SEXPR elt ( LAMBDA (LST N) (COND ((OR (>= N (LENGTH LST)) (MINUSP N)) 
(RAISEERROR "elt: индекс за пределами списка")) ((ZEROP N) (CAR LST)) 
(T (DOTIMES (I N T) (SETQ LST (CDR LST))) (CAR LST)))) )
;;
(SEXPR range ( LAMBDA (N K) (ITER (FOR I FROM N TO K) (COLLECTING I))) 
)
;;
(SEXPR qbcolor ( LAMBDA (N) (COND ((EQ N 0) &H000000) ((EQ N 1) &H800000)
 ((EQ N 2) &H008000) ((EQ N 3) &H808000) ((EQ N 4) &H000080) ((EQ N 5)
 &H800080) ((EQ N 6) &H008080) ((EQ N 7) &HC0C0C0) ((EQ N 8) &H808080)
 ((EQ N 9) &HFF0000) ((EQ N 10) &H00FF00) ((EQ N 11) &HFFFF00) ((EQ N 
12) &H0000FF) ((EQ N 13) &HFF00FF) ((EQ N 14) &H00FFFF) ((EQ N 15) &HFFFFFF)
)) )
;;
(SEXPR count-if ( LAMBDA (PREDICATE LST &KEY (FROM-END NIL) (START NIL)
 (END NIL) (KEY NIL)) (COND ((AND START END) (COUNT-IF PREDICATE (SUBSEQ 
LST START END) :FROM-END FROM-END :KEY KEY)) (START (COUNT-IF PREDICATE 
(SUBSEQ LST START) :FROM-END FROM-END :KEY KEY)) (END (COUNT-IF PREDICATE 
(SUBSEQ LST 0 END) :FROM-END FROM-END :KEY KEY)) (FROM-END (COUNT-IF PREDICATE 
(REVERSE LST) :KEY KEY)) (KEY (APPLY (QUOTE +) (MAPCAR (FUNCTION (LAMBDA 
(X) (IF (FUNCALL PREDICATE (FUNCALL KEY X)) 1 0))) LST))) (T (APPLY (QUOTE 
+) (MAPCAR (FUNCTION (LAMBDA (X) (IF (FUNCALL PREDICATE X) 1 0))) LST)
)))) )
;;
(SEXPR count-if-not ( LAMBDA (PREDICATE LST &KEY (FROM-END NIL) (START 
NIL) (END NIL) (KEY NIL)) (LET ((NPREDICATE (FUNCTION (LAMBDA (X) (NOT 
(FUNCALL PREDICATE X)))))) (COUNT-IF NPREDICATE LST :FROM-END FROM-END 
:START START :END END :KEY KEY))) )
;;
(SEXPR substitute-if ( LAMBDA (REP P LST &KEY (COUNT NIL) (KEY NIL) (FROM-END 
NIL)) (LET* ((C 0) (!LST (IF FROM-END (REVERSE LST) LST)) (RES (COND ((NOT 
COUNT) (COND ((NOT KEY) (MAPCAR (FUNCTION (LAMBDA (ITEM) (IF (FUNCALL 
P ITEM) REP ITEM))) !LST)) (T (MAPCAR (FUNCTION (LAMBDA (ITEM) (IF (FUNCALL 
P (FUNCALL KEY ITEM)) REP ITEM))) !LST)))) (T (COND ((NOT KEY) (MAPCAR 
(FUNCTION (LAMBDA (ITEM) (IF (= C COUNT) ITEM (IF (FUNCALL P ITEM) (PROGN 
(SETQ C (ADD1 C)) REP) ITEM)))) !LST)) (T (MAPCAR (FUNCTION (LAMBDA (ITEM)
 (IF (= C COUNT) ITEM (IF (FUNCALL P (FUNCALL KEY ITEM)) (PROGN (SETQ 
C (ADD1 C)) REP) ITEM)))) !LST))))))) (IF FROM-END (REVERSE RES) RES))
) )
;;
(SEXPR substitute-if-not ( LAMBDA (REP P LST &KEY (COUNT NIL) (KEY NIL)
 (FROM-END NIL)) (SUBSTITUTE-IF REP (FUNCTION (LAMBDA (A) (NOT (FUNCALL 
P A)))) LST :COUNT COUNT :KEY KEY :FROM-END FROM-END)) )
;;
(SEXPR mapcan ( LAMBDA (F &REST LSTS) (APPLY (QUOTE NCONC) (APPLY (QUOTE 
MAPCAR) (CONS F LSTS)))) )
;;
(SEXPR mapcon ( LAMBDA (F &REST LSTS) (APPLY (QUOTE NCONC) (APPLY (QUOTE 
MAPLIST) (CONS F LSTS)))) )
;;
(SEXPR strRight ( LAMBDA (S L) (LET ((LL (STRLEN S))) (IF (> L LL) S (STRMID 
S (- LL L -1))))) )
;;
(SEXPR strLeft ( LAMBDA (S N) (STRMID S 1 N)) )
;;
(SEXPR strWords ( LAMBDA (STRI &OPTIONAL (W "") &KEY (DELIM " ")) (COND 
((ZEROP (STRLEN STRI)) (IF (ZEROP (STRLEN W)) NIL (LIST W))) ((EQ DELIM 
(STRLEFT STRI 1)) (IF (ZEROP (STRLEN W)) (STRWORDS (STRMID STRI 2) W :DELIM 
DELIM) (CONS W (STRWORDS (STRMID STRI 2) "" :DELIM DELIM)))) (T (STRWORDS 
(STRMID STRI 2) (STRCAT W (STRLEFT STRI 1)) :DELIM DELIM)))) )
;;
(SEXPR strSplit ( LAMBDA (TXT) (ITER (FOR I FROM 1 TO (STRLEN TXT)) (COLLECTING 
(STRMID TXT I 1)))) )
;;
(SEXPR strSplitA ( LAMBDA (TXT) (ITER (FOR I FROM 1 TO (STRLEN TXT)) (COLLECTING 
(STRASC (STRMID TXT I 1))))) )
;;
(SEXPR min-max-pos ( LAMBDA (LST) (LET ((AMIN (CAR LST)) (AMAX (CAR LST)
) (PMIN (LIST 0)) (PMAX (LIST 0)) (P 1)) (DOLIST (I (CDR LST) (LIST AMIN 
PMIN AMAX PMAX)) (COND ((= I AMAX) (PUSH P PMAX)) ((= I AMIN) (PUSH P 
PMIN)) ((> I AMAX) (SETQ AMAX I PMAX (LIST P))) ((< I AMIN) (SETQ AMIN 
I PMIN (LIST P)))) (SETQ P (+ 1 P))))) )
;;
(SEXPR max-sublist ( LAMBDA (LST) (CDR (REDUCE (FUNCTION (LAMBDA (ACC X)
 (LET ((L (IF (LISTP X) (LENGTH X) 0))) (IF (> L (CAR ACC)) (CONS L X)
 ACC)))) LST :INITIAL-VALUE (QUOTE (0 NIL))))) )
;;
(SEXPR replicate ( LAMBDA (ANY N) (COND ((ATOM ANY) (LET ((R NIL)) (DOTIMES 
(I N R) (PUSH ANY R)))) (T (LET ((R NIL)) (DOTIMES (I N R) (SETF R (APPEND 
R ANY))))))) )
;;
(SEXPR sum-every ( LAMBDA (LST SCALE) (REDUCE (QUOTE +) (MAPCAR (QUOTE 
*) LST (REPLICATE SCALE (+ 1 (\ (LENGTH LST) (LENGTH SCALE))))))) )
;;
(SEXPR random ( LAMBDA (X) (RND X)) )
;;
(SEXPR signum ( LAMBDA (X) (SIGN X)) )
;;
(SEXPR rem ( LAMBDA (X Y) (% X Y)) )
;;
(SEXPR mod ( LAMBDA (X Y) (% X Y)) )
;;
(SEXPR gcd ( LAMBDA (N M) (COND ((> M N) (GCD M N)) ((ZEROP M) N) (T (GCD 
M (REM N M))))) )
;;
(SEXPR lcm ( LAMBDA (N M) (\ (* N M) (GCD N M))) )
;;
(SMACRO sqrt ( LAMBDA (X) (BACKQUOTE (SQR , X))) )
;;
(SMACRO isqrt ( LAMBDA (X) (BACKQUOTE (FLO2FIX (SQR , X)))) )
;;
(SEXPR atom2string ( LAMBDA (A) (IF (ATOM A) (OUTPUT A) (RAISEERROR "ATOM2STRING неверный аргумент")
)) )
;;
(SMACRO incf ( LAMBDA (X) (BACKQUOTE (SETF , X (+ 1 , X)))) )
;;
(SMACRO decf ( LAMBDA (X) (BACKQUOTE (SETF , X (- , X 1)))) )
;;
(SEXPR $mkpair: ( LAMBDA (N V) (APPLY (QUOTE APPEND) (MAPCAR (QUOTE LIST)
 N V))) )
;;
(SEXPR $add: ( LAMBDA (V) (MAPCAR (LAMBDA (X) (IF (ATOM X) (IMPLODE (CONS 
(QUOTE :) (EXPLODE X))) (IMPLODE (CONS (QUOTE :) (EXPLODE (CAR X))))))
 V)) )
;;
(SEXPR $test-struct: ( LAMBDA (TEST PATT) (COND ((ATOM TEST) NIL) ((ATOM 
PATT) NIL) (T (LET ((RES (LIST (CAR TEST)))) (ITER (FOR A IN (CDR TEST)
) (FOR I UPFROM 0) (WHEN (ZEROP (MOD I 2)) (COLLECTING A INTO RES))) (EQUAL 
RES PATT))))) )
;;
(SMACRO defstruct ( LAMBDA (NAME-STR &REST FLDS) (COND ((NOT (ATOM NAME-STR)
) (RAISEERROR "Имя структуры - не атом")) ((PROPLIST NAME-STR) (RAISEERROR 
"Имя структуры должно быть чистым атомом")) (T (LET* ((NS (EXPLODE NAME-STR)
) (MAKE (IMPLODE (APPEND (QUOTE (M A K E -)) NS))) (COPY (IMPLODE (APPEND 
(QUOTE (C O P Y -)) NS))) (CHECK (IMPLODE (APPEND NS (QUOTE (- P))))) 
(VLDS ($ADD: FLDS)) (PARMS ($LPARMS: FLDS)) (PATT (CONS NAME-STR VLDS)
) (GETTS (BACKQUOTE ((, COPY (LAMBDA (INP) (COPY INP))) (, CHECK (LAMBDA 
(INP) ($TEST-STRUCT: INP (QUOTE , PATT))))))) (N 2)) (ITER (FOR VAR IN 
FLDS) (IF (ATOM VAR) (COLLECTING (LIST (IMPLODE (APPEND NS (QUOTE (-))
 (EXPLODE VAR))) (BACKQUOTE (LAMBDA (S) (NTH , N S)))) INTO GETTS) (COLLECTING 
(LIST (IMPLODE (APPEND NS (QUOTE (-)) (EXPLODE (CAR VAR)))) (BACKQUOTE 
(LAMBDA (S) (NTH , N S)))) INTO GETTS)) (SETQ N (+ 2 N))) (BACKQUOTE (PROGN 
(DEFINE ((, MAKE (LAMBDA (&KEY ,@ FLDS) (CONS (QUOTE , NAME-STR) ($MKPAIR: 
(QUOTE , VLDS) (LIST ,@ PARMS))))) ,@ GETTS)) (SPROPL (QUOTE , NAME-STR)
 (LIST (QUOTE STRUCTURE))))))))) )
;;
(SEXPR $lparms: ( LAMBDA (V) (MAPCAR (FUNCTION (LAMBDA (A) (IF (ATOM A)
 A (CAR A)))) V)) )
;;
(SEXPR lg ( LAMBDA (X) (/ (LOG X) (LOG 10))) )
;;
(SEXPR concl ( LAMBDA (LST &OPTIONAL B) (COND ((NULL LST) NIL) ((EQ (CAR 
LST) (QUOTE ЕСЛИ)) (CONCL (CDR LST) 0)) ((EQ (CAR LST) (QUOTE И)) (CONCL 
(CDR LST) B)) ((EQ (CAR LST) (QUOTE ТО)) (CONCL (CDR LST) 1)) (T (COND 
((= B 1) (CONS (CAR LST) (CONCL (CDR LST) B))) (T (CONCL (CDR LST) B))
)))) )
;;
(SEXPR condit ( LAMBDA (LST) (COND ((NULL LST) NIL) ((EQ (CAR LST) (QUOTE 
ЕСЛИ)) (CONDIT (CDR LST))) ((EQ (CAR LST) (QUOTE И)) (CONDIT (CDR LST)
)) ((EQ (CAR LST) (QUOTE ТО)) NIL) (T (CONS (CAR LST) (CONDIT (CDR LST)
))))) )
;;
(SEXPR incl ( LAMBDA (A LST) (COND ((NULL LST) NIL) ((EQ (CAR LST) A) T)
 (T (INCL A (CDR LST))))) )
;;
(SEXPR concl! ( LAMBDA (LST) (CONCL LST 0)) )
;;
(SEXPR make-ruLe ( LAMBDA (&KEY NAME_ COND_ CONCL_) (CONS (QUOTE RULE) 
($MKPAIR: (QUOTE (:NAME_ :COND_ :CONCL_)) (LIST NAME_ COND_ CONCL_))))
 )
;;
(SEXPR copy-ruLe ( LAMBDA (INP) (COPY INP)) )
;;
(SEXPR ruLe-p ( LAMBDA (INP) ($TEST-STRUCT: INP (QUOTE (RULE :NAME_ :COND_ 
:CONCL_)))) )
;;
(SEXPR ruLe-name_ ( LAMBDA (S) (NTH 2 S)) )
;;
(SEXPR ruLe-cond_ ( LAMBDA (S) (NTH 4 S)) )
;;
(SEXPR ruLe-concL_ ( LAMBDA (S) (NTH 6 S)) )
;;
(SEXPR init_rules ( LAMBDA NIL (MAPCAR (LAMBDA (A) (MAKE-RULE :NAME_ A 
:COND_ (CONDIT (EVAL A)) :CONCL_ (CONCL (EVAL A)))) *БАЗА-ЗНАНИЙ*)) )
;;
(SEXPR finit_rules ( LAMBDA NIL (LET ((N 0)) (SETQ *БАЗА-ДАННЫХ* (MAPCAR 
(LAMBDA (A) (SETQ N (+ N 1)) (MAKE-RULE :NAME_ (STRCAT "правило" (FIX2STR 
N)) :COND_ (CONDIT (CRUTCH A)) :CONCL_ (CONCL (CRUTCH A)))) *БАЗА-ДАННЫХ*)
))) )
;;
(SEXPR CRUTCH ( LAMBDA (LST) (LET ((C NIL) (RES (QUOTE (ЕСЛИ)))) (MAPCAR 
(LAMBDA (A) (COND ((OR (EQ A (QUOTE И)) (EQ A (QUOTE ТО))) (SETQ RES (APPEND 
RES (LIST C))) (SETQ RES (APPEND RES (LIST A))) (SETQ C NIL)) (T (WHEN 
(NOT (EQ A (QUOTE ЕСЛИ))) (SETQ C (APPEND C (LIST A))) (PRINTLINE C)))
)) LST) (APPEND RES (LIST C)))) )
;;
(SEXPR load-data ( LAMBDA (FNAME) (LET ((BASE NIL)) (ITER (FOR STRI IN-FILE 
(STRCAT (SYSHOME) "\" FNAME)) (COLLECTING (INPUT (STRCAT "(" STRI ")")
) INTO BASE)) (SETQ *БАЗА-ДАННЫХ* BASE) (SETQ *CONDSF* BASE))) )
;;
(SEXPR prob ( LAMBDA (HYP &AUX (LL NIL)) (MAPCAR (LAMBDA (A) (WHEN (MEMBER 
HYP (RULE-CONCL_ A)) (SETQ LL (CONS (RULE-NAME_ A) LL)))) *БАЗА-ДАННЫХ*)
 (COND ((NOT (NULL LL)) LL) (T (QUOTE ПРАВИЛО_ОТСУТСТВУЕТ)))) )
;;
(SEXPR add-concl ( LAMBDA (R &AUX RUL) (COND ((NOT (EQ (QUOTE ПРАВИЛО_ОТСУТСТВУЕТ)
 R)) (MAPCAR (LAMBDA (A) (WHEN (EQ (RULE-NAME_ A) R) (SETQ RUL A))) *БАЗА-ДАННЫХ*)
 (MAPCAR (LAMBDA (A) (WHEN (NOT (MEMBER A *ФАКТЫ*)) (PUSH A *ФАКТЫ*)))
 (RULE-CONCL_ RUL)) (STRCAT "Согласно правилу №" (STRDEL (RULE-NAME_ RUL)
 1 7) " " (OUTPUT (RULE-CONCL_ RUL)))) (T (QUOTE ERROR!)))) )
;;
(SEXPR proof ( LAMBDA (HYP) (LET ((P NIL) (RUL NIL)) (COND ((MEMBER HYP 
*ФАКТЫ*) (QUOTE OK!)) (T (SETQ P (PROB HYP)) (MAPCAR (LAMBDA (A) (WHEN 
(EQ (RULE-NAME_ A) P) (SETQ RUL A))) *БАЗА-ДАННЫХ*) (COND ((EVERY (LAMBDA 
(A) (MEMBER A *ФАКТЫ*)) (RULE-COND_ RUL)) (PUSH HYP *ФАКТЫ*) (QUOTE OK!)
) (T (MAPCAR (LAMBDA (A) (PROOF A)) (RULE-COND_ RUL)))))))) )
;;
(SEXPR add-fact ( LAMBDA (STR) (LET ((ACC NIL) (RES NIL) (INP (INPUT (STRCAT 
"(" STR " и)")))) (ITER (FOR A IN INP) (COND ((EQ (QUOTE И) A) (COLLECTING 
ACC INTO RES) (SETQ ACC NIL)) (T (COLLECTING A INTO ACC)))) RES)) )
;;
(SEXPR define_ ( LAMBDA (CHARS) (LET ((CC (ADD-FACT CHARS)) (POSSIBLE NIL)
 (HIT NIL)) (MAPCAR (LAMBDA (A) (WHEN (NOT (OR (NULL (INTERSECR (RULE-COND_ 
A) CC)) (MEMBER (CAR (RULE-CONCL_ A)) POSSIBLE))) (SETQ POSSIBLE (CONS 
(CAR (RULE-CONCL_ A)) POSSIBLE)) (SETQ HIT (CONS (LENGTH (INTERSECR (RULE-COND_ 
A) CC)) HIT)))) *БАЗА-ДАННЫХ*) (SETQ *ГИПОТЕЗЫ* POSSIBLE) (COND ((NOT 
(NULL POSSIBLE)) (ZIP POSSIBLE HIT)) (T "Я не знаю таких животных"))))
 )
;;
(SEXPR INTERSECR ( LAMBDA (L1 L2) (REMOVE-IF-NOT (LAMBDA (A) (MEMBER A 
L2)) L1)) )
;;
(SEXPR ZIP ( LAMBDA (LST1 LST2) (COND ((AND (NULL LST1) (NULL LST2)) NIL)
 ((NULL LST1) (CONS (LIST (CAR LST2)) (ZIP LST1 (CDR LST2)))) ((NULL LST2)
 (CONS (LIST (CAR LST1)) (ZIP (CDR LST1) LST2))) (T (CONS (LIST (CAR LST1)
 (CAR LST2)) (ZIP (CDR LST1) (CDR LST2)))))) )
;;
(SEXPR dif ( LAMBDA (L1 L2) (COND ((NULL L2) L1) ((NULL L1) NIL) ((NOT 
(MEMBER (CAR L1) L2)) (CONS (CAR L1) (DIF (CDR L1) L2))) (T (DIF (CDR 
L1) L2)))) )
;;
(SEXPR ins-in-all ( LAMBDA (LST A) (LET ((N (LENGTH LST)) (R NIL)) (ITER 
(FOR I FROM 0 TO N) (COLLECTING (INS-IN-POS LST A I) INTO R)) R)) )
;;
(SEXPR INS-IN-POS ( LAMBDA (LST A N) (IF (= N 0) (CONS A LST) (CONS (CAR 
LST) (INS-IN-POS (CDR LST) A (- N 1))))) )
;;
(SEXPR output_pos ( LAMBDA (POS HIT) (INS-IN-ALL (ZIP POS HIT) (QUOTE MATCH)
)) )
;;
(SEXPR maxIn ( LAMBDA (LIST &OPTIONAL (MAX 0)) (COND ((NOT (LISTP LIST)
) -1) ((NULL LIST) MAX) ((> (CAR (CDR (CAR LIST))) MAX) (MAXIN (CDR LIST)
 (CAR (CDR (CAR LIST))))) (T (MAXIN (CDR LIST) MAX)))) )
;;
(SEXPR outputAnimals ( LAMBDA (LST &OPTIONAL (MAX (MAXIN LST))) (COND ((NOT 
(LISTP LST)) (list LST)) ((NULL LST) NIL) ((= (CAR (CDR (CAR LST))) MAX) (CONS 
(LIST (CAR (CAR LST)) (PROB (CAR (CAR LST))) (NTH (- (STR2FIX (STRMID 
(CAR (PROB (CAR (CAR LST)))) 8 2)) 1) *CONDSF*)) (OUTPUTANIMALS (CDR LST)
 MAX))) (T (OUTPUTANIMALS (CDR LST) MAX)))) )
;;
(SEXPR output2 ( LAMBDA (LST &AUX (STR "")) (COND ((NOT (LISTP LST)) LST)
 (T (MAPCAR (LAMBDA (A) (SETQ STR (STRCAT STR (OUTPUT A) (STRCHR 10)))
) LST) STR))) )
;;
;; Константы
;;
(CSETQ _Pi 3.1415926535897900E+0)
(CSETQ _E 2.7182818284590500E+0)
(CSETQ _LPAR "(")
(CSETQ _RPAR ")")
(CSETQ _INPUT 0)
(CSETQ _OUTPUT 1)
(CSETQ _APPEND 2)
(CSETQ _BINARY_READ 3)
(CSETQ _BINARY_WRITE 4)
(CSETQ _BINARY_READ_WRITE 5)
(CSETQ _TEXT_ARRAY -1)
(CSETQ _WHITE &HFFFFFF)
(CSETQ _BLACK &H000000)
(CSETQ _RED &HFF0000)
(CSETQ _LIME &H00FF00)
(CSETQ _BLUE &H0000FF)
(CSETQ _YELLOW &HFFFF00)
(CSETQ _AQUA &H00FFFF)
(CSETQ _FUCHSIA &HFF00FF)
(CSETQ _GREEN &H008000)
(CSETQ _SILVER &HC0C0C0)
(CSETQ _GRAY &H808080)
(CSETQ _MAROON &H800000)
(CSETQ _OLIVE &H808000)
(CSETQ _NAVY &H000080)
(CSETQ _PURPLE &H800080)
(CSETQ _TEAL &H008080)
(CSETQ _LABEL 1)
(CSETQ _TEXT 2)
(CSETQ _LIST 3)
(CSETQ _COMBO 4)
(CSETQ _BUTTON 5)
(CSETQ _CHECK 6)
(CSETQ _OPTION 7)
(CSETQ _CENTER 2)
(CSETQ _LEFT 0)
(CSETQ _RIGHT 1)
(CSETQ HKEY_CLASSES_ROOT &H80000000)
(CSETQ HKEY_CURRENT_CONFIG &H80000005)
(CSETQ HKEY_CURRENT_USER &H80000001)
(CSETQ HKEY_DYN_DATA &H80000006)
(CSETQ HKEY_LOCAL_MACHINE &H80000002)
(CSETQ HKEY_PERFORMANCE_DATA &H80000004)
(CSETQ HKEY_USERS &H80000003)
(CSETQ KEY_ALL_ACCESS &HF003F)
(CSETQ KEY_CREATE_LINK &H20)
(CSETQ KEY_CREATE_SUB_KEY &H4)
(CSETQ KEY_ENUMERATE_SUB_KEYS &H8)
(CSETQ KEY_EXECUTE &H20019)
(CSETQ KEY_NOTIFY &H10)
(CSETQ KEY_QUERY_VALUE &H1)
(CSETQ KEY_READ &H20019)
(CSETQ KEY_SET_VALUE &H2)
(CSETQ KEY_WRITE &H20006)
(CSETQ REG_BINARY 3)
(CSETQ REG_DWORD 4)
(CSETQ REG_DWORD_BIG_ENDIAN 5)
(CSETQ REG_DWORD_LITTLE_ENDIAN 4)
(CSETQ REG_EXPAND_SZ 2)
(CSETQ REG_LINK 6)
(CSETQ REG_MULTI_SZ 7)
(CSETQ REG_NONE 0)
(CSETQ REG_RESOURCE_LIST 8)
(CSETQ REG_SZ 1)
(CSETQ _BMP 0)
(CSETQ _GIF 1)
(CSETQ _JPG 2)
;;
;; Глобальные переменные
;;
(SETQ *CONDSF* '((ЕСЛИ ЖИВОТНОЕ ИМЕЕТ ШЕРСТЬ ТО ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ) (ЕСЛИ ЖИВОТНОЕ КОРМИТ ДЕТЕНЫШЕЙ МОЛОКОМ ТО ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ) (ЕСЛИ ЖИВОТНОЕ ИМЕЕТ ПЕРЬЯ ТО ЖИВОТНОЕ ПТИЦА) (ЕСЛИ ЖИВОТНОЕ УМЕЕТ ЛЕТАТЬ И ЖИВОТНОЕ НЕСЕТ ЯЙЦА ТО ЖИВОТНОЕ ПТИЦА) (ЕСЛИ ЖИВОТНОЕ ЕСТ МЯСО ТО ЖИВОТНОЕ ХИЩНИК) (ЕСЛИ ЖИВОТНОЕ ИМЕЕТ ОСТРЫЕ ЗУБЫ И ЖИВОТНОЕ ИМЕЕТ КОГТИ И ГЛАЗА ЖИВОТНОГО ПОСАЖЕНЫ ПРЯМО ТО ЖИВОТНОЕ ХИЩНИК) (ЕСЛИ ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ И ЖИВОТНОЕ ИМЕЕТ КОПЫТА ТО ЖИВОТНОЕ ЖВАЧНОЕ) (ЕСЛИ ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ И ЖИВОТНОЕ ЖУЕТ ЖВАЧКУ ТО ЖИВОТНОЕ ЖВАЧНОЕ) (ЕСЛИ ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ И ЖИВОТНОЕ ХИЩНИК И ЖИВОТНОЕ ЖЕЛТО-КОРИЧНЕВОЕ И ЖИВОТНОЕ ИМЕЕТ ТЕМНЫЕ ПЯТНА ТО ЖИВОТНОЕ ГЕПАРД) (ЕСЛИ ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ И ЖИВОТНОЕ ХИЩНИК И ЖИВОТНОЕ ЖЕЛТО-КОРИЧНЕВОЕ И ЖИВОТНОЕ ПОЛОСАТОЕ ТО ЖИВОТНОЕ ТИГР) (ЕСЛИ ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ И ЖИВОТНОЕ ДЛИННОШЕЕЕ И ЖИВОТНОЕ ДЛИННОНОГОЕ И ЖИВОТНОЕ ИМЕЕТ ТЕМНЫЕ ПЯТНА ТО ЖИВОТНОЕ ЖИРАФ) (ЕСЛИ ЖИВОТНОЕ ЖВАЧНОЕ И ЖИВОТНОЕ ПОЛОСАТОЕ ТО ЖИВОТНОЕ ЗЕБРА) (ЕСЛИ ЖИВОТНОЕ ПТИЦА И ЖИВОТНОЕ НЕ УМЕЕТ ЛЕТАТЬ И ЖИВОТНОЕ ДЛИННОШЕЕЕ И ЖИВОТНОЕ ДЛИННОНОГОЕ И ЖИВОТНОЕ ЧЕРНО-БЕЛОЕ ТО ЖИВОТНОЕ СТРАУС) (ЕСЛИ ЖИВОТНОЕ ПТИЦА И ЖИВОТНОЕ НЕ УМЕЕТ ЛЕТАТЬ И ЖИВОТНОЕ ПЛАВАЕТ И ЖИВОТНОЕ ЧЕРНО-БЕЛОЕ ТО ЖИВОТНОЕ ПИНГВИН)))
(SETQ * 'output2)
(SETQ *БАЗА-ЗНАНИЙ* '(ПРАВИЛО1 ПРАВИЛО2 ПРАВИЛО3 ПРАВИЛО4 ПРАВИЛО5 ПРАВИЛО6 ПРАВИЛО7 ПРАВИЛО8 ПРАВИЛО9 ПРАВИЛО10 ПРАВИЛО11 ПРАВИЛО12 ПРАВИЛО13 ПРАВИЛО14))
(SETQ ПРАВИЛО1 '(ЕСЛИ ЖИВОТНОЕ ИМЕЕТ ШЕРСТЬ ТО ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ))
(SETQ ПРАВИЛО2 '(ЕСЛИ ЖИВОТНОЕ КОРМИТ ДЕТЕНЫШЕЙ МОЛОКОМ ТО ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ))
(SETQ ПРАВИЛО3 '(ЕСЛИ ЖИВОТНОЕ ИМЕЕТ ПЕРЬЯ ТО ЖИВОТНОЕ ПТИЦА))
(SETQ ПРАВИЛО4 '(ЕСЛИ ЖИВОТНОЕ УМЕЕТ ЛЕТАТЬ И ЖИВОТНОЕ НЕСЕТ ЯЙЦА ТО ЖИВОТНОЕ ПТИЦА))
(SETQ ПРАВИЛО5 '(ЕСЛИ ЖИВОТНОЕ ЕСТ МЯСО ТО ЖИВОТНОЕ ХИЩНИК))
(SETQ ПРАВИЛО6 '(ЕСЛИ ЖИВОТНОЕ ИМЕЕТ ОСТРЫЕ ЗУБЫ И ЖИВОТНОЕ ИМЕЕТ КОГТИ И ГЛАЗА ЖИВОТНОГО ПОСАЖЕНЫ ПРЯМО ТО ЖИВОТНОЕ ХИЩНИК))
(SETQ ПРАВИЛО7 '(ЕСЛИ ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ И ЖИВОТНОЕ ИМЕЕТ КОПЫТА ТО ЖИВОТНОЕ ЖВАЧНОЕ))
(SETQ ПРАВИЛО8 '(ЕСЛИ ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ И ЖИВОТНОЕ ЖУЕТ ЖВАЧКУ ТО ЖИВОТНОЕ ЖВАЧНОЕ))
(SETQ ПРАВИЛО9 '(ЕСЛИ ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ И ЖИВОТНОЕ ХИЩНИК И ЖИВОТНОЕ ЖЕЛТО-КОРИЧНЕВОЕ И ЖИВОТНОЕ ИМЕЕТ ТЕМНЫЕ ПЯТНА ТО ЖИВОТНОЕ ГЕПАРД))
(SETQ ПРАВИЛО10 '(ЕСЛИ ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ И ЖИВОТНОЕ ХИЩНИК И ЖИВОТНОЕ ЖЕЛТО-КОРИЧНЕВОЕ И ЖИВОТНОЕ ПОЛОСАТОЕ ТО ЖИВОТНОЕ ТИГР))
(SETQ ПРАВИЛО11 '(ЕСЛИ ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ И ЖИВОТНОЕ ДЛИННОШЕЕЕ И ЖИВОТНОЕ ДЛИННОНОГОЕ И ЖИВОТНОЕ ИМЕЕТ ТЕМНЫЕ ПЯТНА ТО ЖИВОТНОЕ ЖИРАФ))
(SETQ ПРАВИЛО12 '(ЕСЛИ ЖИВОТНОЕ ЖВАЧНОЕ И ЖИВОТНОЕ ПОЛОСАТОЕ ТО ЖИВОТНОЕ ЗЕБРА))
(SETQ ПРАВИЛО13 '(ЕСЛИ ЖИВОТНОЕ ПТИЦА И ЖИВОТНОЕ НЕ УМЕЕТ ЛЕТАТЬ И ЖИВОТНОЕ ДЛИННОШЕЕЕ И ЖИВОТНОЕ ДЛИННОНОГОЕ И ЖИВОТНОЕ ЧЕРНО-БЕЛОЕ ТО ЖИВОТНОЕ СТРАУС))
(SETQ ПРАВИЛО14 '(ЕСЛИ ЖИВОТНОЕ ПТИЦА И ЖИВОТНОЕ НЕ УМЕЕТ ЛЕТАТЬ И ЖИВОТНОЕ ПЛАВАЕТ И ЖИВОТНОЕ ЧЕРНО-БЕЛОЕ ТО ЖИВОТНОЕ ПИНГВИН))
(SETQ *ГИПОТЕЗЫ* '((ЖИВОТНОЕ ПИНГВИН) (ЖИВОТНОЕ СТРАУС)))
(SETQ *БАЗА-ДАННЫХ* '((RULE :NAME_ "правило1" :COND_ ((ЖИВОТНОЕ ИМЕЕТ ШЕРСТЬ)) :CONCL_ ((ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ))) (RULE :NAME_ "правило2" :COND_ ((ЖИВОТНОЕ КОРМИТ ДЕТЕНЫШЕЙ МОЛОКОМ)) :CONCL_ ((ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ))) (RULE :NAME_ "правило3" :COND_ ((ЖИВОТНОЕ ИМЕЕТ ПЕРЬЯ)) :CONCL_ ((ЖИВОТНОЕ ПТИЦА))) (RULE :NAME_ "правило4" :COND_ ((ЖИВОТНОЕ УМЕЕТ ЛЕТАТЬ) (ЖИВОТНОЕ НЕСЕТ ЯЙЦА)) :CONCL_ ((ЖИВОТНОЕ ПТИЦА))) (RULE :NAME_ "правило5" :COND_ ((ЖИВОТНОЕ ЕСТ МЯСО)) :CONCL_ ((ЖИВОТНОЕ ХИЩНИК))) (RULE :NAME_ "правило6" :COND_ ((ЖИВОТНОЕ ИМЕЕТ ОСТРЫЕ ЗУБЫ) (ЖИВОТНОЕ ИМЕЕТ КОГТИ) (ГЛАЗА ЖИВОТНОГО ПОСАЖЕНЫ ПРЯМО)) :CONCL_ ((ЖИВОТНОЕ ХИЩНИК))) (RULE :NAME_ "правило7" :COND_ ((ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ) (ЖИВОТНОЕ ИМЕЕТ КОПЫТА)) :CONCL_ ((ЖИВОТНОЕ ЖВАЧНОЕ))) (RULE :NAME_ "правило8" :COND_ ((ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ) (ЖИВОТНОЕ ЖУЕТ ЖВАЧКУ)) :CONCL_ ((ЖИВОТНОЕ ЖВАЧНОЕ))) (RULE :NAME_ "правило9" :COND_ ((ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ) (ЖИВОТНОЕ ХИЩНИК) (ЖИВОТНОЕ ЖЕЛТО-КОРИЧНЕВОЕ) (ЖИВОТНОЕ ИМЕЕТ ТЕМНЫЕ ПЯТНА)) :CONCL_ ((ЖИВОТНОЕ ГЕПАРД))) (RULE :NAME_ "правило10" :COND_ ((ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ) (ЖИВОТНОЕ ХИЩНИК) (ЖИВОТНОЕ ЖЕЛТО-КОРИЧНЕВОЕ) (ЖИВОТНОЕ ПОЛОСАТОЕ)) :CONCL_ ((ЖИВОТНОЕ ТИГР))) (RULE :NAME_ "правило11" :COND_ ((ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ) (ЖИВОТНОЕ ДЛИННОШЕЕЕ) (ЖИВОТНОЕ ДЛИННОНОГОЕ) (ЖИВОТНОЕ ИМЕЕТ ТЕМНЫЕ ПЯТНА)) :CONCL_ ((ЖИВОТНОЕ ЖИРАФ))) (RULE :NAME_ "правило12" :COND_ ((ЖИВОТНОЕ ЖВАЧНОЕ) (ЖИВОТНОЕ ПОЛОСАТОЕ)) :CONCL_ ((ЖИВОТНОЕ ЗЕБРА))) (RULE :NAME_ "правило13" :COND_ ((ЖИВОТНОЕ ПТИЦА) (ЖИВОТНОЕ НЕ УМЕЕТ ЛЕТАТЬ) (ЖИВОТНОЕ ДЛИННОШЕЕЕ) (ЖИВОТНОЕ ДЛИННОНОГОЕ) (ЖИВОТНОЕ ЧЕРНО-БЕЛОЕ)) :CONCL_ ((ЖИВОТНОЕ СТРАУС))) (RULE :NAME_ "правило14" :COND_ ((ЖИВОТНОЕ ПТИЦА) (ЖИВОТНОЕ НЕ УМЕЕТ ЛЕТАТЬ) (ЖИВОТНОЕ ПЛАВАЕТ) (ЖИВОТНОЕ ЧЕРНО-БЕЛОЕ)) :CONCL_ ((ЖИВОТНОЕ ПИНГВИН)))))
(SETQ *ФАКТЫ* '((ЖИВОТНОЕ ГЕПАРД) (ЖИВОТНОЕ ТИГР) (ЖИВОТНОЕ ЗЕБРА) (ЖИВОТНОЕ СТРАУС) (ЖИВОТНОЕ ПИНГВИН) (ЖИВОТНОЕ МЛЕКОПИТАЮЩЕЕ) (ЖИВОТНОЕ ЖИРАФ)))
