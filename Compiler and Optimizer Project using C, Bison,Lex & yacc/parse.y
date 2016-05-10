%{
#include <stdio.h>
#include "attr.h"
#include "instrutil.h"
int yylex();
void yyerror(char * s);
#include "symtab.h"

FILE *outfile;
char *CommentBuffer;
int count=0;
int jumpLabel=-1;
int rlabel;
int for_flag=0;
int store_flag=0;
extern int optimize_flag; 
%}

%union {tokentype token;
	RegInfo regInfo;
	LabelInfo labelInfo;
	TypeData typeData;
	ForLabelInfo forLabelInfo;
	VariableList variableList;
       }

%token PROG PERIOD VAR 
%token INT WRITELN THEN IF BEG END 
%token DO ARRAY OF REPEAT UNTIL ELSE FOR
%token ASG EQ NEQ LT LEQ 
%token <token> ID ICONST 

%type <regInfo> exp lhs condexp writestmt
%type <labelInfo> ifhead ifstmt
%type <typeData> type
%type <forLabelInfo> ctrlexp
%type <variableList> idlist

%start program

%nonassoc EQ NEQ LT LEQ GT GEQ 
%left '+' '-' 
%left '*' 

%nonassoc THEN
%nonassoc ELSE

%%
program : {emitComment("Assign STATIC_AREA_ADDRESS to register \"r0\"");
           emit(NOLABEL, LOADI, STATIC_AREA_ADDRESS, 0, EMPTY);} 
           PROG ID ';' block PERIOD { }
	;

block	: variables cmpdstmt { }
	;

variables: /* empty */
	| VAR vardcls { }
	;

vardcls	: vardcls vardcl ';' { }
	| vardcl ';' { }
	| error ';' { yyerror("***Error: illegal variable declaration\n");}  
	;

vardcl	: idlist ':' type {int i=0; for(i=0;$1.variables[i]!=NULL;i++)
									{
										if($3.type<=2){
										insert($1.variables[i],NextOffset($3.rows),$3.rows,$3.cols);}
										else {
											if($3.type==3)
											{
												insert($1.variables[i],NextOffset($3.rows * $3.cols),$3.rows,$3.cols);
											}
										}
								
									}
								count=0; 
							}
	;

type	: INT 	{ $$.rows=1; $$.type=1;}
	| ARRAY '[' ICONST ']' OF INT	{ $$.rows=$3.num; $$.type=2;}	
        | ARRAY '[' ICONST ',' ICONST ']' OF INT { $$.rows=$3.num; $$.cols=$5.num; $$.type=3;}
	;

idlist	: idlist ',' ID { $$.variables[count++]=$3.str; }
        | ID		{ $$.variables[count++]=$1.str; }
	;

stmtlist : stmtlist ';' stmt { }
	| stmt { }
        | error { yyerror("***Error: ';' expected or illegal statement \n");}
	;

stmt    : ifstmt { emit($1.true_label,NOP,EMPTY,EMPTY,EMPTY); }
			  
	| fstmt { }
	| rstmt { }
	| astmt { }
	| writestmt { }
	| cmpdstmt { }
	;

cmpdstmt: BEG stmtlist END { }
	;

ifstmt :  ifhead 
          THEN stmt {$$.true_label= $1.false_label; } 
        | ifhead THEN stmt ELSE {jumpLabel=NextLabel(); emit(NOLABEL,BR,jumpLabel,EMPTY,EMPTY);
								emit($1.false_label,NOP,EMPTY,EMPTY,EMPTY);} stmt { $$.true_label=jumpLabel; }
	;


ifhead : IF condexp { int true_label=NextLabel();int false_label=NextLabel(); emit(NOLABEL,CBR,$2.reg_no,true_label,false_label); $$.true_label=true_label;
						$$.false_label=false_label; emit(true_label,NOP,EMPTY,EMPTY,EMPTY);}
        ;

writestmt: WRITELN{store_flag=1;} '(' exp ')' { int offset=NextOffset(1); 			
					emit(NOLABEL,STOREAI,$4.reg_no,0,offset); 

					emit(NOLABEL,OUTPUT,1024+offset,EMPTY,EMPTY);}
	;

fstmt	: FOR ctrlexp DO {for_flag=1;} stmt { int reg,reg1; 

					reg=NextRegister();
					reg1=NextRegister();
					emit(NOLABEL,LOADAI,0,$2.counter_offset,reg);
					emit(NOLABEL,ADDI,reg,1,reg1);	

					emit(NOLABEL,STOREAI,reg1,0,$2.counter_offset);

					emit(NOLABEL,BR,$2.init_label,EMPTY,EMPTY);
					emit($2.false_label,NOP,EMPTY,EMPTY,EMPTY);
				} 
	;

astmt : lhs ASG exp { 
		emit(NOLABEL,STORE,$3.reg_no,$1.reg_no,EMPTY); 
		if(optimize_flag==1)
		{
			opdelete($1.str);
		}
}
	;

rstmt : REPEAT {rlabel=NextLabel(); emit(rlabel,NOP,EMPTY,EMPTY,EMPTY);} stmt UNTIL condexp { int label=NextLabel(); emit(NOLABEL,CBR,$5.reg_no,label,rlabel); emit(label,NOP,EMPTY,EMPTY,EMPTY); }

lhs	:  ID			    { int reg,reg1;
					if(optimize_flag==1)
					{ 
						reg=checkInOPTable(LOADI,lookup($1.str)->offset,EMPTY," ");
						if(reg>=getCurrent())
						{
							emit(NOLABEL,LOADI,lookup($1.str)->offset,reg,EMPTY);
						}
						reg1=checkInOPTable(ADD,0,reg," ");
						if(reg1>=getCurrent())
						{
							emit(NOLABEL,ADD,0,reg,reg1);
						}
					}

				else if(optimize_flag==0)
				{
					reg=NextRegister();
					reg1=NextRegister();
					emit(NOLABEL,LOADI,lookup($1.str)->offset,reg,EMPTY);
					emit(NOLABEL,ADD,0,reg,reg1);
				}
					

						  $$.reg_no=reg1;
						  $$.str=$1.str;

						}
        |  ID '[' exp ']'	    { int reg,reg1,reg2,reg3,reg4; 
					if(optimize_flag==1)
					{ 
						reg=checkInOPTable(LOADI,4,EMPTY," ");
						if(reg>=getCurrent())
						{
							emit(NOLABEL,LOADI,4,reg,EMPTY);
						}
						reg1=checkInOPTable(MULT,$3.reg_no,reg," ");
						if(reg1>=getCurrent())
						{
							emit(NOLABEL,MULT,$3.reg_no,reg,reg1);
						}
						reg2=checkInOPTable(LOADI,lookup($1.str)->offset,EMPTY," ");
						if(reg2>=getCurrent())
						{
							emit(NOLABEL,LOADI,lookup($1.str)->offset,reg2,EMPTY);
						}
						reg3=checkInOPTable(ADD,reg1,reg2," ");
						if(reg3>=getCurrent())
						{
							emit(NOLABEL,ADD,reg1,reg2,reg3);
						}
						reg4=checkInOPTable(ADD,0,reg3," ");
						if(reg4>=getCurrent())
						{		
							emit(NOLABEL,ADD,0,reg3,reg4);
						}
					}

					else if(optimize_flag==0)
					{
						reg=NextRegister();
						reg1=NextRegister();
						reg2=NextRegister();
						reg3=NextRegister();
						reg4=NextRegister();
						emit(NOLABEL,LOADI,4,reg,EMPTY);
						emit(NOLABEL,MULT,$3.reg_no,reg,reg1);
						emit(NOLABEL,LOADI,lookup($1.str)->offset,reg2,EMPTY);
						emit(NOLABEL,ADD,reg1,reg2,reg3);
						emit(NOLABEL,ADD,0,reg3,reg4);
					}
					
					$$.str=$1.str;
					$$.reg_no=reg4;}

        |  ID '[' exp ',' exp ']'   { int reg,reg1,reg2,reg3,reg4,reg5,reg6,reg7;

					if(optimize_flag==1)
					{ 
						reg=checkInOPTable(LOADI,4,EMPTY," ");
						if(reg>=getCurrent())
						{
					emit(NOLABEL,LOADI,4,reg,EMPTY);
						}
						reg1=checkInOPTable(LOADI,lookup($1.str)->rows,EMPTY," ");
						if(reg1>=getCurrent())
						{
					emit(NOLABEL,LOADI,lookup($1.str)->rows,reg1,EMPTY);
						}
						reg2=checkInOPTable(MULT,reg1,$5.reg_no," ");
						if(reg2>=getCurrent())
						{
						emit(NOLABEL,MULT,reg1,$5.reg_no,reg2);
						}
						reg3=checkInOPTable(ADD,reg2,$3.reg_no," ");
						if(reg3>=getCurrent())
						{
					emit(NOLABEL,ADD,reg2,$3.reg_no,reg3);
						}
						reg4=checkInOPTable(MULT,reg3,reg," ");
						if(reg4>=getCurrent())
						{
					emit(NOLABEL,MULT,reg3,reg,reg4);
						}
						reg5=checkInOPTable(LOADI,lookup($1.str)->offset,EMPTY," ");
						if(reg5>=getCurrent())
						{
					emit(NOLABEL,LOADI,lookup($1.str)->offset,reg5,EMPTY);
						}
						reg6=checkInOPTable(ADD,reg4,reg5," ");
						if(reg6>=getCurrent())
						{
					emit(NOLABEL,ADD,reg4,reg5,reg6);
						}
						reg7=checkInOPTable(ADD,0,reg6," ");
						if(reg7>=getCurrent())
						{
					emit(NOLABEL,ADD,0,reg6,reg7);
						}
					}

					else if(optimize_flag==0)
					{
						reg=NextRegister();
						reg1=NextRegister();
						reg2=NextRegister();
						reg3=NextRegister();
						reg4=NextRegister();
						reg5=NextRegister();
						reg6=NextRegister();
						reg7=NextRegister();
					emit(NOLABEL,LOADI,4,reg,EMPTY);
					emit(NOLABEL,LOADI,lookup($1.str)->rows,reg1,EMPTY);
					emit(NOLABEL,MULT,reg1,$5.reg_no,reg2);
					emit(NOLABEL,ADD,reg2,$3.reg_no,reg3);
					emit(NOLABEL,MULT,reg3,reg,reg4);
					emit(NOLABEL,LOADI,lookup($1.str)->offset,reg5,EMPTY);
					emit(NOLABEL,ADD,reg4,reg5,reg6);
					emit(NOLABEL,ADD,0,reg6,reg7);
					}

					$$.str=$1.str;
					$$.reg_no=reg7;}
        ;

exp	: exp '+' exp		{ int reg;
					
				if(optimize_flag==1)
				{ 
					reg=checkInOPTable(ADD,$1.reg_no,$3.reg_no," ");
					if(reg>=getCurrent())
					{
						emit(NOLABEL,ADD,$1.reg_no,$3.reg_no,reg);
					}
				}

				else if(optimize_flag==0)
				{
					reg=NextRegister();
					emit(NOLABEL,ADD,$1.reg_no,$3.reg_no,reg); 
				} 
				$$.reg_no=reg;	
				} 

        | exp '-' exp		{ int reg;
					
				if(optimize_flag==1)
				{ 
					reg=checkInOPTable(SUB,$1.reg_no,$3.reg_no," ");
					if(reg>=getCurrent())
					{
						emit(NOLABEL,SUB,$1.reg_no,$3.reg_no,reg);
					}
				}

				else if(optimize_flag==0)
				{
					reg=NextRegister();
					emit(NOLABEL,SUB,$1.reg_no,$3.reg_no,reg); 
				} 
				$$.reg_no=reg;
				} 

	| exp '*' exp		{ int reg;
					
				if(optimize_flag==1)
				{ 
					reg=checkInOPTable(MULT,$1.reg_no,$3.reg_no," ");
					if(reg>=getCurrent())
					{
						emit(NOLABEL,MULT,$1.reg_no,$3.reg_no,reg); 
					}
				}

				else if(optimize_flag==0)
				{
					reg=NextRegister();
					emit(NOLABEL,MULT,$1.reg_no,$3.reg_no,reg); 
				}  
				$$.reg_no=reg;
				}

        | ID			{ int reg;
					
				if(optimize_flag==1 && for_flag!=1)
				{ 
					reg=checkInOPTable(LOADAI,0,lookup($1.str)->offset,$1.str);
					if(reg>=getCurrent())
					{
						emit(NOLABEL,LOADAI,0,lookup($1.str)->offset,reg);
						if(store_flag==1)
						{
							opdelete($1.str);
							store_flag=0;
						}
					}
				}

				else if(optimize_flag==0 || for_flag==1)
				{
					reg=NextRegister();
					emit(NOLABEL,LOADAI,0,lookup($1.str)->offset,reg); 
				}  
				$$.reg_no=reg;} 

        | ID '[' exp ']'	{ int reg,reg1,reg2,reg3,reg4; 
					if(optimize_flag==1)
					{ 
						reg=checkInOPTable(LOADI,4,EMPTY," ");
						if(reg>=getCurrent())
						{
							emit(NOLABEL,LOADI,4,reg,EMPTY); 	
						}
						reg1=checkInOPTable(MULT,$3.reg_no,reg," ");
						if(reg1>=getCurrent())
						{
							emit(NOLABEL,MULT,$3.reg_no,reg,reg1);
						}
						reg2=checkInOPTable(LOADI,lookup($1.str)->offset,EMPTY," ");
						if(reg2>=getCurrent())
						{
							emit(NOLABEL,LOADI,lookup($1.str)->offset,reg2,EMPTY);
						}
						reg3=checkInOPTable(ADD,reg1,reg2," ");
						if(reg3>=getCurrent())
						{
							emit(NOLABEL,ADD,reg1,reg2,reg3);		
						}
						reg4=checkInOPTable(LOADAO,0,reg3,$1.str);
						if(reg4>=getCurrent())
						{
							emit(NOLABEL,LOADAO,0,reg3,reg4);
						}
					}

					else if(optimize_flag==0)
					{
						reg=NextRegister();
						reg1=NextRegister();
						reg2=NextRegister();
						reg3=NextRegister();
						reg4=NextRegister();
					emit(NOLABEL,LOADI,4,reg,EMPTY); 
					emit(NOLABEL,MULT,$3.reg_no,reg,reg1);
					emit(NOLABEL,LOADI,lookup($1.str)->offset,reg2,EMPTY);
					emit(NOLABEL,ADD,reg1,reg2,reg3);
					emit(NOLABEL,LOADAO,0,reg3,reg4);
					} 

					$$.reg_no=reg4;} 

        | ID '[' exp ',' exp ']' { int reg,reg1,reg2,reg3,reg4,reg5,reg6,reg7;

					if(optimize_flag==1)
					{ 
						reg=checkInOPTable(LOADI,4,EMPTY," ");
						if(reg>=getCurrent())
						{
							emit(NOLABEL,LOADI,4,reg,EMPTY);
						}
						reg1=checkInOPTable(LOADI,lookup($1.str)->rows,EMPTY," ");
						if(reg1>=getCurrent())
						{
												emit(NOLABEL,LOADI,lookup($1.str)->rows,reg1,EMPTY);
						}
						reg2=checkInOPTable(MULT,reg1,$5.reg_no," ");
						if(reg2>=getCurrent())
						{
							emit(NOLABEL,MULT,reg1,$5.reg_no,reg2);
						}
						reg3=checkInOPTable(ADD,reg2,$3.reg_no," ");
						if(reg3>=getCurrent())
						{					emit(NOLABEL,ADD,reg2,$3.reg_no,reg3);
							
						}
						reg4=checkInOPTable(MULT,reg3,reg," ");
						if(reg4>=getCurrent())
						{
												emit(NOLABEL,MULT,reg3,reg,reg4);
						}
						reg5=checkInOPTable(LOADI,lookup($1.str)->offset,EMPTY," ");
						if(reg5>=getCurrent())
						{
										emit(NOLABEL,LOADI,lookup($1.str)->offset,reg5,EMPTY);		
						}
						reg6=checkInOPTable(ADD,reg4,reg5," ");
						if(reg6>=getCurrent())
						{
										emit(NOLABEL,ADD,reg4,reg5,reg6);		
						}
						reg7=checkInOPTable(LOADAO,0,reg6,$1.str);
						if(reg7>=getCurrent())
						{
										emit(NOLABEL,LOADAO,0,reg6,reg7);		
						}
					}

					else if(optimize_flag==0)
					{
						reg=NextRegister();
						reg1=NextRegister();
						reg2=NextRegister();
						reg3=NextRegister();
						reg4=NextRegister();
						reg5=NextRegister();
						reg6=NextRegister();
						reg7=NextRegister();
					emit(NOLABEL,LOADI,4,reg,EMPTY);
					emit(NOLABEL,LOADI,lookup($1.str)->rows,reg1,EMPTY);
			emit(NOLABEL,MULT,reg1,$5.reg_no,reg2);
					emit(NOLABEL,ADD,reg2,$3.reg_no,reg3);
					emit(NOLABEL,MULT,reg3,reg,reg4);
					emit(NOLABEL,LOADI,lookup($1.str)->offset,reg5,EMPTY);
					emit(NOLABEL,ADD,reg4,reg5,reg6);
					emit(NOLABEL,LOADAO,0,reg6,reg7);
					} 

					$$.reg_no=reg7;}
	| ICONST                 { int reg;
					
				if(optimize_flag==1)
				{ 
					reg=checkInOPTable(LOADI,$1.num,EMPTY," ");
					if(reg>=getCurrent())
					{
						emit(NOLABEL,LOADI,$1.num,reg,EMPTY);
					}
				}

				else if(optimize_flag==0)
				{
					reg=NextRegister();
					emit(NOLABEL,LOADI,$1.num,reg,EMPTY);
				}  
				 $$.reg_no=reg;}

	| error { yyerror("***Error: illegal expression\n");}  
	;


ctrlexp	: ID ASG ICONST ',' ICONST { int reg,reg1,reg2,reg3,reg4,reg5; 
					if(optimize_flag==1)
					{ 
						reg=checkInOPTable(LOADI,lookup($1.str)->offset,EMPTY," ");
						if(reg>=getCurrent())
						{
												 emit(NOLABEL,LOADI,lookup($1.str)->offset,reg,EMPTY);
						}
						reg1=checkInOPTable(ADD,0,reg," ");
						if(reg1>=getCurrent())
						{
												 emit(NOLABEL,ADD,0,reg,reg1);
						}
						reg2=checkInOPTable(LOADI,$3.num,EMPTY," ");
						if(reg2>=getCurrent())
						{
							
					 emit(NOLABEL,LOADI,$3.num,reg2,EMPTY);
	
						}
						reg3=checkInOPTable(LOADI,$5.num,EMPTY," ");
						if(reg3>=getCurrent())
						{
										 emit(NOLABEL,LOADI,$5.num,reg3,EMPTY);	
						}
						
						
					}

					else if(optimize_flag==0)
					{
						reg=NextRegister();
						reg1=NextRegister();
						reg2=NextRegister();
						reg3=NextRegister();
						
						reg5=NextRegister();
					 emit(NOLABEL,LOADI,lookup($1.str)->offset,reg,EMPTY);
					 emit(NOLABEL,ADD,0,reg,reg1);

					 emit(NOLABEL,LOADI,$3.num,reg2,EMPTY);
					 emit(NOLABEL,LOADI,$5.num,reg3,EMPTY);
					} 

					reg4=NextRegister();
					
					emit(NOLABEL,STORE,reg2,reg1,EMPTY);
	

				        int label=NextLabel(); emit(label,LOADAI,0,lookup($1.str)->offset,reg4);
									if(optimize_flag==1)
									{
										reg5=checkInOPTable(CMPLE,reg4,reg3," ");
										if(reg5>=getCurrent())
										{
											emit(NOLABEL,CMPLE,reg4,reg3,reg5);
										}
									}
									else if(optimize_flag==0)
									{
										emit(NOLABEL,CMPLE,reg4,reg3,reg5);
									}
									
									int label1=NextLabel(); int label2=NextLabel(); emit(NOLABEL,CBR,reg5,label1,label2); $$.init_label=label;
									$$.true_label=label1; $$.false_label=label2;
									emit(label1,NOP,EMPTY,EMPTY,EMPTY);
									$$.counter_offset=lookup($1.str)->offset;
									$$.var=$1.str;
									}
        ;


condexp	: exp NEQ exp		{ int reg;
					
				if(optimize_flag==1)
				{ 
					reg=checkInOPTable(CMPNE,$1.reg_no,$3.reg_no," ");
					if(reg>=getCurrent())
					{
						emit(NOLABEL,CMPNE,$1.reg_no,$3.reg_no,reg);
					}
				}

				else if(optimize_flag==0)
				{
					reg=NextRegister();
				emit(NOLABEL,CMPNE,$1.reg_no,$3.reg_no,reg);
				}  
				 $$.reg_no=reg;}

	| exp EQ exp		{ int reg;
					
				if(optimize_flag==1)
				{ 
					reg=checkInOPTable(CMPEQ,$1.reg_no,$3.reg_no," ");
					if(reg>=getCurrent())
					{
						emit(NOLABEL,CMPEQ,$1.reg_no,$3.reg_no,reg);
					}
				}

				else if(optimize_flag==0)
				{
					reg=NextRegister();
					emit(NOLABEL,CMPEQ,$1.reg_no,$3.reg_no,reg);
				}  
				  $$.reg_no=reg;}

	| exp LT exp		{ int reg;
					
				if(optimize_flag==1)
				{ 
					reg=checkInOPTable(CMPLT,$1.reg_no,$3.reg_no," ");
					if(reg>=getCurrent())
					{
					 emit(NOLABEL,CMPLT,$1.reg_no,$3.reg_no,reg);	
					}
				}

				else if(optimize_flag==0)
				{
					reg=NextRegister(); emit(NOLABEL,CMPLT,$1.reg_no,$3.reg_no,reg);
				}  
				 $$.reg_no=reg;}

	| exp LEQ exp		{ int reg;
					
				if(optimize_flag==1)
				{ 
					reg=checkInOPTable(CMPLE,$1.reg_no,$3.reg_no," ");
					if(reg>=getCurrent())
					{
						emit(NOLABEL,CMPLE,$1.reg_no,$3.reg_no,reg); 
					}
				}

				else if(optimize_flag==0)
				{
					reg=NextRegister();emit(NOLABEL,CMPLE,$1.reg_no,$3.reg_no,reg); 
				}  
				 $$.reg_no=reg;}

	| exp GT exp		{ int reg;
					
				if(optimize_flag==1)
				{ 
					reg=checkInOPTable(CMPGT,$1.reg_no,$3.reg_no," ");
					if(reg>=getCurrent())
					{
						 emit(NOLABEL,CMPGT,$1.reg_no,$3.reg_no,reg); 
					}
				}

				else if(optimize_flag==0)
				{
					reg=NextRegister(); emit(NOLABEL,CMPGT,$1.reg_no,$3.reg_no,reg); 
				}  
				$$.reg_no=reg;}

	| exp GEQ exp		{ int reg;
					
				if(optimize_flag==1)
				{ 
					reg=checkInOPTable(CMPGE,$1.reg_no,$3.reg_no," ");
					if(reg>=getCurrent())
					{
						 emit(NOLABEL,CMPGE,$1.reg_no,$3.reg_no,reg);
					}
				}

				else if(optimize_flag==0)
				{
					reg=NextRegister(); emit(NOLABEL,CMPGE,$1.reg_no,$3.reg_no,reg);
				}  
				 $$.reg_no=reg;}

	| error { yyerror("***Error: illegal conditional expression\n");}  
        ;

%%

void yyerror(char* s) {
        fprintf(stderr,"%s\n",s);
        }

int optimize_flag = 0;


int
main(int argc, char* argv[]) {

  printf("\n     CS515 Spring 2016 Compiler\n");

  if (argc == 2 && strcmp(argv[1],"-O") == 0) {
    optimize_flag = 1;
    printf("     Version 1.0 CSE OPTIMIZER \n\n");
  }
    else
    printf("    Version 1.0 (non-optimizing)\n\n");
  
  outfile = fopen("iloc.out", "w");
  if (outfile == NULL) { 
    printf("ERROR: cannot open output file \"iloc.out\".\n");
    return -1;
  }

  CommentBuffer = (char *) malloc(500);  
  InitSymbolTable();
  InitOptimTable();

  printf("1\t");
  yyparse();
  printf("\n");

  PrintSymbolTable();

  if(optimize_flag==1)
  {
	PrintOptimTable();
  }

  /* ** START: THIS IS BOGUS AND NEEDS TO BE REMOVED **    
  emitComment("This is bogus code and just meant to demonstrate the use of \"emit\" ");
  emit(1, NOP, EMPTY, EMPTY, EMPTY);
  emit(NOLABEL, LOADI, 12, 1, EMPTY);
  emit(NOLABEL, LOADI, 1028, 2, EMPTY);
  emit(NOLABEL, STORE, 1, 2, EMPTY);
  emit(NOLABEL, OUTPUT, 1028, EMPTY, EMPTY);
  emit(NOLABEL, LOADI, -5, 3, EMPTY);
  emit(NOLABEL, CMPLT, 1, 3, 4);
  emit(NOLABEL, STORE, 4, 2, EMPTY);
  emit(NOLABEL, OUTPUT, 1028, EMPTY, EMPTY);
  emit(NOLABEL, CBR, 4, 1, 2);
  emit(2, NOP, EMPTY, EMPTY, EMPTY);

  ** END: THIS IS BOGUS AND NEEDS TO BE REMOVED ** */    
  
  fclose(outfile);
  
  return 1;
}




