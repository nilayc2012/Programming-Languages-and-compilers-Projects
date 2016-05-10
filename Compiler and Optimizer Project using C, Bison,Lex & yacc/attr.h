/**********************************************
        CS515  Compiler  
        Project 1
        Spring 2016
**********************************************/
#include "instrutil.h"
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#ifndef ATTR_H
#define ATTR_H

typedef union {int num; char *str;} tokentype;

typedef struct {
  int reg_no;
  char * str;
} RegInfo;

typedef struct {
  int true_label;
  int false_label;
} LabelInfo;

typedef struct {
  int rows;
  int cols;
  int type;
} TypeData;

typedef struct {
	int init_label;
  int true_label;
  int false_label;
  int counter_offset;
  char * var; 
} ForLabelInfo;

typedef struct {
  char * variables[30];
} VariableList;

extern char* integerToString(int val);

typedef struct {
  char *name;
  int reg_no;
  char* var;
} OptimTabEntry;

extern
void InitOptimTable();

extern
OptimTabEntry * oplookup(char *name);

extern
void opinsert(char *name,int reg_no,char * var);

extern
void opdelete(char * var);

extern int checkInOPTable(
	  Opcode_Name opcode, 
	  int field1, 
	  int field2,char * field3);

extern
void PrintOptimTable();

extern int getCurrent();
#endif

