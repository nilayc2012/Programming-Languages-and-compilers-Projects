/**********************************************
        CS515  Compiler 
        Project 1
        Spring 2016
**********************************************/

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "instrutil.h"
#include "attr.h" 
#define HASH_TABLE_SIZE 347

char* integerToString(int val)
{
	char* stringval=(char*) malloc(sizeof(char*));
	sprintf(stringval, "%d" ,val);
return stringval;
}

static 
OptimTabEntry **OPTable;

static int current=-1;

static
int hash(char *name) {
  int i;
  int hashValue = 1;
  
  for (i=0; i < strlen(name); i++) {
    hashValue = (hashValue * name[i]) % HASH_TABLE_SIZE;
  }

  return hashValue;
}


void
InitOptimTable() {
  int i;
  int dummy;

  OPTable = (OptimTabEntry **) malloc (sizeof(OptimTabEntry *) * HASH_TABLE_SIZE);
  for (i=0; i < HASH_TABLE_SIZE; i++)
    OPTable[i] = NULL;
}


/* Returns pointer to symbol table entry, if entry is found */
/* Otherwise, NULL is returned */
OptimTabEntry * 
oplookup(char *name) {

  int currentIndex;
  int visitedSlots = 0;
  
  currentIndex = hash(name);
  while (OPTable[currentIndex] != NULL && visitedSlots < HASH_TABLE_SIZE) {
    if (!strcmp(OPTable[currentIndex]->name, name) )
      return OPTable[currentIndex];
    currentIndex = (currentIndex + 1) % HASH_TABLE_SIZE; 
    visitedSlots++;
  }
  return NULL;
}


void 
opinsert(char *name,int reg_no,char* var) {
  int currentIndex;
  int visitedSlots = 0;

  currentIndex = hash(name);
  while (OPTable[currentIndex] != NULL && visitedSlots < HASH_TABLE_SIZE) {
    if (!strcmp(OPTable[currentIndex]->name, name) ) 
      printf("*** WARNING *** in function \"insert\": %s has already an entry\n", name);
    currentIndex = (currentIndex + 1) % HASH_TABLE_SIZE; 
    visitedSlots++;
  }
  if (visitedSlots == HASH_TABLE_SIZE) {
    printf("*** ERROR *** in function \"insert\": No more space for entry %s\n", name);
    return;
  }
  
  OPTable[currentIndex] = (OptimTabEntry *) malloc (sizeof(OptimTabEntry));
  OPTable[currentIndex]->name = (char *) malloc (strlen(name)+1);
  strcpy(OPTable[currentIndex]->name, name);
  OPTable[currentIndex]->reg_no= reg_no;
  OPTable[currentIndex]->var = (char *) malloc (strlen(var)+1);
  strcpy(OPTable[currentIndex]->var, var);
}

void opdelete(char * var)
{int i,j;

  for (i=0; i < HASH_TABLE_SIZE; i++) {

    if (OPTable[i] != NULL && strcmp(OPTable[i]->var,var)==0) {
	OPTable[i]=NULL;
	}
   }
}
void 
PrintOptimTable() {
  int i;
  
  printf("\n --- Virtual Register Table ---------------\n\n");
  for (i=0; i < HASH_TABLE_SIZE; i++) {
    if (OPTable[i] != NULL) {
      printf("\"%s\" \t \"r%d\" \n", OPTable[i]->name,OPTable[i]->reg_no); 
    }
  }
  printf("\n --------------------------------\n\n");
}

int
checkInOPTable(Opcode_Name opcode, 
	  int field1, 
	  int field2, char* field3) 
{

char stringval[40]; 
  switch (opcode) {

  case ADDI:

					strcpy(stringval,"ADDI r");
					strcat(stringval,integerToString(field1));
					strcat(stringval,",");
					strcat(stringval,integerToString(field2));
    break;
  case ADD:
    strcpy(stringval,"ADD r");
					strcat(stringval,integerToString(field1));
					strcat(stringval,",r");
					strcat(stringval,integerToString(field2));
    break;
  case SUBI:
strcpy(stringval,"SUBI r");
					strcat(stringval,integerToString(field1));
					strcat(stringval,",");
					strcat(stringval,integerToString(field2));
    break;
  case SUB: 
strcpy(stringval,"SUB r");
					strcat(stringval,integerToString(field1));
					strcat(stringval,",r");
					strcat(stringval,integerToString(field2));
    break;
  case MULT: 
strcpy(stringval,"MULT r");
					strcat(stringval,integerToString(field1));
					strcat(stringval,",r");
					strcat(stringval,integerToString(field2));
    break;
  case LOAD: 
strcpy(stringval,"LOAD r");
					strcat(stringval,integerToString(field1));
    break;
  case LOADI: 
    strcpy(stringval,"LOADI ");
					strcat(stringval,integerToString(field1));
    break;
  case LOADAI: 
strcpy(stringval,"LOADAI r");
					strcat(stringval,integerToString(field1));
					strcat(stringval,",");
					strcat(stringval,integerToString(field2));
    break;
  case LOADAO: 
strcpy(stringval,"LOADAO r");
					strcat(stringval,integerToString(field1));
					strcat(stringval,",r");
					strcat(stringval,integerToString(field2));
    break;
  case CMPLT: 
strcpy(stringval,"CMPLT r");
					strcat(stringval,integerToString(field1));
					strcat(stringval,",r");
					strcat(stringval,integerToString(field2));
    break;
  case CMPLE: 
strcpy(stringval,"CMPLE r");
					strcat(stringval,integerToString(field1));
					strcat(stringval,",r");
					strcat(stringval,integerToString(field2));
    break;
  case CMPGT: 
strcpy(stringval,"CMPGT r");
					strcat(stringval,integerToString(field1));
					strcat(stringval,",r");
					strcat(stringval,integerToString(field2));
    break;
  case CMPGE: 
strcpy(stringval,"CMPGE r");
					strcat(stringval,integerToString(field1));
					strcat(stringval,",r");
					strcat(stringval,integerToString(field2));
    break;
  case CMPEQ: 
strcpy(stringval,"CMPEQ r");
					strcat(stringval,integerToString(field1));
					strcat(stringval,",r");
					strcat(stringval,integerToString(field2));
    break;
  case CMPNE: 
strcpy(stringval,"CMPNE r");
					strcat(stringval,integerToString(field1));
					strcat(stringval,",r");
					strcat(stringval,integerToString(field2));
    break;
  default:
    printf("Illegal instruction in \"emit\" \n");
  }


if(oplookup(stringval)!=NULL)
{
	return oplookup(stringval)->reg_no;
}
else
{	current=NextRegister();
	opinsert(stringval,current,field3);
	return oplookup(stringval)->reg_no;
}
}

int getCurrent()
{
return current;
}


