%{
	#include <stdio.h>
	#include <ctype.h>
	int yylex(void);
	int yyerror(char *s);
%}

%token VARIABLE
%token CONSTANT
%token T_LAMBDA
%token LPARENS
%token RPARENS
%token END
%token E


%start final_exp

%% /*LAMBDA EXPRESSION RULES*/

final_exp: 	expression
			|final_exp expression
;						
expression: VARIABLE
			{
					printf("Parse for syntactically correct lambda-calculus expression was successful:");
					printf("\n");
					printf("The overall expression is a variable\n\n");								
			}
			|
			CONSTANT
			{
					printf("Parse for syntactically correct lambda-calculus expression was successful:");
					printf("\n");
					printf("The overall expression is a constant\n\n");	 	
			}
			|
			 LPARENS expression expression RPARENS
			{
					printf("Parse for syntactically correct lambda-calculus expression was successful:");
					printf("\n");
					printf("The overall expression is a combination\n\n");
			}
			|
			T_LAMBDA expression RPARENS
			{
					printf("Parse for syntactically correct lambda-calculus expression was successful:");
					printf("\n");
					printf("The overall expression is an abstraction\n\n");			
			}
			|
			E
			{	
				
			    	printf("Sorry, Charlie: Not everybody can be a lambda expression!\n\n");
			}
;
 
%%
